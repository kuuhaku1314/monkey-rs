use crate::ast::Program;
use crate::error::{CallFrame, Error, ErrorKind};
use crate::lexer::{Position, Span};
use crate::map_key::AbleToMapKey;
use crate::token::Token;
use compiler::Compiler;
use env::{
    capture_vm_upvalues, define_vm_env, define_vm_slot_binding, has_current_vm_binding,
    inject_vm_module, mark_env_chain_captured, mark_vm_export, sync_locals_to_env, VmEnv,
    VmEnvHandle,
};
use frame::{
    can_reuse_closure_env_for_call, collect_builtin_args_from_registers, compare_frame_local_int,
    compare_frame_local_int_bool, compare_frame_local_local_bool, eval_local_int_infix_from_values,
    eval_register_int_int_infix, frame_locals, get_frame_local, get_register,
    leave_register_for_in_iteration, leave_register_scope_depth,
    prepare_register_call_frame_from_registers, read_compiled_register,
    refresh_captured_frame_locals, refresh_frame_locals_from_env, replace_frame_locals,
    reset_frame_locals, set_frame_local, set_optional_register_or_empty, set_register,
    set_register_last, sync_frame_locals_to_env_if_dirty, sync_frame_locals_to_env_if_needed,
    update_frame_local_from_local, update_frame_local_from_local_no_result, update_frame_local_int,
    update_frame_local_int_no_result, vm_register_int_value, ForInState, VmFrame,
};
use heap::VmHeap;
use program::{BuiltinOp, CompiledFunction};
use register::{RegisterConstant, RegisterInstruction};
use runtime::ops::{
    bool_condition, eval_index, eval_infix_value, eval_member_value_with_key, eval_prefix_value,
    make_struct_named, make_struct_positional, operation_type_error, pop_struct_type, set_index,
    set_member,
};
use runtime::value::{Value, VmFunction, VmStructType};
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;
use std::time::{Duration, Instant};
use symbols::{SymbolId, SymbolInterner};

mod builtin_catalog;
mod builtins;
mod calls;
mod compiler;
mod dump;
mod env;
mod executor;
mod frame;
mod heap;
mod hir;
mod imports;
mod names;
mod profile;
mod program;
mod register;
mod runtime;
mod semantic;
mod slots;
mod symbols;
pub mod value;
use slots::{get_vm_slot, set_vm_slot};

pub(crate) use builtin_catalog::{builtin_names, global_builtin_names};
pub use profile::VmProfile;
pub use value::VmValue;

pub struct VmBenchmark {
    pub compile_elapsed: Duration,
    pub execute_elapsed: Duration,
    pub profile_execute_elapsed: Duration,
    pub value: VmValue,
    pub profile: VmProfile,
}

struct Vm {
    interner: Rc<RefCell<SymbolInterner>>,
    module_cache: HashMap<PathBuf, Value>,
    import_stack: Vec<PathBuf>,
    source_stack: Vec<Option<String>>,
    call_stack: Vec<CallFrame>,
    get_name_cache: HashMap<(usize, usize), usize>,
    set_name_cache: HashMap<(usize, usize), usize>,
    member_key_cache: HashMap<(usize, usize), AbleToMapKey>,
    heap: VmHeap,
    profile: Option<VmProfile>,
    profile_instruction_start: Option<(&'static str, Instant)>,
}

enum VmFlow {
    Value(Value),
    Return(Value),
    Break(Span),
    Continue(Span),
}

pub fn eval_vm(program: &Program) -> Result<VmValue, Error> {
    let mut vm = Vm::new();
    let function = Compiler::compile_program(program, Rc::clone(&vm.interner))?;
    vm.source_stack.push(program.path.clone());
    let env = VmEnv::alloc_root(&mut vm.heap, function.local_count);
    let flow = vm.execute_register_function(function, env);
    vm.source_stack.pop();
    let flow = flow?;
    match flow {
        VmFlow::Value(value) | VmFlow::Return(value) => Ok(VmValue::new(value, vm.heap)),
        VmFlow::Break(span) => Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "break outside loop".to_string(),
            span,
        )),
        VmFlow::Continue(span) => Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "continue outside loop".to_string(),
            span,
        )),
    }
}

pub fn eval_vm_profile(program: &Program) -> Result<(VmValue, VmProfile), Error> {
    let mut vm = Vm::new_profiled();
    let function = Compiler::compile_program(program, Rc::clone(&vm.interner))?;
    vm.source_stack.push(program.path.clone());
    let env = VmEnv::alloc_root(&mut vm.heap, function.local_count);
    let flow = vm.execute_register_function(function, env);
    vm.source_stack.pop();
    let flow = flow?;
    let value = match flow {
        VmFlow::Value(value) | VmFlow::Return(value) => value,
        VmFlow::Break(span) => {
            return Err(Error::with_kind_span(
                ErrorKind::Runtime,
                "break outside loop".to_string(),
                span,
            ))
        }
        VmFlow::Continue(span) => {
            return Err(Error::with_kind_span(
                ErrorKind::Runtime,
                "continue outside loop".to_string(),
                span,
            ))
        }
    };
    if let Some(profile) = &mut vm.profile {
        profile.heap_allocated = vm.heap.allocated();
    }
    let profile = vm.profile.take().unwrap_or_default();
    Ok((VmValue::new(value, vm.heap), profile))
}

pub fn benchmark_vm(program: &Program) -> Result<VmBenchmark, Error> {
    let mut vm = Vm::new();
    let compile_start = Instant::now();
    let function = Compiler::compile_program(program, Rc::clone(&vm.interner))?;
    let compile_elapsed = compile_start.elapsed();

    vm.source_stack.push(program.path.clone());
    let env = VmEnv::alloc_root(&mut vm.heap, function.local_count);
    let execute_start = Instant::now();
    let flow = vm.execute_register_function(function, env);
    let execute_elapsed = execute_start.elapsed();
    vm.source_stack.pop();
    let value = finish_top_level_flow(flow?)?;

    let mut profiled_vm = Vm::new_profiled();
    let profiled_function = Compiler::compile_program(program, Rc::clone(&profiled_vm.interner))?;
    profiled_vm.source_stack.push(program.path.clone());
    let profiled_env = VmEnv::alloc_root(&mut profiled_vm.heap, profiled_function.local_count);
    let profile_execute_start = Instant::now();
    let profiled_flow = profiled_vm.execute_register_function(profiled_function, profiled_env);
    let profile_execute_elapsed = profile_execute_start.elapsed();
    profiled_vm.source_stack.pop();
    finish_top_level_flow(profiled_flow?)?;
    if let Some(profile) = &mut profiled_vm.profile {
        profile.heap_allocated = profiled_vm.heap.allocated();
    }
    let profile = profiled_vm.profile.take().unwrap_or_default();

    Ok(VmBenchmark {
        compile_elapsed,
        execute_elapsed,
        profile_execute_elapsed,
        value: VmValue::new(value, vm.heap),
        profile,
    })
}

fn finish_top_level_flow(flow: VmFlow) -> Result<Value, Error> {
    match flow {
        VmFlow::Value(value) | VmFlow::Return(value) => Ok(value),
        VmFlow::Break(span) => Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "break outside loop".to_string(),
            span,
        )),
        VmFlow::Continue(span) => Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "continue outside loop".to_string(),
            span,
        )),
    }
}

pub fn dump_vm_ir(program: &Program) -> Result<String, Error> {
    let interner = Rc::new(RefCell::new(SymbolInterner::default()));
    let (_, ir) = Compiler::compile_program_with_ir(program, interner)?;
    Ok(dump::dump_hir(&ir))
}

pub fn dump_vm_bytecode(program: &Program) -> Result<String, Error> {
    let interner = Rc::new(RefCell::new(SymbolInterner::default()));
    let (function, _) = Compiler::compile_program_with_ir(program, interner)?;
    Ok(dump::dump_bytecode(&function))
}

impl Vm {
    fn new() -> Self {
        Self::with_profile(None)
    }

    fn new_profiled() -> Self {
        Self::with_profile(Some(VmProfile::default()))
    }

    fn with_profile(profile: Option<VmProfile>) -> Self {
        Self {
            interner: Rc::new(RefCell::new(SymbolInterner::default())),
            module_cache: HashMap::new(),
            import_stack: Vec::new(),
            source_stack: Vec::new(),
            call_stack: Vec::new(),
            get_name_cache: HashMap::new(),
            set_name_cache: HashMap::new(),
            member_key_cache: HashMap::new(),
            heap: VmHeap::default(),
            profile,
            profile_instruction_start: None,
        }
    }

    fn symbol_name(&self, id: SymbolId) -> String {
        self.interner.borrow().name(id).to_string()
    }

    fn record_instruction(
        &mut self,
        kind: &'static str,
        frame_depth: usize,
        register_count: usize,
    ) {
        if let Some(profile) = &mut self.profile {
            profile.total_instructions += 1;
            *profile.instruction_counts.entry(kind).or_insert(0) += 1;
            profile.max_frame_depth = profile.max_frame_depth.max(frame_depth);
            profile.max_register_count = profile.max_register_count.max(register_count);
            self.profile_instruction_start = Some((kind, Instant::now()));
        }
    }

    fn record_builtin(&mut self, name: &str) {
        if let Some(profile) = &mut self.profile {
            *profile.builtin_counts.entry(name.to_string()).or_insert(0) += 1;
        }
    }

    fn record_builtin_elapsed(&mut self, name: &str, elapsed: Duration) {
        if let Some(profile) = &mut self.profile {
            *profile.builtin_elapsed.entry(name.to_string()).or_default() += elapsed;
        }
    }

    fn record_allocation(&mut self, kind: &str) {
        if let Some(profile) = &mut self.profile {
            *profile
                .allocation_counts
                .entry(kind.to_string())
                .or_insert(0) += 1;
        }
    }

    fn record_import(&mut self, cache_hit: bool) {
        if let Some(profile) = &mut self.profile {
            profile.import_count += 1;
            if cache_hit {
                profile.import_cache_hits += 1;
            }
        }
    }

    fn record_call(&mut self) {
        if let Some(profile) = &mut self.profile {
            profile.call_count += 1;
        }
    }

    fn finish_profile_instruction(&mut self) {
        let Some((kind, started)) = self.profile_instruction_start.take() else {
            return;
        };
        if let Some(profile) = &mut self.profile {
            *profile.instruction_elapsed.entry(kind).or_default() += started.elapsed();
        }
    }
}

fn user_value(value: Value) -> Value {
    match value {
        Value::Empty => Value::Null,
        value => value,
    }
}

fn register_constant_to_value(value: RegisterConstant) -> Value {
    match value {
        RegisterConstant::Int(value) => Value::Integer(value),
        RegisterConstant::Float(value) => Value::Float(value),
        RegisterConstant::Bool(value) => Value::Boolean(value),
        RegisterConstant::Null => Value::Null,
        RegisterConstant::Empty => Value::Empty,
    }
}

fn snapshot_for_in_entries(
    value: Value,
    heap: &mut VmHeap,
    span: Span,
) -> Result<Vec<(Value, Option<Value>)>, Error> {
    match value {
        Value::Map(map) => {
            let entries = heap
                .map(map)
                .iter()
                .map(|(key, value)| (key.clone(), value.clone()))
                .collect::<Vec<_>>();
            Ok(entries
                .into_iter()
                .map(|(key, value)| (value_from_map_key(&key, heap), Some(value)))
                .collect())
        }
        Value::Slice(slice) => Ok(heap
            .list(slice)
            .iter()
            .cloned()
            .enumerate()
            .map(|(index, value)| (Value::Integer(index as i64), Some(value)))
            .collect()),
        _ => Err(operation_type_error(span)),
    }
}

fn value_from_map_key(key: &AbleToMapKey, heap: &mut VmHeap) -> Value {
    match key {
        AbleToMapKey::Integer(value) => Value::Integer(*value),
        AbleToMapKey::Boolean(value) => Value::Boolean(*value),
        AbleToMapKey::String(value) => heap.alloc_string(value.to_owned()),
    }
}

fn vm_map_key_from_value(value: Value, heap: &VmHeap, span: Span) -> Result<AbleToMapKey, Error> {
    match value {
        Value::Integer(value) => Ok(AbleToMapKey::Integer(value)),
        Value::Boolean(value) => Ok(AbleToMapKey::Boolean(value)),
        Value::String(value) => Ok(AbleToMapKey::String(heap.string(value).to_string())),
        _ => Err(Error::with_kind_span(
            ErrorKind::Type,
            "unable as map key".to_string(),
            span,
        )),
    }
}

fn eval_vm_slot_infix(
    heap: &mut VmHeap,
    env: VmEnvHandle,
    depth: usize,
    slot: usize,
    token: &Token,
    right: Value,
    span: Span,
) -> Result<Value, Error> {
    let left = get_vm_slot(heap, env, depth, slot, span)?;
    if let (Some(left), Some(right)) = (vm_register_int_value(&left), vm_register_int_value(&right))
    {
        return eval_register_int_int_infix(left, token, right, span);
    }
    eval_infix_value(heap, token, left, right, span)
}

fn zero_span() -> Span {
    Span::point(Position(0, 0))
}

#[cfg(test)]
mod tests {
    use super::{eval_vm, eval_vm_profile, VmValue};
    use crate::error::{Error, ErrorKind};
    use crate::lexer::new_lexer;
    use crate::parser::new_parser;
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn eval_input(source: &str) -> VmValue {
        eval_input_with_path(source, Some("vm_test.monkey".to_string()))
    }

    fn eval_input_with_path(source: &str, path: Option<String>) -> VmValue {
        let lexer = new_lexer(source.to_string(), path);
        let mut parser = new_parser(lexer);
        let program = parser.parse_program().unwrap();
        eval_vm(&program).unwrap()
    }

    fn eval_error_with_path(source: &str, path: Option<String>) -> Error {
        let lexer = new_lexer(source.to_string(), path);
        let mut parser = new_parser(lexer);
        let program = parser.parse_program().unwrap();
        match eval_vm(&program) {
            Ok(value) => panic!("expected error, got {value}"),
            Err(err) => err,
        }
    }

    fn profile_input(source: &str) -> (VmValue, super::VmProfile) {
        let lexer = new_lexer(source.to_string(), Some("vm_test.monkey".to_string()));
        let mut parser = new_parser(lexer);
        let program = parser.parse_program().unwrap();
        let (value, profile) = eval_vm_profile(&program).unwrap();
        (value, profile)
    }

    #[test]
    fn vm_evaluates_expressions_and_conditionals() {
        assert_eq!(
            eval_input(
                r#"
            let x = 1 + 2 * 3;
            if x > 5 {
                x + 1
            } else {
                0
            }
            "#,
            )
            .expect_integer(),
            8
        );
    }

    #[test]
    fn vm_matches_infix_and_prefix_language_semantics() {
        assert_eq!(
            eval_input(
                r#"
            let text = "id=" + 12 + ", score=" + 3.5;
            let comparisons = "b" > "a" && true > false && null == null;
            if comparisons {
                text
            } else {
                "bad"
            }
            "#,
            )
            .expect_string(),
            "id=12, score=3.5"
        );

        let err = eval_error_with_path("!1", Some("vm_test.monkey".to_string()));
        assert_eq!(err.kind, ErrorKind::Type);

        let err = eval_error_with_path("1 == 1.0", Some("vm_test.monkey".to_string()));
        assert_eq!(err.kind, ErrorKind::Type);
    }

    #[test]
    fn vm_self_update_optimization_preserves_side_effect_order() {
        assert_eq!(
            eval_input(
                r#"
            let x = 1;
            fn bump() {
                x = 10;
                1
            }
            x = x + bump();
            x
            "#,
            )
            .expect_integer(),
            2
        );
    }

    #[test]
    fn vm_discards_semicolon_terminated_expression_values() {
        assert!(
            eval_input(
                r#"
            1;
            2;
            "#,
            )
            .is_empty(),
            "expected empty"
        );
    }

    #[test]
    fn vm_index_assignment_errors_keep_assignment_span() {
        let err = eval_error_with_path(
            r#"
            let values = [1];
            values[5] = 2;
            "#,
            Some("vm_test.monkey".to_string()),
        );
        assert_eq!(err.kind, ErrorKind::Index);
        let span = err.span.expect("error should have a span");
        assert_eq!(span.start.0, 3);
        assert_eq!(span.start.1, 13);
    }

    #[test]
    fn vm_runtime_errors_include_call_stack() {
        let err = eval_error_with_path(
            r#"
            fn boom() {
                1 / 0
            }
            fn wrap() {
                boom()
            }
            wrap()
            "#,
            Some("vm_test.monkey".to_string()),
        );

        assert_eq!(err.kind, ErrorKind::Runtime);
        let names = err
            .stack
            .iter()
            .map(|frame| frame.name.as_str())
            .collect::<Vec<_>>();
        assert_eq!(names, vec!["wrap", "boom"]);
    }

    #[test]
    fn vm_builtin_errors_include_call_stack() {
        let err = eval_error_with_path(
            r#"
            fn show() {
                print({x: 1});
            }
            show()
            "#,
            Some("vm_test.monkey".to_string()),
        );

        assert_eq!(err.kind, ErrorKind::Type);
        let names = err
            .stack
            .iter()
            .map(|frame| frame.name.as_str())
            .collect::<Vec<_>>();
        assert_eq!(names, vec!["show", "print"]);
    }

    #[test]
    fn vm_runs_while_with_assignment_and_block_scope() {
        assert_eq!(
            eval_input(
                r#"
            let i = 0;
            let total = 0;
            while i < 5 {
                let local = i * 2;
                total = total + local;
                i = i + 1;
            }
            total
            "#,
            )
            .expect_integer(),
            20
        );
    }

    #[test]
    fn vm_optimizes_local_local_loop_conditions() {
        assert_eq!(
            eval_input(
                r#"
            let i = 0;
            let limit = 5;
            let total = 0;
            while i < limit {
                total = total + i;
                i = i + 1;
            }
            total
            "#,
            )
            .expect_integer(),
            10
        );
    }

    #[test]
    fn vm_reuses_dead_temporary_registers() {
        let (value, profile) = profile_input(
            r#"
            let value = (((1 + 2) + (3 + 4)) + ((5 + 6) + (7 + 8))) + ((9 + 10) + (11 + 12));
            value
            "#,
        );
        assert_eq!(value.expect_integer(), 78);
        assert!(
            profile.max_register_count <= 6,
            "dead temporaries should be recycled, got {} registers",
            profile.max_register_count
        );
    }

    #[test]
    fn vm_break_does_not_leak_while_block_scope() {
        let err = eval_error_with_path(
            r#"
            while true {
                let hidden = 1;
                break;
            }
            hidden
            "#,
            Some("vm_test.monkey".to_string()),
        );
        assert_eq!(err.kind, ErrorKind::Name);
    }

    #[test]
    fn vm_continue_does_not_leak_while_block_scope() {
        let err = eval_error_with_path(
            r#"
            let i = 0;
            while i < 1 {
                i = i + 1;
                let hidden = 1;
                continue;
            }
            hidden
            "#,
            Some("vm_test.monkey".to_string()),
        );
        assert_eq!(err.kind, ErrorKind::Name);
    }

    #[test]
    fn vm_omits_block_scope_only_when_block_defines_no_names() {
        assert_eq!(
            eval_input(
                r#"
            let x = 1;
            {
                x = x + 1;
            }
            x
            "#,
            )
            .expect_integer(),
            2
        );

        let err = eval_error_with_path(
            r#"
            {
                fn local() { 1 }
            }
            local()
            "#,
            Some("vm_test.monkey".to_string()),
        );
        assert_eq!(err.kind, ErrorKind::Name);
    }

    #[test]
    fn vm_runs_for_in_with_break_and_continue() {
        assert_eq!(
            eval_input(
                r#"
            let total = 0;
            for i, value in [1, 2, 3, 4] {
                if value == 2 {
                    continue;
                }
                if value == 4 {
                    break;
                }
                total = total + value;
            }
            total
            "#,
            )
            .expect_integer(),
            4
        );
    }

    #[test]
    fn vm_calls_functions_and_closures() {
        assert_eq!(
            eval_input(
                r#"
            fn make_adder(x) {
                fn add(y) {
                    x + y
                }
                add
            }
            let add_two = make_adder(2);
            add_two(40)
            "#,
            )
            .expect_integer(),
            42
        );
    }

    #[test]
    fn vm_closures_can_mutate_captured_bindings() {
        assert_eq!(
            eval_input(
                r#"
            fn make_counter() {
                let value = 0;
                fn next() {
                    value = value + 1;
                    value
                }
                next
            }
            let counter = make_counter();
            counter();
            counter();
            counter()
            "#,
            )
            .expect_integer(),
            3
        );
    }

    #[test]
    fn vm_runs_recursive_calls_on_vm_frames() {
        assert_eq!(
            eval_input(
                r#"
            fn sum_to(n, acc) {
                if n == 0 {
                    acc
                } else {
                    sum_to(n - 1, acc + n)
                }
            }
            sum_to(500, 0)
            "#,
            )
            .expect_integer(),
            125250
        );
    }

    #[test]
    fn vm_stores_functions_in_collections_and_records() {
        assert_eq!(
            eval_input(
                r#"
            fn inc(x) { x + 1 }
            fn double(x) { x * 2 }
            let funcs = [inc];
            append(funcs, double);
            let record = {f: funcs[0]};
            struct Box { f; }
            let boxed = Box{f: funcs[1]};
            record.f(10) + boxed.f(10)
            "#,
            )
            .expect_integer(),
            31
        );
    }

    #[test]
    fn vm_iterates_function_values_in_lists() {
        assert_eq!(
            eval_input(
                r#"
            fn inc(x) { x + 1 }
            fn double(x) { x * 2 }
            let funcs = [inc, double];
            let total = 0;
            for i, f in funcs {
                total = total + f(10);
            }
            total
            "#,
            )
            .expect_integer(),
            31
        );
    }

    #[test]
    fn vm_handles_collections_members_and_builtins() {
        assert_eq!(
            eval_input(
                r#"
            let xs = [1, 2];
            append(xs, 3);
            let user = {name: "tom", score: xs[2]};
            user.score = user.score + len(xs);
            user.score
            "#,
            )
            .expect_integer(),
            6
        );
    }

    #[test]
    fn vm_builtin_call_optimization_respects_local_shadowing() {
        assert_eq!(
            eval_input(
                r#"
            let len = fn(value) {
                value + 10
            };
            len(5)
            "#,
            )
            .expect_integer(),
            15
        );
    }

    #[test]
    fn vm_supports_structs_and_struct_literals() {
        assert_eq!(
            eval_input(
                r#"
            struct User { name; age; }
            let user = User{name: "tom", age: 18};
            user.age = user.age + 1;
            let other = User{"ann", 20};
            user.age + other.age
            "#,
            )
            .expect_integer(),
            39
        );
    }

    #[test]
    fn vm_supports_namespace_imports_with_functions_and_structs() {
        let module_path = temp_file_path("vm_models.monkey");
        fs::write(
            module_path.as_str(),
            r#"
            struct User { name; age; }
            fn next_age(user) {
                user.age + 1
            }
            export User;
            export next_age;
            "#,
        )
        .unwrap();
        let source = format!(
            r#"
            import model from "{}";
            let user = model.User{{name: "tom", age: 18}};
            model.next_age(user)
            "#,
            monkey_string_content(module_path.as_str())
        );
        assert_eq!(
            eval_input_with_path(&source, Some("vm_test.monkey".to_string())).expect_integer(),
            19
        );
        let _ = fs::remove_file(module_path);
    }

    #[test]
    fn vm_supports_bare_imports_and_import_cache() {
        let module_path = temp_file_path("vm_counter.monkey");
        fs::write(
            module_path.as_str(),
            r#"
            count = count + 1;
            fn add(x, y) {
                x + y + count
            }
            export add;
            "#,
        )
        .unwrap();
        let source = format!(
            r#"
            let count = 0;
            import "{}";
            import again from "{}";
            add(2, 3) + again.add(2, 3)
            "#,
            monkey_string_content(module_path.as_str()),
            monkey_string_content(module_path.as_str())
        );
        assert_eq!(
            eval_input_with_path(&source, Some("vm_test.monkey".to_string())).expect_integer(),
            12
        );
        let _ = fs::remove_file(module_path);
    }

    #[test]
    fn vm_supports_stdlib_exports() {
        assert_eq!(
            eval_input(
                r#"
            import json from "stdlib/json.monkey";
            json.json_stringify({ok: true, value: 42})
            "#,
            )
            .expect_string(),
            "{\"ok\":true,\"value\":42}"
        );
    }

    #[test]
    fn vm_direct_calls_imported_stdlib_builtins() {
        let (value, profile) = profile_input(
            r#"
            import json from "stdlib/json.monkey";
            json.json_stringify({ok: true, value: 42})
            "#,
        );
        assert_eq!(value.expect_string(), "{\"ok\":true,\"value\":42}");
        assert_eq!(profile.builtin_counts.get("json_stringify"), Some(&1));
        assert_eq!(profile.instruction_counts.get("CallBuiltin"), Some(&1));
        assert_eq!(profile.instruction_counts.get("Member"), None);
        assert_eq!(profile.instruction_counts.get("Call"), None);
        assert!(profile.import_count > 0);
        assert!(profile.heap_allocated > 0);
        assert!(profile.instruction_elapsed.contains_key("CallBuiltin"));
        assert!(profile.builtin_elapsed.contains_key("json_stringify"));
        assert!(profile.allocation_counts.contains_key("map"));
    }

    #[test]
    fn vm_direct_calls_bare_imported_stdlib_builtins() {
        let (value, profile) = profile_input(
            r#"
            import "stdlib/json.monkey";
            json_stringify({ok: true, value: 42})
            "#,
        );
        assert_eq!(value.expect_string(), "{\"ok\":true,\"value\":42}");
        assert_eq!(profile.builtin_counts.get("json_stringify"), Some(&1));
        assert_eq!(profile.instruction_counts.get("CallBuiltin"), Some(&1));
        assert_eq!(profile.instruction_counts.get("Call"), None);
    }

    #[test]
    fn vm_does_not_treat_stdlib_exports_as_globals() {
        let err = eval_error_with_path(
            r#"
            json_stringify({ok: true})
            "#,
            Some("vm_test.monkey".to_string()),
        );
        assert_eq!(err.kind, ErrorKind::Name);
    }

    #[test]
    fn vm_json_stringify_handles_vm_values_and_cycles() {
        assert_eq!(
            eval_input(
                r#"
                import json from "stdlib/json.monkey";
                struct User { name; age; }
                let data = {user: User{name: "tom", age: 18}, tags: ["a", null, 3]};
                json.json_stringify(data)
                "#,
            )
            .expect_string(),
            "{\"tags\":[\"a\",null,3],\"user\":{\"name\":\"tom\",\"age\":18}}"
        );

        let err = eval_error_with_path(
            r#"
            import json from "stdlib/json.monkey";
            let values = [];
            append(values, values);
            json.json_stringify(values)
            "#,
            Some("vm_test.monkey".to_string()),
        );
        assert_eq!(err.kind, ErrorKind::Type);
        assert_eq!(err.msg, "cannot stringify cyclic value");

        let err = eval_error_with_path(
            r#"
            import json from "stdlib/json.monkey";
            fn f() { 1 }
            json.json_stringify([f])
            "#,
            Some("vm_test.monkey".to_string()),
        );
        assert_eq!(err.kind, ErrorKind::Type);
        assert_eq!(err.msg, "cannot stringify function");
    }

    #[test]
    fn vm_supports_string_index_and_prelude_split() {
        assert_eq!(
            eval_input(
                r#"
            import "stdlib/prelude.monkey";
            let user = {name: "tom", scores: range(1, 5)};
            user.total = sum(user.scores);
            user.lines = split("a,b,c", ",");
            if contains(user.scores, 3) {
                user.lines[1]
            } else {
                "missing"
            }
            "#,
            )
            .expect_string(),
            "b"
        );
    }

    #[test]
    fn vm_imported_runtime_error_keeps_imported_source_path() {
        let module_path = temp_file_path("vm_bad_import.monkey");
        fs::write(module_path.as_str(), "\"abc\"[9];\nexport missing;\n").unwrap();
        let source = format!(
            r#"import "{}";"#,
            monkey_string_content(module_path.as_str())
        );

        let err = eval_error_with_path(&source, Some("vm_test.monkey".to_string()));
        assert_eq!(err.kind, ErrorKind::Index);
        let expected_path = fs::canonicalize(module_path.as_str())
            .unwrap()
            .to_string_lossy()
            .into_owned();
        assert_eq!(err.source_path.as_deref(), Some(expected_path.as_str()));

        let _ = fs::remove_file(module_path);
    }

    #[test]
    fn vm_detects_cyclic_imports() {
        let dir = temp_dir_path("vm_cycle");
        fs::create_dir_all(dir.as_str()).unwrap();
        let a_path = std::path::Path::new(dir.as_str())
            .join("a.monkey")
            .to_string_lossy()
            .into_owned();
        let b_path = std::path::Path::new(dir.as_str())
            .join("b.monkey")
            .to_string_lossy()
            .into_owned();
        fs::write(a_path.as_str(), "import \"b.monkey\";\n").unwrap();
        fs::write(b_path.as_str(), "import \"a.monkey\";\n").unwrap();
        let source = "import \"a.monkey\";\n";
        let entry_path = std::path::Path::new(dir.as_str())
            .join("main.monkey")
            .to_string_lossy()
            .into_owned();

        let err = eval_error_with_path(source, Some(entry_path));
        assert_eq!(err.kind, ErrorKind::Import);
        assert!(err.msg.contains("cyclic import"), "{}", err.msg);

        let _ = fs::remove_file(a_path);
        let _ = fs::remove_file(b_path);
        let _ = fs::remove_dir(dir);
    }

    #[test]
    fn vm_requires_boolean_logic_operands() {
        let lexer = new_lexer("true && 1".to_string(), Some("vm_test.monkey".to_string()));
        let mut parser = new_parser(lexer);
        let program = parser.parse_program().unwrap();
        let err = match eval_vm(&program) {
            Ok(value) => panic!("expected type error, got {value}"),
            Err(err) => err,
        };
        assert_eq!(err.kind, crate::error::ErrorKind::Type);
    }

    fn monkey_string_content(value: &str) -> String {
        let mut output = String::new();
        for ch in value.chars() {
            match ch {
                '\\' => output.push_str("\\\\"),
                '"' => output.push_str("\\\""),
                '\n' => output.push_str("\\n"),
                '\r' => output.push_str("\\r"),
                '\t' => output.push_str("\\t"),
                '\0' => output.push_str("\\0"),
                _ => output.push(ch),
            }
        }
        output
    }

    fn temp_file_path(name: &str) -> String {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir()
            .join(format!("{nanos}_{name}"))
            .to_string_lossy()
            .into_owned()
    }

    fn temp_dir_path(name: &str) -> String {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir()
            .join(format!("{nanos}_{name}"))
            .to_string_lossy()
            .into_owned()
    }
}
