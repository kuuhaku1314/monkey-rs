use crate::ast::{
    AssignStatement, AssignTarget, BlockStatement, BreakStatement, CallExpression,
    ContinueStatement, ExportStatement, Expression, ExpressionStatement, ForInStatement,
    FunctionLiteral, Identifier, IfExpression, ImportStatement, IndexExpression, InfixExpression,
    LetStatement, MapLiteral, MemberExpression, PrefixExpression, Program, ReturnStatement,
    SliceLiteral, Statement, StructLiteral, StructStatement, WhileStatement,
};
use crate::builtin::{new_builtin_function_map, new_global_builtin_function_map};
use crate::error::{CallFrame, Error, ErrorKind};
use crate::lexer::new_lexer;
use crate::lexer::Span;
use crate::object::{
    current_env_exports, define_env, enter_env, from_env, has_current_env_binding, mark_env_export,
    to_env, AbleToMapKey, Env, FunctionObject, Object, StructInstanceObject, StructTypeObject,
};
use crate::parser::new_parser;
use crate::token::Token;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::LazyLock;

pub type BuildInFn = fn(&[Object]) -> Result<Object, Error>;
pub static BUILD_IN_FUNCTIONS: LazyLock<HashMap<&'static str, BuildInFn>> =
    LazyLock::new(|| new_builtin_function_map());
pub static GLOBAL_BUILD_IN_FUNCTIONS: LazyLock<HashMap<&'static str, BuildInFn>> =
    LazyLock::new(|| new_global_builtin_function_map());

#[derive(Default)]
struct EvalContext {
    call_stack: Vec<CallFrame>,
    source_stack: Vec<Option<String>>,
    import_stack: Vec<PathBuf>,
    module_cache: HashMap<PathBuf, Object>,
}

pub fn eval(program: &Program, env: Env) -> Result<Object, Error> {
    let mut ctx = EvalContext::default();
    eval_program(program, env, &mut ctx)
}

fn eval_program(program: &Program, env: Env, ctx: &mut EvalContext) -> Result<Object, Error> {
    ctx.source_stack.push(program.path.to_owned());
    let result = eval_statements(&program.statements, true, env, ctx);
    ctx.source_stack.pop();
    result
}

impl EvalContext {
    fn current_source_path(&self) -> Option<String> {
        self.source_stack
            .iter()
            .rev()
            .find_map(|path| path.to_owned())
    }

    fn attach_source(&self, err: Error) -> Error {
        err.with_source_path(self.current_source_path())
    }

    fn error(&self, kind: ErrorKind, msg: String, span: Span) -> Error {
        Error::with_kind_source_span(kind, msg, self.current_source_path(), span)
    }
}

fn eval_statement(stmt: &Statement, env: Env, ctx: &mut EvalContext) -> Result<Object, Error> {
    let _ = stmt.span();
    match stmt {
        Statement::Block(stmt) => eval_block_statement(stmt, enter_env(Rc::clone(&env)), ctx),
        Statement::Empty(_) => Ok(Object::Empty),
        Statement::Expression(stmt) => eval_expression_statement(stmt, Rc::clone(&env), ctx),
        Statement::Let(stmt) => eval_let_statement(stmt, Rc::clone(&env), ctx),
        Statement::Return(stmt) => eval_return_statement(stmt, Rc::clone(&env), ctx),
        Statement::Import(stmt) => eval_import_statement(stmt, Rc::clone(&env), ctx),
        Statement::Export(stmt) => eval_export_statement(stmt, Rc::clone(&env)),
        Statement::Struct(stmt) => eval_struct_statement(stmt, Rc::clone(&env)),
        Statement::Break(stmt) => eval_break_statement(stmt),
        Statement::Continue(stmt) => eval_continue_statement(stmt),
        Statement::While(stmt) => eval_while_statement(stmt, Rc::clone(&env), ctx),
        Statement::Assign(stmt) => eval_assign_statement(stmt, Rc::clone(&env), ctx),
        Statement::ForIn(stmt) => eval_for_in_statement(stmt, enter_env(Rc::clone(&env)), ctx),
    }
}

fn eval_expression(exp: &Expression, env: Env, ctx: &mut EvalContext) -> Result<Object, Error> {
    match exp {
        Expression::Bool(v) => Ok(Object::Boolean(v.value)),
        Expression::Null(_) => Ok(Object::Null),
        Expression::Float(v) => Ok(Object::Float(v.value)),
        Expression::Integer(v) => Ok(Object::Integer(v.value)),
        Expression::String(v) => Ok(Object::String(v.value.to_owned())),
        Expression::Function(v) => eval_function_literal(v, Rc::clone(&env)),
        Expression::Call(v) => eval_call_expression(v, Rc::clone(&env), ctx),
        Expression::Identifier(v) => eval_identifier(v, Rc::clone(&env)),
        Expression::If(v) => eval_if_expression(v, Rc::clone(&env), ctx),
        Expression::Infix(v) => eval_infix_expression(v, Rc::clone(&env), ctx),
        Expression::Prefix(v) => eval_prefix_expression(v, Rc::clone(&env), ctx),
        Expression::Slice(v) => eval_slice_literal(v, Rc::clone(&env), ctx),
        Expression::Map(v) => eval_map_literal(v, Rc::clone(&env), ctx),
        Expression::StructLiteral(v) => eval_struct_literal(v, Rc::clone(&env), ctx),
        Expression::Index(v) => eval_index_expression(v, Rc::clone(&env), ctx),
        Expression::Member(v) => eval_member_expression(v, Rc::clone(&env), ctx),
    }
}

fn eval_let_statement(
    stmt: &LetStatement,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    let value = eval_expression(&stmt.value, Rc::clone(&env), ctx)?;
    define_env(&env, stmt.name.name.to_owned(), user_value(value));
    Ok(Object::Empty)
}

fn eval_struct_statement(stmt: &StructStatement, env: Env) -> Result<Object, Error> {
    let mut fields = Vec::new();
    for field in stmt.fields.iter() {
        if fields.contains(&field.name) {
            return Err(Error::with_kind_span(
                ErrorKind::Name,
                format!(
                    "duplicate field '{}' in struct {}",
                    field.name, stmt.name.name
                ),
                field.span,
            ));
        }
        fields.push(field.name.to_owned());
    }
    define_env(
        &env,
        stmt.name.name.to_owned(),
        Object::StructType(Rc::new(StructTypeObject {
            name: stmt.name.name.to_owned(),
            fields,
        })),
    );
    Ok(Object::Empty)
}

fn eval_return_statement(
    stmt: &ReturnStatement,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    Ok(Object::ReturnValue(Box::new(user_value(eval_expression(
        &stmt.value,
        env,
        ctx,
    )?))))
}

fn eval_export_statement(stmt: &ExportStatement, env: Env) -> Result<Object, Error> {
    if !has_current_env_binding(&env, stmt.name.name.as_str()) {
        if BUILD_IN_FUNCTIONS.contains_key(stmt.name.name.as_str()) {
            define_env(
                &env,
                stmt.name.name.to_owned(),
                Object::BuildIn(stmt.name.name.to_owned()),
            );
        } else {
            return Err(Error::with_kind_span(
                ErrorKind::Name,
                format!("cannot export undefined name '{}'", stmt.name.name),
                stmt.name.span,
            ));
        }
    }
    mark_env_export(&env, stmt.name.name.to_owned());
    Ok(Object::Empty)
}

fn eval_import_statement(
    stmt: &ImportStatement,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    let path = stmt.path.value.to_owned();
    let resolved_path = resolve_import_path(path.as_str(), ctx.current_source_path().as_deref())
        .map_err(|err| {
            ctx.error(
                ErrorKind::Import,
                format!("import {} failed: {}", path.as_str(), err),
                stmt.span,
            )
        })?;
    let canonical_path = fs::canonicalize(resolved_path.as_path()).map_err(|err| {
        ctx.error(
            ErrorKind::Import,
            format!("import {} failed: {}", resolved_path.display(), err),
            stmt.span,
        )
    })?;
    if let Some(module) = ctx.module_cache.get(&canonical_path).cloned() {
        if let Some(alias) = &stmt.alias {
            define_env(&env, alias.name.to_owned(), module);
        } else {
            inject_module_object(&env, &module)?;
        }
        return Ok(Object::Empty);
    }
    if ctx.import_stack.contains(&canonical_path) {
        return Err(ctx.error(
            ErrorKind::Import,
            format!("cyclic import {}", canonical_path.display()),
            stmt.span,
        ));
    }

    let source = fs::read_to_string(canonical_path.as_path()).map_err(|err| {
        ctx.error(
            ErrorKind::Import,
            format!("import {} failed: {}", canonical_path.display(), err),
            stmt.span,
        )
    })?;
    let import_path = canonical_path.to_string_lossy().into_owned();
    let mut parser = new_parser(new_lexer(source, Some(import_path.to_owned())));
    let program = parser.parse_program().map_err(|err| {
        err.with_source_path(Some(import_path.to_owned()))
            .with_stack(&ctx.call_stack)
    })?;

    ctx.import_stack.push(canonical_path.to_owned());
    let module_env = enter_env(Rc::clone(&env));
    let result = eval_program(&program, Rc::clone(&module_env), ctx);
    ctx.import_stack.pop();
    result?;
    let module = env_to_module_object(&module_env);
    ctx.module_cache
        .insert(canonical_path.to_owned(), module.to_owned());
    if let Some(alias) = &stmt.alias {
        define_env(&env, alias.name.to_owned(), module);
    } else {
        inject_module_object(&env, &module)?;
    }
    Ok(Object::Empty)
}

fn inject_module_object(env: &Env, module: &Object) -> Result<(), Error> {
    match module {
        Object::StructInstance(module) => {
            let entries = module
                .borrow()
                .values
                .iter()
                .filter_map(|(key, value)| match key {
                    AbleToMapKey::String(name) => Some((name.to_owned(), value.clone_value())),
                    _ => None,
                })
                .collect::<Vec<_>>();
            for (name, value) in entries {
                define_env(env, name, value);
            }
            Ok(())
        }
        _ => Err(Error::with_kind(
            ErrorKind::Import,
            "cached module is not a module object".to_string(),
        )),
    }
}

fn env_to_module_object(env: &Env) -> Object {
    let exported_names = current_env_exports(env);
    let env = env.borrow();
    let fields = exported_names;
    let values = fields
        .iter()
        .filter_map(|name| {
            env.store
                .get(name)
                .map(|value| (AbleToMapKey::String(name.to_owned()), value.clone_value()))
        })
        .collect::<HashMap<_, _>>();
    Object::StructInstance(Rc::new(RefCell::new(StructInstanceObject {
        type_name: "module".to_string(),
        fields,
        values,
    })))
}

fn resolve_import_path(path: &str, current_source_path: Option<&str>) -> Result<PathBuf, String> {
    let import_path = Path::new(path);
    if import_path.is_absolute() {
        return Ok(import_path.to_path_buf());
    }
    if let Some(source_path) = current_source_path {
        if let Some(parent) = Path::new(source_path).parent() {
            let relative_to_source = parent.join(import_path);
            if relative_to_source.exists() {
                return Ok(relative_to_source);
            }
        }
    }
    Ok(std::env::current_dir()
        .map_err(|err| err.to_string())?
        .join(import_path))
}

fn eval_break_statement(stmt: &BreakStatement) -> Result<Object, Error> {
    Ok(Object::Break(stmt.span))
}

fn eval_continue_statement(stmt: &ContinueStatement) -> Result<Object, Error> {
    Ok(Object::Continue(stmt.span))
}

fn eval_expression_statement(
    stmt: &ExpressionStatement,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    eval_expression(&stmt.expression, env, ctx)
}

fn eval_block_statement(
    block: &BlockStatement,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    eval_statements(&block.statements, false, env, ctx)
}

fn eval_assign_statement(
    stmt: &AssignStatement,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    match &stmt.target {
        AssignTarget::Identifier(identifier) => {
            let value = user_value(eval_expression(&stmt.value, Rc::clone(&env), ctx)?);
            assign_identifier(identifier, value, env)
        }
        AssignTarget::Index { left, index, .. } => {
            let index = eval_expression(index, Rc::clone(&env), ctx)?;
            let left = eval_expression(left, Rc::clone(&env), ctx)?;
            let value = user_value(eval_expression(&stmt.value, Rc::clone(&env), ctx)?);
            assign_index(left, index, value, stmt.span)
        }
        AssignTarget::Member { left, property, .. } => {
            let left = eval_expression(left, Rc::clone(&env), ctx)?;
            let value = user_value(eval_expression(&stmt.value, Rc::clone(&env), ctx)?);
            assign_member(left, property, value, stmt.span)
        }
    }
}

fn assign_index(left: Object, index: Object, value: Object, span: Span) -> Result<Object, Error> {
    match left {
        Object::Map(map) => {
            let key = index
                .try_into()
                .map_err(|err: Error| Error::with_kind_span(err.kind, err.msg, span))?;
            Object::Map(map)
                .field_set(key, value)
                .map_err(|err| Error::with_kind_span(err.kind, err.msg, span))?;
        }
        Object::StructInstance(instance) => {
            let key = index
                .try_into()
                .map_err(|err: Error| Error::with_kind_span(err.kind, err.msg, span))?;
            Object::StructInstance(instance)
                .field_set(key, value)
                .map_err(|err| Error::with_kind_span(err.kind, err.msg, span))?;
        }
        Object::Slice(vec) => match index {
            Object::Integer(i) => {
                Object::Slice(vec)
                    .list_set(i, value)
                    .map_err(|err| Error::with_kind_span(err.kind, err.msg, span))?;
            }
            _ => return Err(operation_type_error(span)),
        },
        _ => return Err(operation_type_error(span)),
    }
    Ok(Object::Empty)
}

fn assign_member(
    left: Object,
    property: &Identifier,
    value: Object,
    span: Span,
) -> Result<Object, Error> {
    match left {
        Object::Map(map) => {
            Object::Map(map)
                .field_set(property.name.as_str().into(), value)
                .map_err(|err| Error::with_kind_span(err.kind, err.msg, span))?;
            Ok(Object::Empty)
        }
        Object::StructInstance(instance) => {
            Object::StructInstance(instance)
                .field_set(property.name.as_str().into(), value)
                .map_err(|err| Error::with_kind_span(err.kind, err.msg, span))?;
            Ok(Object::Empty)
        }
        _ => Err(operation_type_error(span)),
    }
}

fn eval_while_statement(
    exp: &WhileStatement,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    loop {
        if let Object::Boolean(ok) = eval_expression(&exp.condition, Rc::clone(&env), ctx)? {
            if ok {
                let result = eval_block_statement(&exp.consequence, Rc::clone(&env), ctx)?;
                match result {
                    Object::ReturnValue(_) => return Ok(result),
                    Object::Break(_) => return Ok(Object::Empty),
                    Object::Continue(_) => continue,
                    _ => {}
                }
                continue;
            } else {
                return Ok(Object::Empty);
            }
        } else {
            return Err(operation_type_error(exp.span));
        }
    }
}

fn eval_for_in_statement(
    exp: &ForInStatement,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    let collection = eval_expression(&exp.collection, Rc::clone(&env), ctx)?;
    match collection {
        Object::Map(map) => {
            let entries = map
                .borrow()
                .iter()
                .map(|(k, v)| (k.to_owned(), v.to_owned()))
                .collect::<Vec<_>>();
            for (k, v) in entries {
                {
                    let store = &mut env.borrow_mut().store;
                    store.insert(exp.key.name.to_owned(), k.into());
                    if let Some(value) = &exp.value {
                        store.insert(value.name.to_owned(), v);
                    }
                }
                let result = eval_block_statement(&exp.body, enter_env(Rc::clone(&env)), ctx)?;
                match result {
                    Object::ReturnValue(_) => return Ok(result),
                    Object::Break(_) => return Ok(Object::Empty),
                    Object::Continue(_) => continue,
                    _ => {}
                }
            }
        }
        Object::Slice(vec) => {
            let elements = vec.borrow().iter().cloned().collect::<Vec<_>>();
            for (i, element) in elements.into_iter().enumerate() {
                {
                    let store = &mut env.borrow_mut().store;
                    store.insert(exp.key.name.to_owned(), Object::Integer(i as i64));
                    if let Some(value) = &exp.value {
                        store.insert(value.name.to_owned(), element);
                    }
                }
                let result = eval_block_statement(&exp.body, enter_env(Rc::clone(&env)), ctx)?;
                match result {
                    Object::ReturnValue(_) => return Ok(result),
                    Object::Break(_) => return Ok(Object::Empty),
                    Object::Continue(_) => continue,
                    _ => {}
                }
            }
        }
        _ => return Err(operation_type_error(exp.span)),
    };
    Ok(Object::Empty)
}

fn eval_statements(
    stmts: &[Statement],
    is_root: bool,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    let mut result = Object::Empty;
    let mut result_is_empty = false;
    if !stmts.is_empty() {
        if let Statement::Expression(ref expression_statement) = stmts[stmts.len() - 1] {
            result_is_empty = expression_statement.end_of_semicolon
        }
    }
    for stmt in stmts.iter() {
        result =
            eval_statement(stmt, Rc::clone(&env), ctx).map_err(|err| ctx.attach_source(err))?;
        match result {
            Object::ReturnValue(v) => {
                return if is_root {
                    Ok(*v)
                } else {
                    Ok(Object::ReturnValue(v))
                };
            }
            Object::Break(span) => {
                return if is_root {
                    Err(ctx.error(ErrorKind::Runtime, "break outside loop".to_string(), span))
                } else {
                    Ok(Object::Break(span))
                };
            }
            Object::Continue(span) => {
                return if is_root {
                    Err(ctx.error(
                        ErrorKind::Runtime,
                        "continue outside loop".to_string(),
                        span,
                    ))
                } else {
                    Ok(Object::Continue(span))
                };
            }
            _ => {}
        }
    }
    if result_is_empty {
        Ok(Object::Empty)
    } else {
        Ok(result)
    }
}

fn eval_prefix_expression(
    exp: &PrefixExpression,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    match exp.token {
        Token::Bang => eval_bang_operator_expression(&exp.right, Rc::clone(&env), ctx),
        Token::Minus => eval_minus_operator_expression(&exp.right, Rc::clone(&env), ctx),
        _ => unreachable!(),
    }
}

fn eval_infix_expression(
    exp: &InfixExpression,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    if exp.token.is_assign() {
        return eval_assign_expression(
            exp,
            eval_expression(&exp.right, Rc::clone(&env), ctx)?,
            Rc::clone(&env),
        );
    }
    let left = eval_expression(&exp.left, Rc::clone(&env), ctx)?;
    if exp.token.is_and() || exp.token.is_or() {
        return eval_logic_operator_expression(exp, left, Rc::clone(&env), ctx);
    }
    let right = eval_expression(&exp.right, Rc::clone(&env), ctx)?;
    match exp.token {
        Token::EQ | Token::NotEq | Token::LT | Token::Lte | Token::GT | Token::Gte => {
            eval_compare_operator_expression(exp, left, right)
        }
        Token::Plus | Token::Minus | Token::Slash | Token::Asterisk => {
            eval_compute_operator_expression(exp, left, right)
        }
        _ => unreachable!(),
    }
}

fn eval_logic_operator_expression(
    exp: &InfixExpression,
    left: Object,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    let Object::Boolean(left) = left else {
        return Err(operation_type_error(exp.left.span()));
    };
    match exp.token {
        Token::And => {
            if !left {
                return Ok(Object::Boolean(false));
            }
            match eval_expression(&exp.right, env, ctx)? {
                Object::Boolean(right) => Ok(Object::Boolean(right)),
                _ => Err(operation_type_error(exp.right.span())),
            }
        }
        Token::Or => {
            if left {
                return Ok(Object::Boolean(true));
            }
            match eval_expression(&exp.right, env, ctx)? {
                Object::Boolean(right) => Ok(Object::Boolean(right)),
                _ => Err(operation_type_error(exp.right.span())),
            }
        }
        _ => unreachable!(),
    }
}

fn eval_identifier(exp: &Identifier, env: Env) -> Result<Object, Error> {
    let value = from_env(&env, &exp.name);
    if let Some(value) = value {
        Ok(value)
    } else if GLOBAL_BUILD_IN_FUNCTIONS.contains_key(exp.name.as_str()) {
        Ok(Object::BuildIn(exp.name.to_owned()))
    } else {
        Err(Error::with_kind_span(
            ErrorKind::Name,
            format!("not found variable name '{}'", exp.name),
            exp.span,
        ))
    }
}

fn eval_if_expression(
    exp: &IfExpression,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    if let Object::Boolean(ok) = eval_expression(&exp.condition, Rc::clone(&env), ctx)? {
        if ok {
            eval_block_statement(&exp.consequence, Rc::clone(&env), ctx)
        } else if let Some(stmt) = &exp.alternative {
            eval_block_statement(stmt, Rc::clone(&env), ctx)
        } else if let Some(exp) = &exp.optional {
            eval_expression(exp, Rc::clone(&env), ctx)
        } else {
            Ok(Object::Empty)
        }
    } else {
        Err(operation_type_error(exp.span))
    }
}

fn eval_function_literal(exp: &FunctionLiteral, env: Env) -> Result<Object, Error> {
    let func = Object::Function(Rc::new(FunctionObject {
        literal: exp.to_owned(),
        env: Rc::clone(&env),
    }));
    if let Some(name) = &exp.name {
        env.borrow_mut()
            .store
            .insert(name.name.to_owned(), func.to_owned());
    }
    Ok(func)
}

fn eval_map_literal(exp: &MapLiteral, env: Env, ctx: &mut EvalContext) -> Result<Object, Error> {
    let mut map = HashMap::new();
    for (k, v) in exp.kv_pair.iter() {
        let key = eval_expression(k, Rc::clone(&env), ctx)?;
        let value = eval_expression(v, Rc::clone(&env), ctx)?;
        map.insert(key.try_into()?, value);
    }
    Ok(Object::Map(Rc::new(RefCell::new(map))))
}

fn eval_struct_literal(
    exp: &StructLiteral,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    let struct_type = match eval_expression(&exp.name, Rc::clone(&env), ctx)? {
        Object::StructType(value) => value,
        value => {
            return Err(Error::with_kind_span(
                ErrorKind::Type,
                format!("{} is not a struct", value),
                exp.name.span(),
            ))
        }
    };
    let mut values = HashMap::new();
    for field in struct_type.fields.iter() {
        values.insert(AbleToMapKey::String(field.to_owned()), Object::Null);
    }
    if !exp.values.is_empty() {
        if exp.values.len() != struct_type.fields.len() {
            return Err(Error::with_kind_span(
                ErrorKind::Type,
                format!(
                    "struct {} expects {} values, got {}",
                    struct_type.name,
                    struct_type.fields.len(),
                    exp.values.len()
                ),
                exp.span,
            ));
        }
        for (index, value) in exp.values.iter().enumerate() {
            let field = &struct_type.fields[index];
            values.insert(
                AbleToMapKey::String(field.to_owned()),
                eval_expression(value, Rc::clone(&env), ctx)?,
            );
        }
        return Ok(Object::StructInstance(Rc::new(RefCell::new(
            StructInstanceObject {
                type_name: struct_type.name.to_owned(),
                fields: struct_type.fields.to_owned(),
                values,
            },
        ))));
    }
    let mut assigned_fields = HashSet::new();
    for (field, value) in exp.fields.iter() {
        if !struct_type
            .fields
            .iter()
            .any(|allowed| allowed == &field.name)
        {
            return Err(Error::with_kind_span(
                ErrorKind::Name,
                format!(
                    "unknown field '{}' for struct {}",
                    field.name, struct_type.name
                ),
                field.span,
            ));
        }
        if !assigned_fields.insert(field.name.to_owned()) {
            return Err(Error::with_kind_span(
                ErrorKind::Name,
                format!(
                    "duplicate field '{}' in struct literal {}",
                    field.name, struct_type.name
                ),
                field.span,
            ));
        }
        values.insert(
            AbleToMapKey::String(field.name.to_owned()),
            eval_expression(value, Rc::clone(&env), ctx)?,
        );
    }
    Ok(Object::StructInstance(Rc::new(RefCell::new(
        StructInstanceObject {
            type_name: struct_type.name.to_owned(),
            fields: struct_type.fields.to_owned(),
            values,
        },
    ))))
}

fn eval_slice_literal(
    exp: &SliceLiteral,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    let mut vec = Vec::new();
    for exp in exp.elements.iter() {
        vec.push(eval_expression(exp, Rc::clone(&env), ctx)?)
    }
    Ok(Object::Slice(Rc::new(RefCell::new(vec))))
}

fn eval_call_expression(
    exp: &CallExpression,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    let func = eval_expression(&exp.function, Rc::clone(&env), ctx)?;
    let mut params = vec![];
    for x in exp.arguments.iter() {
        let result = eval_expression(x, Rc::clone(&env), ctx)?;
        params.push(result)
    }
    match func {
        Object::Function(body) => {
            if body.literal.parameters.len() != params.len() {
                return Err(Error::with_kind_span(
                    ErrorKind::Runtime,
                    "the number of parameter not match".to_string(),
                    exp.span,
                ));
            }
            let frame_name = call_frame_name(exp, Some(&body.literal.name));
            let env = enter_env(Rc::clone(&body.env));
            for (i, param) in params.into_iter().enumerate() {
                define_env(&env, body.literal.parameters[i].name.to_owned(), param);
            }
            ctx.call_stack.push(CallFrame {
                name: frame_name,
                span: exp.span,
                source_path: ctx.current_source_path(),
            });
            let result = eval_statements(&body.literal.body.statements, true, env, ctx)
                .map_err(|err| err.with_stack(&ctx.call_stack));
            ctx.call_stack.pop();
            result.map(user_value)
        }
        Object::BuildIn(name) => {
            ctx.call_stack.push(CallFrame {
                name: name.to_owned(),
                span: exp.span,
                source_path: ctx.current_source_path(),
            });
            let Some(buildin) = BUILD_IN_FUNCTIONS.get(name.as_str()) else {
                return Err(Error::with_kind_span(
                    ErrorKind::Name,
                    format!("not found builtin function '{name}'"),
                    exp.span,
                ));
            };
            let result = match buildin(&params) {
                Ok(result) => Ok(user_value(result)),
                Err(err) => Err(Error::with_kind_span(
                    err.kind,
                    format!("call function {} error, cause:{}", name, err),
                    exp.span,
                )
                .with_stack(&ctx.call_stack)),
            };
            ctx.call_stack.pop();
            result
        }
        _ => Err(operation_type_error(exp.span)),
    }
}

fn user_value(value: Object) -> Object {
    match value {
        Object::Empty => Object::Null,
        value => value,
    }
}

fn call_frame_name(exp: &CallExpression, literal_name: Option<&Option<Identifier>>) -> String {
    match &exp.function {
        Expression::Identifier(identifier) => identifier.name.to_owned(),
        _ => literal_name
            .and_then(|name| name.as_ref().map(|identifier| identifier.name.to_owned()))
            .unwrap_or_else(|| "<anonymous>".to_string()),
    }
}

fn eval_index_expression(
    exp: &IndexExpression,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    let index = match eval_expression(&exp.index, Rc::clone(&env), ctx)? {
        Object::Integer(v) => Object::Integer(v),
        Object::Boolean(v) => Object::Boolean(v),
        Object::String(v) => Object::String(v),
        _ => return Err(operation_type_error(exp.span)),
    };
    let left = match eval_expression(&exp.left, Rc::clone(&env), ctx)? {
        Object::String(v) => Object::String(v),
        Object::Map(v) => Object::Map(v),
        Object::StructInstance(v) => Object::StructInstance(v),
        Object::Slice(v) => Object::Slice(v),
        _ => return Err(operation_type_error(exp.span)),
    };
    Ok(match (left, index) {
        (Object::String(ref result), Object::Integer(i)) => Object::String(
            result
                .chars()
                .nth(i as usize)
                .ok_or(Error::with_kind_span(
                    ErrorKind::Index,
                    "index out of bound".to_string(),
                    exp.span,
                ))?
                .to_string(),
        ),
        (Object::Slice(result), Object::Integer(i)) => Object::Slice(result)
            .list_get(i)
            .map_err(|err| Error::with_kind_span(err.kind, err.msg, exp.span))?,
        (Object::Map(result), key) => Object::Map(result).field_get(key.try_into()?)?,
        (Object::StructInstance(result), key) => {
            Object::StructInstance(result).field_get(key.try_into()?)?
        }
        _ => return Err(operation_type_error(exp.span)),
    })
}

fn eval_member_expression(
    exp: &MemberExpression,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    match eval_expression(&exp.left, env, ctx)? {
        Object::Map(map) => Object::Map(map).field_get(exp.property.name.as_str().into()),
        Object::StructInstance(instance) => {
            Object::StructInstance(instance).field_get(exp.property.name.as_str().into())
        }
        _ => Err(operation_type_error(exp.span)),
    }
}

fn eval_bang_operator_expression(
    exp: &Expression,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    match eval_expression(exp, env, ctx)? {
        Object::Boolean(v) => Ok(Object::Boolean(!v)),
        _ => Err(operation_type_error(exp.span())),
    }
}

fn eval_minus_operator_expression(
    exp: &Expression,
    env: Env,
    ctx: &mut EvalContext,
) -> Result<Object, Error> {
    match eval_expression(exp, env, ctx)? {
        Object::Integer(v) => Ok(Object::Integer(-v)),
        Object::Float(v) => Ok(Object::Float(-v)),
        _ => Err(operation_type_error(exp.span())),
    }
}

fn eval_compute_operator_expression(
    exp: &InfixExpression,
    left: Object,
    right: Object,
) -> Result<Object, Error> {
    let operator = exp.token.to_owned();
    let value = (left, right);
    Ok(match value {
        (Object::Integer(l), Object::Integer(r)) => match operator {
            Token::Plus => Object::Integer(l + r),
            Token::Minus => Object::Integer(l - r),
            Token::Asterisk => Object::Integer(l * r),
            Token::Slash => {
                if r == 0 {
                    return Err(Error::with_kind_span(
                        ErrorKind::Runtime,
                        "division by zero".to_string(),
                        exp.span,
                    ));
                }
                Object::Integer(l / r)
            }
            _ => return Err(operation_type_error(exp.span)),
        },
        (Object::Float(l), Object::Float(r)) => match operator {
            Token::Plus => Object::Float(l + r),
            Token::Minus => Object::Float(l - r),
            Token::Asterisk => Object::Float(l * r),
            Token::Slash => float_div(l, r, exp.span)?,
            _ => return Err(operation_type_error(exp.span)),
        },
        (Object::String(l), Object::String(r)) => match operator {
            Token::Plus => Object::String(l + r.as_str()),
            _ => return Err(operation_type_error(exp.span)),
        },
        (Object::String(l), Object::Integer(r)) => match operator {
            Token::Plus => Object::String(l + r.to_string().as_str()),
            _ => return Err(operation_type_error(exp.span)),
        },
        (Object::Integer(l), Object::String(r)) => match operator {
            Token::Plus => Object::String(l.to_string() + r.as_str()),
            _ => return Err(operation_type_error(exp.span)),
        },
        (Object::String(l), Object::Float(r)) => match operator {
            Token::Plus => Object::String(l + r.to_string().as_str()),
            _ => return Err(operation_type_error(exp.span)),
        },
        (Object::Float(l), Object::String(r)) => match operator {
            Token::Plus => Object::String(l.to_string() + r.as_str()),
            _ => return Err(operation_type_error(exp.span)),
        },
        (Object::Float(l), Object::Integer(r)) => match operator {
            Token::Plus => Object::Float(l + r as f64),
            Token::Minus => Object::Float(l - r as f64),
            Token::Asterisk => Object::Float(l * r as f64),
            Token::Slash => float_div(l, r as f64, exp.span)?,
            _ => return Err(operation_type_error(exp.span)),
        },
        (Object::Integer(l), Object::Float(r)) => match operator {
            Token::Plus => Object::Float(l as f64 + r),
            Token::Minus => Object::Float(l as f64 - r),
            Token::Asterisk => Object::Float(l as f64 * r),
            Token::Slash => float_div(l as f64, r, exp.span)?,
            _ => return Err(operation_type_error(exp.span)),
        },
        _ => return Err(operation_type_error(exp.span)),
    })
}

fn float_div(left: f64, right: f64, span: Span) -> Result<Object, Error> {
    if right == 0.0 {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "division by zero".to_string(),
            span,
        ));
    }
    Ok(Object::Float(left / right))
}

fn eval_assign_expression(exp: &InfixExpression, right: Object, env: Env) -> Result<Object, Error> {
    let left = match exp.left {
        Expression::Identifier(ref v) => v,
        _ => return Err(operation_type_error(exp.span)),
    };
    assign_identifier(left, right, env)
}

fn assign_identifier(identifier: &Identifier, right: Object, env: Env) -> Result<Object, Error> {
    let name = identifier.name.to_owned();
    if from_env(&env, &name).is_some() {
        to_env(&env, name, right.clone_value());
        Ok(right)
    } else {
        Err(Error::with_kind_span(
            ErrorKind::Name,
            format!("not found variable name '{}'", identifier.name),
            identifier.span,
        ))
    }
}

fn operation_type_error(span: Span) -> Error {
    Error::with_kind_span(ErrorKind::Type, "operation type invalid".to_string(), span)
}

macro_rules! eval_compare {
    ($operator:expr, $l:expr, $r:expr) => {
        match $operator {
            Token::EQ => Object::Boolean($l == $r),
            Token::NotEq => Object::Boolean($l != $r),
            Token::LT => Object::Boolean($l < $r),
            Token::Lte => Object::Boolean($l <= $r),
            Token::GT => Object::Boolean($l > $r),
            Token::Gte => Object::Boolean($l >= $r),
            _ => unreachable!(),
        }
    };
}

fn eval_compare_operator_expression(
    exp: &InfixExpression,
    left: Object,
    right: Object,
) -> Result<Object, Error> {
    let operator = exp.token.to_owned();
    let value = (left, right);
    Ok(match value {
        (Object::Integer(l), Object::Integer(r)) => eval_compare!(operator, l, r),
        (Object::Float(l), Object::Float(r)) => eval_compare!(operator, l, r),
        (Object::Boolean(l), Object::Boolean(r)) => eval_compare!(operator, l, r),
        (Object::Null, Object::Null) => match operator {
            Token::EQ => Object::Boolean(true),
            Token::NotEq => Object::Boolean(false),
            _ => return Err(operation_type_error(exp.span)),
        },
        (Object::String(l), Object::String(r)) => eval_compare!(operator, l, r),
        _ => return Err(operation_type_error(exp.span)),
    })
}

#[cfg(test)]
mod tests {
    use super::eval;
    use crate::error::ErrorKind;
    use crate::lexer::{new_lexer, Position, Span};
    use crate::object::{new_env, Object};
    use crate::parser::new_parser;
    use std::fs;
    use std::io::{Read, Write};
    use std::net::TcpListener;
    use std::rc::Rc;
    use std::thread;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn eval_input(input: &str) -> Result<Object, crate::error::Error> {
        eval_input_with_path(input, "test")
    }

    fn eval_input_with_path(input: &str, path: &str) -> Result<Object, crate::error::Error> {
        let mut parser = new_parser(new_lexer(input.to_owned(), Some(path.to_owned())));
        let program = parser.parse_program()?;
        eval(&program, Rc::clone(&new_env()))
    }

    fn assert_integer(input: &str, expected: i64) {
        match eval_input(input).unwrap() {
            Object::Integer(actual) => assert_eq!(actual, expected),
            other => panic!("expected integer {expected}, got {other}"),
        }
    }

    fn assert_string(input: &str, expected: &str) {
        match eval_input(input).unwrap() {
            Object::String(actual) => assert_eq!(actual, expected),
            other => panic!("expected string {expected}, got {other}"),
        }
    }

    fn echo_exec_expression() -> &'static str {
        if cfg!(windows) {
            r#"process.exec("cmd", ["/C", "echo monkey"])"#
        } else {
            r#"process.exec("sh", ["-c", "printf monkey"])"#
        }
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

    fn monkey_string_literal(value: &str) -> String {
        format!("\"{}\"", monkey_string_content(value))
    }

    #[test]
    fn return_inside_while_exits_function() {
        assert_integer(
            r#"
            fn f() {
                let i = 0;
                while i < 3 {
                    return 99;
                }
                return 0;
            }
            let result = f();
            if true { result }
            "#,
            99,
        );
    }

    #[test]
    fn return_inside_for_in_exits_function() {
        assert_integer(
            r#"
            fn f() {
                for i in [1, 2, 3] {
                    return i;
                }
                return 0;
            }
            let result = f();
            if true { result }
            "#,
            0,
        );
    }

    #[test]
    fn map_index_assignment_can_read_same_map_on_right_hand_side() {
        assert_integer(
            r#"
            let m = {"a": 2};
            m["a"] = m["a"] + 3;
            let result = m["a"];
            if true { result }
            "#,
            5,
        );
    }

    #[test]
    fn slice_index_assignment_can_read_same_slice_on_right_hand_side() {
        assert_integer(
            r#"
            let values = [2];
            values[0] = values[0] + 3;
            let result = values[0];
            if true { result }
            "#,
            5,
        );
    }

    #[test]
    fn index_assignment_accepts_expression_index() {
        assert_integer(
            r#"
            let values = [0, 0, 0, 0];
            let a = 1;
            let b = 2;
            values[a + b] = 7;
            let result = values[3];
            if true { result }
            "#,
            7,
        );
    }

    #[test]
    fn index_assignment_accepts_nested_index_left_value() {
        assert_integer(
            r#"
            let matrix = [[1, 2], [3, 4]];
            matrix[0][1] = 9;
            let result = matrix[0][1];
            if true { result }
            "#,
            9,
        );
    }

    #[test]
    fn index_assignment_accepts_member_left_value() {
        assert_integer(
            r#"
            let user = {scores: [1, 2, 3]};
            user.scores[1] = 8;
            let result = user.scores[1];
            if true { result }
            "#,
            8,
        );
    }

    #[test]
    fn slice_index_equal_to_len_is_an_error() {
        let result = eval_input(
            r#"
            let values = [1];
            let result = values[1];
            if true { result }
            "#,
        );
        let err = match result {
            Ok(value) => panic!("expected index error, got {value}"),
            Err(err) => err,
        };
        assert!(err.msg.contains("index out of bound"), "{}", err.msg);
    }

    #[test]
    fn function_captures_local_bindings() {
        assert_integer(
            r#"
            let make_adder = fn(x) {
                fn(y) {
                    x + y
                }
            };
            let add2 = make_adder(2);
            let result = add2(3);
            if true { result }
            "#,
            5,
        );
    }

    #[test]
    fn join_concatenates_slice_elements_with_separator() {
        assert_string(
            r#"
            let values = ["a", "b", 3];
            let result = join(values, "-");
            if true { result }
            "#,
            "a-b-3",
        );
    }

    #[test]
    fn parse_error_reports_token_span() {
        let mut parser = new_parser(new_lexer("let x = ;".to_owned(), Some("test".to_owned())));
        let err = match parser.parse_program() {
            Ok(_) => panic!("expected parse error"),
            Err(err) => err,
        };
        assert_eq!(err.span, Some(Span::new(Position(1, 9), Position(1, 10))));
    }

    #[test]
    fn runtime_index_error_reports_expression_span() {
        let result = eval_input("let xs = [1];\nlet y = xs[1];\n");
        let err = match result {
            Ok(value) => panic!("expected index error, got {value}"),
            Err(err) => err,
        };
        assert_eq!(err.span, Some(Span::new(Position(2, 9), Position(2, 14))));
    }

    #[test]
    fn index_expression_can_be_used_as_statement_value() {
        assert_integer(
            r#"
            let f = fn() {
                let xs = [7];
                xs[0]
            };
            let result = f();
            if true { result }
            "#,
            7,
        );
    }

    #[test]
    fn for_in_iterates_over_snapshot_when_slice_is_mutated() {
        assert_integer(
            r#"
            let xs = [1, 2];
            for i, v in xs {
                append(xs, v);
            }
            let result = len(xs);
            if true { result }
            "#,
            4,
        );
    }

    #[test]
    fn top_level_last_expression_can_omit_semicolon() {
        assert_integer(
            r#"
            let x = 1;
            x + 2
            "#,
            3,
        );
    }

    #[test]
    fn while_supports_break_and_continue() {
        assert_integer(
            r#"
            let i = 0;
            let total = 0;
            while i < 10 {
                i = i + 1;
                if i == 2 {
                    continue;
                }
                if i == 5 {
                    break;
                }
                total = total + i;
            }
            total
            "#,
            8,
        );
    }

    #[test]
    fn for_in_supports_break_and_continue() {
        assert_integer(
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
            4,
        );
    }

    #[test]
    fn break_outside_loop_is_error() {
        let result = eval_input("break;");
        let err = match result {
            Ok(value) => panic!("expected break error, got {value}"),
            Err(err) => err,
        };
        assert!(err.msg.contains("outside loop"), "{}", err.msg);
    }

    #[test]
    fn file_builtins_can_write_append_read_and_check_existence() {
        let path = temp_file_path("monkey_file_builtin.txt");
        let path_literal = monkey_string_literal(path.as_str());
        let source = format!(
            r#"
            import "stdlib/fs.monkey";
            let path = {};
            write_file(path, "hello");
            append_file(path, "\nworld");
            let content = read_file(path);
            let result = if content.ok && content.value == "hello\nworld" {{
                file_exists(path)
            }} else {{
                false
            }};
            if true {{ result }}
            "#,
            path_literal
        );

        match eval_input(&source).unwrap() {
            Object::Boolean(actual) => assert!(actual),
            other => panic!("expected boolean true, got {other}"),
        }

        let _ = fs::remove_file(path);
    }

    #[test]
    fn utility_builtins_support_string_time_args_and_assertions() {
        assert_string(
            r#"
            import time from "stdlib/time.monkey";
            assert(find("hello", "ll") == 2, "find failed");
            assert(substr("hello", 1, 3) == "ell", "substr failed");
            assert(replace("a-b-a", "a", "x") == "x-b-x", "replace failed");
            assert(char_code("A") == 65, "char_code failed");
            assert(from_char_code(65) == "A", "from_char_code failed");
            assert(is_int(time.time_ms()), "time_ms failed");
            assert(time.sleep(0) == null, "sleep failed");
            assert(is_list(args()), "args failed");
            "ok"
            "#,
            "ok",
        );
    }

    #[test]
    fn panic_builtin_returns_runtime_error() {
        let result = eval_input(r#"panic("bad");"#);
        let err = match result {
            Ok(value) => panic!("expected panic error, got {value}"),
            Err(err) => err,
        };
        assert!(err.msg.contains("bad"), "{}", err.msg);
        assert_eq!(err.source_path.as_deref(), Some("test"));
    }

    #[test]
    fn import_statement_loads_stdlib_into_current_env() {
        match eval_input(
            r#"
            import "stdlib/prelude.monkey";
            let result = sum(range(1, 5));
            if true { result }
            "#,
        )
        .unwrap()
        {
            Object::Integer(actual) => assert_eq!(actual, 10),
            other => panic!("expected integer 10, got {other}"),
        }
    }

    #[test]
    fn import_statement_loads_user_file_into_current_env() {
        let path = temp_file_path("monkey_import.monkey");
        let path_literal = monkey_string_literal(path.as_str());
        fs::write(
            path.as_str(),
            "fn imported_add(x) { x + 1 }\nexport imported_add;\n",
        )
        .unwrap();
        let source = format!(
            r#"
            import {};
            let result = imported_add(2);
            if true {{ result }}
            "#,
            path_literal
        );
        assert_integer(source.as_str(), 3);
        let _ = fs::remove_file(path);
    }

    #[test]
    fn import_resolves_paths_relative_to_current_file() {
        let dir = temp_dir_path("monkey_relative_import");
        fs::create_dir_all(dir.as_str()).unwrap();
        let main_path = std::path::Path::new(dir.as_str())
            .join("main.monkey")
            .to_string_lossy()
            .into_owned();
        let lib_path = std::path::Path::new(dir.as_str())
            .join("lib.monkey")
            .to_string_lossy()
            .into_owned();
        fs::write(
            lib_path.as_str(),
            "fn imported_value() { 42 }\nexport imported_value;\n",
        )
        .unwrap();
        let source = r#"
            import "lib.monkey";
            let result = imported_value();
            if true { result }
            "#;

        match eval_input_with_path(source, main_path.as_str()).unwrap() {
            Object::Integer(actual) => assert_eq!(actual, 42),
            other => panic!("expected integer 42, got {other}"),
        }

        let _ = fs::remove_file(lib_path);
        let _ = fs::remove_dir(dir);
    }

    #[test]
    fn import_cache_skips_repeated_imports() {
        let path = temp_file_path("monkey_cached_import.monkey");
        let path_literal = monkey_string_literal(path.as_str());
        fs::write(path.as_str(), "counter = counter + 1;\n").unwrap();
        let source = format!(
            r#"
            let counter = 0;
            import {path_literal};
            import {path_literal};
            if true {{ counter }}
            "#
        );

        match eval_input(source.as_str()).unwrap() {
            Object::Integer(actual) => assert_eq!(actual, 1),
            other => panic!("expected integer 1, got {other}"),
        }
        let _ = fs::remove_file(path);
    }

    #[test]
    fn import_detects_cycles() {
        let dir = temp_dir_path("monkey_cycle_import");
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
        let source = format!("import {};\n", monkey_string_literal(a_path.as_str()));

        let err = match eval_input(source.as_str()) {
            Ok(value) => panic!("expected cyclic import error, got {value}"),
            Err(err) => err,
        };
        assert!(err.msg.contains("cyclic import"), "{}", err.msg);
        let expected_b_path = fs::canonicalize(b_path.as_str())
            .unwrap()
            .to_string_lossy()
            .into_owned();
        assert_eq!(err.source_path.as_deref(), Some(expected_b_path.as_str()));

        let _ = fs::remove_file(a_path);
        let _ = fs::remove_file(b_path);
        let _ = fs::remove_dir(dir);
    }

    #[test]
    fn imported_runtime_error_keeps_imported_source_path() {
        let path = temp_file_path("monkey_import_runtime_error.monkey");
        fs::write(
            path.as_str(),
            r#"
            fn boom() {
                let xs = [1];
                xs[2]
            }
            boom();
            "#,
        )
        .unwrap();
        let source = format!("import {};\n", monkey_string_literal(path.as_str()));

        let err = match eval_input(source.as_str()) {
            Ok(value) => panic!("expected imported runtime error, got {value}"),
            Err(err) => err,
        };
        let expected_path = fs::canonicalize(path.as_str())
            .unwrap()
            .to_string_lossy()
            .into_owned();
        assert_eq!(err.source_path.as_deref(), Some(expected_path.as_str()));
        let rendered = err.render(source.as_str(), Some("test"));
        assert!(rendered.contains(expected_path.as_str()), "{rendered}");
        assert!(rendered.contains("xs[2]"), "{rendered}");
        let _ = fs::remove_file(path);
    }

    #[test]
    fn record_dot_access_reads_and_writes_map_fields() {
        assert_string(
            r#"
            let user = {"name": "tom"};
            user.name = user.name + "-cat";
            user.city = "shanghai";
            let result = user.name + "@" + user.city;
            if true { result }
            "#,
            "tom-cat@shanghai",
        );
    }

    #[test]
    fn record_shorthand_keys_are_strings() {
        assert_string(
            r#"
            let user = {name: "tom", city: "shanghai"};
            let result = user.name + "@" + user["city"];
            if true { result }
            "#,
            "tom@shanghai",
        );
    }

    #[test]
    fn missing_record_field_returns_null() {
        match eval_input(
            r#"
            let user = {};
            let result = user.name == null;
            if true { result }
            "#,
        )
        .unwrap()
        {
            Object::Boolean(actual) => assert!(actual),
            other => panic!("expected boolean true, got {other}"),
        }
    }

    #[test]
    fn runtime_errors_include_call_stack() {
        let result = eval_input(
            r#"
            fn inner() {
                let xs = [1];
                xs[2]
            }
            fn outer() {
                inner();
            }
            outer();
            "#,
        );
        let err = match result {
            Ok(value) => panic!("expected runtime error, got {value}"),
            Err(err) => err,
        };
        let names = err
            .stack
            .iter()
            .map(|frame| frame.name.as_str())
            .collect::<Vec<_>>();
        assert_eq!(names, vec!["outer", "inner"]);
        let rendered = err.render("", Some("test"));
        assert!(rendered.contains("stack:"), "{rendered}");
        assert!(rendered.contains("at inner"), "{rendered}");
        assert!(rendered.contains("at outer"), "{rendered}");
    }

    #[test]
    fn boolean_operators_short_circuit() {
        match eval_input(
            r#"
            let called = false;
            fn mark() {
                called = true;
                true
            }
            let a = false && mark();
            let b = true || mark();
            let c = true && !false;
            if a == false && b == true && c == true && called == false {
                "ok"
            } else {
                "bad"
            }
            "#,
        )
        .unwrap()
        {
            Object::String(actual) => assert_eq!(actual, "ok"),
            other => panic!("expected string ok, got {other}"),
        }
    }

    #[test]
    fn no_result_calls_are_user_visible_null() {
        match eval_input(
            r#"
            import time from "stdlib/time.monkey";
            let values = [];
            let append_result = append(values, 1);
            let assert_result = assert(true, "ok");
            let sleep_result = time.sleep_ms(0);
            fn no_result() {
                let x = 1;
            }
            let function_result = no_result();
            let deleted = delete({}, "missing");
            if append_result == null &&
                assert_result == null &&
                sleep_result == null &&
                function_result == null &&
                deleted == null &&
                type(sleep_result) == "null" {
                "ok"
            } else {
                "bad"
            }
            "#,
        )
        .unwrap()
        {
            Object::String(actual) => assert_eq!(actual, "ok"),
            other => panic!("expected string ok, got {other}"),
        }
    }

    #[test]
    fn stdlib_fs_and_path_export_file_builtins() {
        let dir = temp_dir_path("monkey_stdlib_fs");
        let file = std::path::Path::new(dir.as_str()).join("a.txt");
        let copy = std::path::Path::new(dir.as_str()).join("b.txt");
        let renamed = std::path::Path::new(dir.as_str()).join("c.txt");
        let source = r#"
            import fs from "stdlib/fs.monkey";
            import path from "stdlib/path.monkey";

            fs.mkdir("__DIR__");
            fs.write_file("__FILE__", "hello");
            let copied = fs.copy_file("__FILE__", "__COPY__");
            fs.rename("__COPY__", "__RENAMED__");
            let entries = fs.read_dir("__DIR__");
            let ok = copied.value == 5 &&
                fs.file_exists("__FILE__") &&
                path.path_exists("__RENAMED__") &&
                path.path_is_file("__RENAMED__") &&
                path.path_is_dir("__DIR__") &&
                path.path_basename("__RENAMED__") == "c.txt" &&
                path.path_ext("__RENAMED__") == "txt" &&
                entries.value[0] == "a.txt" &&
                entries.value[1] == "c.txt";
            fs.remove_file("__FILE__");
            fs.remove_file("__RENAMED__");
            if ok { "ok" } else { "bad" }
            "#
        .replace("__DIR__", monkey_string_content(dir.as_str()).as_str())
        .replace(
            "__FILE__",
            monkey_string_content(file.to_string_lossy().as_ref()).as_str(),
        )
        .replace(
            "__COPY__",
            monkey_string_content(copy.to_string_lossy().as_ref()).as_str(),
        )
        .replace(
            "__RENAMED__",
            monkey_string_content(renamed.to_string_lossy().as_ref()).as_str(),
        );
        assert_string(source.as_str(), "ok");
        let _ = fs::remove_dir(dir);
    }

    #[test]
    fn stdlib_env_math_and_time_export_builtins() {
        assert_string(
            r#"
            import env from "stdlib/env.monkey";
            import math from "stdlib/math.monkey";
            import time from "stdlib/time.monkey";

            env.env_set("MONKEY_RS_TEST_STDLIB_ENV", "value");
            let ok = env.env_get("MONKEY_RS_TEST_STDLIB_ENV") == "value" &&
                env.env_get("MONKEY_RS_TEST_STDLIB_MISSING") == null &&
                len(env.cwd().value) > 0 &&
                math.abs(-3) == 3 &&
                math.floor(1.9) == 1.0 &&
                math.ceil(1.1) == 2.0 &&
                math.round(1.5) == 2.0 &&
                math.sqrt(9) == 3.0 &&
                math.pow(2, 3) == 8.0 &&
                math.min(3, 1, 2) == 1 &&
                math.max(3.5, 1, 2) == 3.5 &&
                time.now_ms() > 0 &&
                type(time.sleep_ms(0)) == "null";
            if ok { "ok" } else { "bad" }
            "#,
            "ok",
        );
    }

    #[test]
    #[ignore = "requires binding a local TCP port"]
    fn stdlib_http_request_sends_headers_and_returns_response_map() {
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        let server = thread::spawn(move || {
            let (mut stream, _) = listener.accept().unwrap();
            let mut buffer = [0_u8; 4096];
            let bytes = stream.read(&mut buffer).unwrap();
            let request = String::from_utf8_lossy(&buffer[..bytes]);
            let header_seen = request.contains("x-monkey-token: banana");
            let body = if header_seen {
                "pong"
            } else {
                "missing-header"
            };
            stream
                .write_all(
                    format!("HTTP/1.0 201 Created\r\nContent-Type: text/plain\r\n\r\n{body}")
                        .as_bytes(),
                )
                .unwrap();
        });
        let source = format!(
            r#"
            import http from "stdlib/http.monkey";
            let res = http.http_request("GET", "http://127.0.0.1:{port}/ping", "", {{"x-monkey-token": "banana"}});
            str(res.value.status) + ":" + res.value.body + ":" + res.value.headers["content-type"]
            "#
        );
        assert_string(source.as_str(), "201:pong:text/plain");
        server.join().unwrap();
    }

    #[test]
    fn http_headers_must_be_map() {
        let result = eval_input(
            r#"
            import http from "stdlib/http.monkey";
            http.http_get("http://127.0.0.1", ["bad"])
            "#,
        );
        let err = match result {
            Ok(value) => panic!("expected header type error, got {value}"),
            Err(err) => err,
        };
        assert_eq!(err.kind, ErrorKind::Type);
        assert!(err.msg.contains("headers must be map"), "{}", err.msg);
    }

    #[test]
    fn system_utility_builtins_are_available_from_stdlib() {
        let dir = temp_dir_path("monkey_system_builtins");
        let file = std::path::Path::new(dir.as_str()).join("data.txt");
        let source = r#"
            import "stdlib/prelude.monkey";
            import process from "stdlib/process.monkey";
            import enc from "stdlib/encoding.monkey";
            import rnd from "stdlib/random.monkey";
            import fs from "stdlib/fs.monkey";
            import terminal from "stdlib/terminal.monkey";

            fs.mkdir("__DIR__");
            fs.write_file("__FILE__", "hello");
            let meta = fs.metadata("__FILE__");
            meta = meta.value;
            let sorted = sort(["b", "a", "c"]);
            let command = __ECHO_EXEC__;
            let ok = meta.exists &&
                meta.is_file &&
                meta.size == 5 &&
                join(sorted, ",") == "a,b,c" &&
                enc.base64_decode(enc.base64_encode("abc")).value == "abc" &&
                enc.url_decode(enc.url_encode("a b")).value == "a b" &&
                enc.sha256("abc") == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" &&
                rnd.random_int(1, 1) == 1 &&
                rnd.random_float() >= 0.0 &&
                rnd.random_float() < 1.0 &&
                type(terminal.clear) == "builtin" &&
                type(terminal.home) == "builtin" &&
                type(terminal.hide_cursor) == "builtin" &&
                type(terminal.show_cursor) == "builtin" &&
                type(terminal.enable_raw_mode) == "builtin" &&
                type(terminal.disable_raw_mode) == "builtin" &&
                type(terminal.read_key) == "builtin" &&
                type(terminal.read_key_timeout) == "builtin" &&
                type(terminal.read_key_latest_timeout) == "builtin" &&
                type(terminal.move) == "builtin" &&
                type(terminal.clear_line) == "builtin" &&
                type(terminal.size) == "builtin" &&
                type(terminal.enter_alt_screen) == "builtin" &&
                type(terminal.leave_alt_screen) == "builtin" &&
                type(terminal.fg) == "builtin" &&
                type(terminal.bg) == "builtin" &&
                type(terminal.bold) == "builtin" &&
                type(terminal.reset_style) == "builtin" &&
                type(terminal.paint) == "builtin" &&
                type(terminal.paint_runs) == "builtin" &&
                command.ok &&
                command.value.success &&
                find(command.value.stdout, "monkey") == 0;
            fs.remove_file("__FILE__");
            fs.remove_dir("__DIR__");
            if ok { "ok" } else { "bad" }
            "#
        .replace("__DIR__", monkey_string_content(dir.as_str()).as_str())
        .replace(
            "__FILE__",
            monkey_string_content(file.to_string_lossy().as_ref()).as_str(),
        )
        .replace("__ECHO_EXEC__", echo_exec_expression());
        assert_string(source.as_str(), "ok");
    }

    #[test]
    fn module_builtins_are_not_global_names() {
        let err = match eval_input("exec(\"monkey-missing-global\", []);") {
            Ok(value) => panic!("expected missing global exec, got {value}"),
            Err(err) => err,
        };
        assert_eq!(err.kind, ErrorKind::Name);

        let err = match eval_input("json_stringify({});") {
            Ok(value) => panic!("expected missing global json_stringify, got {value}"),
            Err(err) => err,
        };
        assert_eq!(err.kind, ErrorKind::Name);

        let err = match eval_input("clear();") {
            Ok(value) => panic!("expected missing global clear, got {value}"),
            Err(err) => err,
        };
        assert_eq!(err.kind, ErrorKind::Name);
    }

    #[test]
    fn prelude_does_not_export_system_builtins() {
        let err = match eval_input(
            r#"
            import "stdlib/prelude.monkey";
            read_file("missing.txt")
            "#,
        ) {
            Ok(value) => panic!("expected missing prelude read_file, got {value}"),
            Err(err) => err,
        };
        assert_eq!(err.kind, ErrorKind::Name);
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

    #[test]
    fn null_is_a_regular_value() {
        assert_string(
            r#"
            let values = [null, 1];
            let data = {"missing": null};
            let ok = values[0] == null;
            let text = str(values[0]);
            if ok {
                text
            } else {
                "bad"
            }
            "#,
            "null",
        );
    }

    #[test]
    fn missing_map_key_returns_null() {
        assert_integer(
            r#"
            let data = {"name": "tom"};
            if data["age"] == null {
                1
            } else {
                0
            }
            "#,
            1,
        );
    }

    #[test]
    fn null_condition_is_a_type_error() {
        let result = eval_input(
            r#"
            if null {
                1
            } else {
                0
            }
            "#,
        );
        let err = match result {
            Ok(value) => panic!("expected null condition error, got {value}"),
            Err(err) => err,
        };
        assert_eq!(err.msg, "operation type invalid");
    }

    #[test]
    fn json_stdlib_parses_nested_values() {
        let source = r#"
            import "stdlib/json.monkey";
            let data = json_parse("{\"name\":\"tom\",\"age\":12,\"active\":true,\"tags\":[\"a\",null,3]}");
            data = data.value;
            let ok = data.name == "tom";
            if ok {
                ok = data.age == 12;
            }
            if ok {
                ok = data.active == true;
            }
            if ok {
                ok = data.tags[1] == null;
            }
            if ok {
                ok = data.tags[2] == 3;
            }
            if true { ok }
            "#;

        match eval_input(source).unwrap() {
            Object::Boolean(actual) => assert!(actual),
            other => panic!("expected boolean true, got {other}"),
        }
    }

    #[test]
    fn json_stdlib_stringifies_values() {
        let source = r#"
            import "stdlib/json.monkey";
            struct User { name; age; }
            let data = {
                name: "tom",
                active: true,
                tags: ["a", null, 3],
                nested: User{name: "ann", age: 9}
            };
            json_stringify(data)
            "#;

        match eval_input(source).unwrap() {
            Object::String(actual) => assert_eq!(
                actual,
                "{\"active\":true,\"name\":\"tom\",\"nested\":{\"name\":\"ann\",\"age\":9},\"tags\":[\"a\",null,3]}"
            ),
            other => panic!("expected json string, got {other}"),
        }
    }

    #[test]
    fn json_stringify_rejects_cycles() {
        let result = eval_input(
            r#"
            import "stdlib/json.monkey";
            let values = [];
            append(values, values);
            json_stringify(values)
            "#,
        );
        let err = match result {
            Ok(value) => panic!("expected cycle error, got {value}"),
            Err(err) => err,
        };
        assert_eq!(err.kind, ErrorKind::Type);
        assert!(err.msg.contains("cyclic"), "{}", err.msg);
    }

    #[test]
    fn json_parse_accepts_stringify_escape_sequences() {
        let source = r#"
            import "stdlib/json.monkey";
            let text = "a" + from_char_code(8) + from_char_code(12) + from_char_code(1);
            let encoded = json_stringify({text: text});
            let decoded = json_parse(encoded);
            let smile = json_parse("\"\\uD83D\\uDE00\"");
            decoded.value.text == text && smile.value == from_char_code(128512)
            "#;

        match eval_input(source).unwrap() {
            Object::Boolean(actual) => assert!(actual),
            other => panic!("expected boolean true, got {other}"),
        }
    }
}
