use super::env::{define_vm_slot_binding, sync_locals_to_env, VmEnv, VmEnvHandle};
use super::heap::VmHeap;
use super::program::CompiledFunction;
use super::register::RegisterInstruction;
use super::runtime::ops::{eval_infix_value, operation_type_error};
use super::runtime::value::{Value, VmCellHandle};
use crate::error::{Error, ErrorKind};
use crate::lexer::Span;
use crate::token::Token;
use std::rc::Rc;

pub(super) struct ForInState {
    pub(super) entries: Vec<(Value, Option<Value>)>,
    pub(super) index: usize,
    pub(super) parent_env: VmEnvHandle,
}

pub(super) struct VmFrame {
    pub(super) function: Rc<CompiledFunction>,
    pub(super) env: VmEnvHandle,
    pub(super) local_base: usize,
    pub(super) local_count: usize,
    pub(super) locals_synced: bool,
    pub(super) upvalues: Rc<[VmCellHandle]>,
    pub(super) register_base: usize,
    pub(super) register_count: usize,
    pub(super) ip: usize,
    pub(super) return_register: Option<usize>,
    pub(super) last: Value,
}

pub(super) fn can_reuse_closure_env_for_call(function: &CompiledFunction) -> bool {
    function.local_count == 0
        && function.parameters.is_empty()
        && !function.registers.sync_env_on_exit
        && function
            .registers
            .instructions
            .iter()
            .all(instruction_preserves_reused_closure_env)
}

fn instruction_preserves_reused_closure_env(instruction: &RegisterInstruction) -> bool {
    !matches!(
        instruction,
        RegisterInstruction::MakeFunction { .. }
            | RegisterInstruction::GetSlot { .. }
            | RegisterInstruction::SetSlot { .. }
            | RegisterInstruction::UpdateSlotInfix { .. }
            | RegisterInstruction::SetName { .. }
            | RegisterInstruction::UpdateNameInfix { .. }
            | RegisterInstruction::DefineStruct { .. }
            | RegisterInstruction::Import { .. }
            | RegisterInstruction::Export { .. }
            | RegisterInstruction::EnterScope { .. }
            | RegisterInstruction::ForInNext { .. }
    )
}

pub(super) fn frame_locals<'a>(frame: &VmFrame, local_stack: &'a [Value]) -> &'a [Value] {
    let start = frame.local_base;
    let end = start + frame.local_count;
    &local_stack[start..end]
}

fn frame_locals_mut<'a>(frame: &VmFrame, local_stack: &'a mut [Value]) -> &'a mut [Value] {
    let start = frame.local_base;
    let end = start + frame.local_count;
    &mut local_stack[start..end]
}

pub(super) fn get_frame_local(
    frame: &VmFrame,
    local_stack: &[Value],
    slot: usize,
    span: Span,
) -> Result<Value, Error> {
    frame_locals(frame, local_stack)
        .get(slot)
        .cloned()
        .ok_or_else(|| {
            Error::with_kind_span(ErrorKind::Runtime, "vm slot out of bound".to_string(), span)
        })
}

pub(super) fn replace_frame_locals(
    frame: &mut VmFrame,
    local_stack: &mut Vec<Value>,
    locals: Vec<Value>,
) {
    local_stack.truncate(frame.local_base);
    frame.local_count = locals.len();
    local_stack.extend(locals);
    frame.locals_synced = true;
}

pub(super) fn reset_frame_locals(
    frame: &mut VmFrame,
    local_stack: &mut Vec<Value>,
    local_count: usize,
) {
    local_stack.truncate(frame.local_base);
    frame.local_count = local_count;
    local_stack.resize(frame.local_base + local_count, Value::Empty);
    frame.locals_synced = true;
}

pub(super) fn get_register(
    frame: &VmFrame,
    register_stack: &[Value],
    register: usize,
    span: Span,
) -> Result<Value, Error> {
    if register >= frame.register_count {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "vm register out of bound".to_string(),
            span,
        ));
    }
    let index = frame.register_base + register;
    // SAFETY: register was checked against frame.register_count above and
    // register_stack windows are resized when frames are pushed.
    Ok(unsafe { register_stack.get_unchecked(index).clone() })
}

pub(super) fn read_compiled_register(
    frame: &VmFrame,
    register_stack: &[Value],
    register: usize,
) -> Value {
    debug_assert!(register < frame.register_count);
    let index = frame.register_base + register;
    debug_assert!(index < register_stack.len());
    // SAFETY: register operands are emitted by the VM compiler for the current
    // frame's register window. Debug assertions keep invalid bytecode visible
    // during development without charging the release hot path.
    unsafe { register_stack.get_unchecked(index).clone() }
}

pub(super) fn collect_builtin_args_from_registers(
    frame: &VmFrame,
    register_stack: &[Value],
    args: &[usize],
) -> Vec<Value> {
    args.iter()
        .map(|register| read_compiled_register(frame, register_stack, *register))
        .collect()
}

pub(super) fn set_register(
    frame: &mut VmFrame,
    register_stack: &mut [Value],
    register: usize,
    value: Value,
    span: Span,
) -> Result<(), Error> {
    if register >= frame.register_count {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "vm register out of bound".to_string(),
            span,
        ));
    }
    let index = frame.register_base + register;
    // SAFETY: register was checked against frame.register_count above and
    // register_stack windows are resized when frames are pushed.
    unsafe {
        *register_stack.get_unchecked_mut(index) = value;
    }
    Ok(())
}

pub(super) fn set_register_last(
    frame: &mut VmFrame,
    register_stack: &mut [Value],
    register: usize,
    value: Value,
    span: Span,
) -> Result<(), Error> {
    set_register(frame, register_stack, register, value.clone(), span)?;
    frame.last = value;
    Ok(())
}

pub(super) fn set_optional_register_or_empty(
    frame: &mut VmFrame,
    register_stack: &mut [Value],
    register: Option<usize>,
    value: Value,
    span: Span,
) -> Result<(), Error> {
    if let Some(register) = register {
        set_register_last(frame, register_stack, register, value, span)?;
    } else {
        frame.last = Value::Empty;
    }
    Ok(())
}

pub(super) fn sync_frame_locals_to_env_if_needed(
    heap: &mut VmHeap,
    frame: &VmFrame,
    local_stack: &[Value],
) {
    if !frame.locals_synced
        && (frame.function.registers.sync_env_on_exit || heap.env(frame.env).captured)
    {
        sync_locals_to_env(heap, frame.env, frame_locals(frame, local_stack));
    }
}

pub(super) fn sync_frame_locals_to_env_if_dirty(
    heap: &mut VmHeap,
    frame: &mut VmFrame,
    local_stack: &[Value],
) {
    if !frame.locals_synced {
        sync_locals_to_env(heap, frame.env, frame_locals(frame, local_stack));
        frame.locals_synced = true;
    }
}

pub(super) fn refresh_frame_locals_from_env(
    heap: &VmHeap,
    frame: &mut VmFrame,
    local_stack: &mut Vec<Value>,
) {
    let locals = heap.env(frame.env).slots.clone();
    replace_frame_locals(frame, local_stack, locals);
}

pub(super) fn refresh_captured_frame_locals(
    heap: &VmHeap,
    frame: &mut VmFrame,
    local_stack: &mut Vec<Value>,
) {
    if heap.env(frame.env).captured {
        refresh_frame_locals_from_env(heap, frame, local_stack);
    }
}

fn sync_frame_local_capture(heap: &mut VmHeap, frame: &VmFrame, slot: usize, value: Value) {
    if heap.env(frame.env).captured {
        let env = heap.env_mut(frame.env);
        env.slots[slot] = value.clone();
        if let Some(cell) = env.upvalue_cells.get(slot).and_then(|cell| *cell) {
            heap.set_cell(cell, value);
        }
    }
}

pub(super) fn set_frame_local(
    heap: &mut VmHeap,
    frame: &mut VmFrame,
    local_stack: &mut [Value],
    slot: usize,
    value: Value,
    span: Span,
) -> Result<(), Error> {
    if slot >= frame.local_count {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "vm slot out of bound".to_string(),
            span,
        ));
    }
    // SAFETY: slot was checked against frame.local_count above.
    unsafe {
        *frame_locals_mut(frame, local_stack).get_unchecked_mut(slot) = value.clone();
    }
    frame.locals_synced = false;
    sync_frame_local_capture(heap, frame, slot, value);
    Ok(())
}

pub(super) fn update_frame_local_int(
    heap: &mut VmHeap,
    frame: &mut VmFrame,
    local_stack: &mut [Value],
    slot: usize,
    token: &Token,
    operand: i64,
    span: Span,
) -> Result<Value, Error> {
    if slot >= frame.local_count {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "vm slot out of bound".to_string(),
            span,
        ));
    }
    // SAFETY: slot was checked against frame.local_count above.
    let target = unsafe { frame_locals(frame, local_stack).get_unchecked(slot).clone() };
    if let Value::Integer(left) = target {
        let value = match token {
            Token::Plus => left + operand,
            Token::Minus => left - operand,
            Token::Asterisk => left * operand,
            Token::Slash => {
                if operand == 0 {
                    return Err(Error::with_kind_span(
                        ErrorKind::Runtime,
                        "division by zero".to_string(),
                        span,
                    ));
                }
                left / operand
            }
            _ => return Err(operation_type_error(span)),
        };
        let value = Value::Integer(value);
        // SAFETY: slot was checked against frame.local_count above.
        unsafe {
            *frame_locals_mut(frame, local_stack).get_unchecked_mut(slot) = value.clone();
        }
        frame.locals_synced = false;
        sync_frame_local_capture(heap, frame, slot, value.clone());
        return Ok(value);
    }
    let value = eval_infix_value(heap, token, target, Value::Integer(operand), span)?;
    // SAFETY: slot was checked against frame.local_count above.
    unsafe {
        *frame_locals_mut(frame, local_stack).get_unchecked_mut(slot) = value.clone();
    }
    frame.locals_synced = false;
    sync_frame_local_capture(heap, frame, slot, value.clone());
    Ok(value)
}

pub(super) fn update_frame_local_int_no_result(
    heap: &mut VmHeap,
    frame: &mut VmFrame,
    local_stack: &mut [Value],
    slot: usize,
    token: &Token,
    operand: i64,
    span: Span,
) -> Result<(), Error> {
    if slot >= frame.local_count {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "vm slot out of bound".to_string(),
            span,
        ));
    }
    let should_sync_capture = heap.env(frame.env).captured;
    let sync_value = {
        // SAFETY: slot was checked against frame.local_count above.
        let target = unsafe { frame_locals(frame, local_stack).get_unchecked(slot).clone() };
        if let Value::Integer(left) = target {
            let value = match token {
                Token::Plus => left + operand,
                Token::Minus => left - operand,
                Token::Asterisk => left * operand,
                Token::Slash => {
                    if operand == 0 {
                        return Err(Error::with_kind_span(
                            ErrorKind::Runtime,
                            "division by zero".to_string(),
                            span,
                        ));
                    }
                    left / operand
                }
                _ => return Err(operation_type_error(span)),
            };
            // SAFETY: slot was checked against frame.local_count above.
            unsafe {
                *frame_locals_mut(frame, local_stack).get_unchecked_mut(slot) =
                    Value::Integer(value);
            }
            should_sync_capture.then_some(Value::Integer(value))
        } else {
            let value = eval_infix_value(heap, token, target, Value::Integer(operand), span)?;
            // SAFETY: slot was checked against frame.local_count above.
            unsafe {
                *frame_locals_mut(frame, local_stack).get_unchecked_mut(slot) = value.clone();
            }
            should_sync_capture.then_some(value)
        }
    };
    frame.locals_synced = false;
    if let Some(value) = sync_value {
        sync_frame_local_capture(heap, frame, slot, value);
    }
    frame.last = Value::Empty;
    Ok(())
}

pub(super) fn update_frame_local_from_local(
    heap: &mut VmHeap,
    frame: &mut VmFrame,
    local_stack: &mut [Value],
    slot: usize,
    token: &Token,
    operand_slot: usize,
    span: Span,
) -> Result<Value, Error> {
    if slot >= frame.local_count || operand_slot >= frame.local_count {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "vm slot out of bound".to_string(),
            span,
        ));
    }
    // SAFETY: Both slots were checked against frame.local_count above.
    let left = unsafe { frame_locals(frame, local_stack).get_unchecked(slot).clone() };
    // SAFETY: Both slots were checked against frame.local_count above.
    let right = unsafe {
        frame_locals(frame, local_stack)
            .get_unchecked(operand_slot)
            .clone()
    };
    let left_int = vm_register_int_value(&left);
    let right_int = vm_register_int_value(&right);
    if let (Some(left), Some(right)) = (left_int, right_int) {
        let value = match token {
            Token::Plus => left + right,
            Token::Minus => left - right,
            Token::Asterisk => left * right,
            Token::Slash => {
                if right == 0 {
                    return Err(Error::with_kind_span(
                        ErrorKind::Runtime,
                        "division by zero".to_string(),
                        span,
                    ));
                }
                left / right
            }
            _ => return Err(operation_type_error(span)),
        };
        let value = Value::Integer(value);
        // SAFETY: slot was checked against frame.local_count above.
        unsafe {
            *frame_locals_mut(frame, local_stack).get_unchecked_mut(slot) = value.clone();
        }
        frame.locals_synced = false;
        sync_frame_local_capture(heap, frame, slot, value.clone());
        return Ok(value);
    }
    let value = eval_infix_value(heap, token, left, right, span)?;
    // SAFETY: slot was checked against frame.local_count above.
    unsafe {
        *frame_locals_mut(frame, local_stack).get_unchecked_mut(slot) = value.clone();
    }
    frame.locals_synced = false;
    sync_frame_local_capture(heap, frame, slot, value.clone());
    Ok(value)
}

pub(super) fn update_frame_local_from_local_no_result(
    heap: &mut VmHeap,
    frame: &mut VmFrame,
    local_stack: &mut [Value],
    slot: usize,
    token: &Token,
    operand_slot: usize,
    span: Span,
) -> Result<(), Error> {
    if slot >= frame.local_count || operand_slot >= frame.local_count {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "vm slot out of bound".to_string(),
            span,
        ));
    }
    let should_sync_capture = heap.env(frame.env).captured;
    // SAFETY: Both slots were checked against frame.local_count above.
    let left = unsafe { frame_locals(frame, local_stack).get_unchecked(slot).clone() };
    // SAFETY: Both slots were checked against frame.local_count above.
    let right = unsafe {
        frame_locals(frame, local_stack)
            .get_unchecked(operand_slot)
            .clone()
    };
    let left_int = vm_register_int_value(&left);
    let right_int = vm_register_int_value(&right);
    let sync_value = if let (Some(left), Some(right)) = (left_int, right_int) {
        let value = match token {
            Token::Plus => left + right,
            Token::Minus => left - right,
            Token::Asterisk => left * right,
            Token::Slash => {
                if right == 0 {
                    return Err(Error::with_kind_span(
                        ErrorKind::Runtime,
                        "division by zero".to_string(),
                        span,
                    ));
                }
                left / right
            }
            _ => return Err(operation_type_error(span)),
        };
        // SAFETY: slot was checked against frame.local_count above.
        unsafe {
            *frame_locals_mut(frame, local_stack).get_unchecked_mut(slot) = Value::Integer(value);
        }
        should_sync_capture.then_some(Value::Integer(value))
    } else {
        let value = eval_infix_value(heap, token, left, right, span)?;
        // SAFETY: slot was checked against frame.local_count above.
        unsafe {
            *frame_locals_mut(frame, local_stack).get_unchecked_mut(slot) = value.clone();
        }
        should_sync_capture.then_some(value)
    };
    frame.locals_synced = false;
    if let Some(value) = sync_value {
        sync_frame_local_capture(heap, frame, slot, value);
    }
    frame.last = Value::Empty;
    Ok(())
}

pub(super) fn vm_register_int_value(value: &Value) -> Option<i64> {
    match value {
        Value::Integer(value) => Some(*value),
        _ => None,
    }
}

pub(super) fn eval_register_int_int_infix(
    left: i64,
    token: &Token,
    right: i64,
    span: Span,
) -> Result<Value, Error> {
    match token {
        Token::Plus => Ok(Value::Integer(left + right)),
        Token::Minus => Ok(Value::Integer(left - right)),
        Token::Asterisk => Ok(Value::Integer(left * right)),
        Token::Slash => {
            if right == 0 {
                Err(Error::with_kind_span(
                    ErrorKind::Runtime,
                    "division by zero".to_string(),
                    span,
                ))
            } else {
                Ok(Value::Integer(left / right))
            }
        }
        Token::EQ => Ok(Value::Boolean(left == right)),
        Token::NotEq => Ok(Value::Boolean(left != right)),
        Token::LT => Ok(Value::Boolean(left < right)),
        Token::Lte => Ok(Value::Boolean(left <= right)),
        Token::GT => Ok(Value::Boolean(left > right)),
        Token::Gte => Ok(Value::Boolean(left >= right)),
        _ => Err(operation_type_error(span)),
    }
}

pub(super) fn eval_local_int_infix_from_values(
    heap: &mut VmHeap,
    locals: &[Value],
    slot: usize,
    token: &Token,
    operand: i64,
    span: Span,
) -> Result<Value, Error> {
    let left = locals.get(slot).ok_or_else(|| {
        Error::with_kind_span(ErrorKind::Runtime, "vm slot out of bound".to_string(), span)
    })?;
    if let Some(left) = vm_register_int_value(left) {
        return eval_register_int_int_infix(left, token, operand, span);
    }
    eval_infix_value(heap, token, left.clone(), Value::Integer(operand), span)
}

pub(super) fn compare_frame_local_int(
    heap: &mut VmHeap,
    locals: &[Value],
    slot: usize,
    token: &Token,
    operand: i64,
    span: Span,
) -> Result<Value, Error> {
    compare_frame_local_int_bool(heap, locals, slot, token, operand, span).map(Value::Boolean)
}

pub(super) fn compare_frame_local_int_bool(
    heap: &mut VmHeap,
    locals: &[Value],
    slot: usize,
    token: &Token,
    operand: i64,
    span: Span,
) -> Result<bool, Error> {
    if slot >= locals.len() {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "vm slot out of bound".to_string(),
            span,
        ));
    }
    // SAFETY: slot was checked against locals.len() above.
    let left = unsafe { locals.get_unchecked(slot) };
    if let Some(left) = vm_register_int_value(left) {
        let value = match token {
            Token::EQ => left == operand,
            Token::NotEq => left != operand,
            Token::LT => left < operand,
            Token::Lte => left <= operand,
            Token::GT => left > operand,
            Token::Gte => left >= operand,
            _ => return Err(operation_type_error(span)),
        };
        return Ok(value);
    }
    match eval_infix_value(heap, token, left.clone(), Value::Integer(operand), span)? {
        Value::Boolean(value) => Ok(value),
        _ => Err(operation_type_error(span)),
    }
}

pub(super) fn compare_frame_local_local_bool(
    heap: &mut VmHeap,
    locals: &[Value],
    left_slot: usize,
    token: &Token,
    right_slot: usize,
    span: Span,
) -> Result<bool, Error> {
    if left_slot >= locals.len() || right_slot >= locals.len() {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "vm slot out of bound".to_string(),
            span,
        ));
    }
    // SAFETY: Both slots were checked against locals.len() above.
    let left = unsafe { locals.get_unchecked(left_slot) };
    // SAFETY: Both slots were checked against locals.len() above.
    let right = unsafe { locals.get_unchecked(right_slot) };
    if let (Some(left), Some(right)) = (vm_register_int_value(left), vm_register_int_value(right)) {
        let value = match token {
            Token::EQ => left == right,
            Token::NotEq => left != right,
            Token::LT => left < right,
            Token::Lte => left <= right,
            Token::GT => left > right,
            Token::Gte => left >= right,
            _ => return Err(operation_type_error(span)),
        };
        return Ok(value);
    }
    match eval_infix_value(heap, token, left.clone(), right.clone(), span)? {
        Value::Boolean(value) => Ok(value),
        _ => Err(operation_type_error(span)),
    }
}

pub(super) fn leave_register_scope_depth(
    heap: &mut VmHeap,
    frame: &mut VmFrame,
    local_stack: &mut Vec<Value>,
    depth: usize,
    span: Span,
) -> Result<(), Error> {
    for _ in 0..depth {
        sync_frame_locals_to_env_if_dirty(heap, frame, local_stack);
        let prev = heap.env(frame.env).prev.ok_or_else(|| {
            Error::with_kind_span(
                ErrorKind::Runtime,
                "vm scope depth out of bound".to_string(),
                span,
            )
        })?;
        let locals = heap.env(prev).slots.clone();
        frame.env = prev;
        replace_frame_locals(frame, local_stack, locals);
    }
    Ok(())
}

pub(super) fn leave_register_for_in_iteration(
    heap: &mut VmHeap,
    frame: &mut VmFrame,
    local_stack: &mut Vec<Value>,
    state: Option<&ForInState>,
    span: Span,
) -> Result<(), Error> {
    let Some(state) = state else {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "for-in state underflow".to_string(),
            span,
        ));
    };
    while frame.env != state.parent_env {
        sync_frame_locals_to_env_if_dirty(heap, frame, local_stack);
        let prev = heap.env(frame.env).prev.ok_or_else(|| {
            Error::with_kind_span(
                ErrorKind::Runtime,
                "for-in scope underflow".to_string(),
                span,
            )
        })?;
        let locals = heap.env(prev).slots.clone();
        frame.env = prev;
        replace_frame_locals(frame, local_stack, locals);
    }
    Ok(())
}

pub(super) fn prepare_register_call_frame_from_registers(
    heap: &mut VmHeap,
    function: &CompiledFunction,
    env: VmEnvHandle,
    caller: &VmFrame,
    register_stack: &[Value],
    args: &[usize],
    span: Span,
) -> Result<(VmEnvHandle, Vec<Value>), Error> {
    if function.parameters.len() != args.len() {
        return Err(Error::with_kind(
            ErrorKind::Runtime,
            "the number of parameter not match".to_string(),
        ));
    }
    let call_env = VmEnv::alloc_child(heap, env, function.local_count);
    let mut locals = vec![Value::Empty; function.local_count];
    for ((parameter, slot), argument) in function
        .parameters
        .iter()
        .zip(function.parameter_slots.iter())
        .zip(args.iter().copied())
    {
        define_vm_slot_binding(heap, call_env, *parameter, *slot);
        locals[*slot] = get_register(caller, register_stack, argument, span)?;
    }
    Ok((call_env, locals))
}
