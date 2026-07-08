use super::env::{set_vm_env_slot_value, vm_env_at_depth, VmEnvHandle};
use super::heap::VmHeap;
use super::runtime::value::Value;
use crate::error::{Error, ErrorKind};
use crate::lexer::Span;

pub(super) fn get_vm_slot(
    heap: &VmHeap,
    env: VmEnvHandle,
    depth: usize,
    slot: usize,
    span: Span,
) -> Result<Value, Error> {
    let env = vm_env_at_depth(heap, env, depth).ok_or_else(|| {
        Error::with_kind_span(
            ErrorKind::Runtime,
            "vm slot depth out of bound".to_string(),
            span,
        )
    })?;
    let value = {
        let env = heap.env(env);
        env.upvalue_cells
            .get(slot)
            .and_then(|cell| *cell)
            .map(|cell| heap.cell(cell))
            .or_else(|| env.slots.get(slot).cloned())
    };
    value.ok_or_else(|| {
        Error::with_kind_span(ErrorKind::Runtime, "vm slot out of bound".to_string(), span)
    })
}

pub(super) fn set_vm_slot(
    heap: &mut VmHeap,
    env: VmEnvHandle,
    depth: usize,
    slot: usize,
    value: Value,
    span: Span,
) -> Result<(), Error> {
    let env = vm_env_at_depth(heap, env, depth).ok_or_else(|| {
        Error::with_kind_span(
            ErrorKind::Runtime,
            "vm slot depth out of bound".to_string(),
            span,
        )
    })?;
    if heap.env(env).slots.get(slot).is_none() {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "vm slot out of bound".to_string(),
            span,
        ));
    }
    set_vm_env_slot_value(heap, env, slot, value);
    Ok(())
}
