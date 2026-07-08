use super::heap::VmHeap;
use super::program::UpvalueSpec;
use super::runtime::value::{Value, VmCellHandle, VmModule};
use super::symbols::SymbolId;
use crate::error::{Error, ErrorKind};
use crate::lexer::Span;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(super) struct VmEnvHandle(pub(super) usize);

pub(super) struct VmEnv {
    pub(super) slots: Vec<Value>,
    pub(super) slot_bindings: HashMap<SymbolId, usize>,
    pub(super) store: HashMap<SymbolId, Value>,
    pub(super) exports: HashSet<SymbolId>,
    pub(super) upvalue_cells: Vec<Option<VmCellHandle>>,
    pub(super) captured: bool,
    pub(super) prev: Option<VmEnvHandle>,
}

impl VmEnv {
    pub(super) fn alloc_root(heap: &mut VmHeap, slot_count: usize) -> VmEnvHandle {
        heap.alloc_env(Self {
            slots: vec![Value::Empty; slot_count],
            slot_bindings: HashMap::new(),
            store: HashMap::new(),
            exports: HashSet::new(),
            upvalue_cells: vec![None; slot_count],
            captured: false,
            prev: None,
        })
    }

    pub(super) fn alloc_child(
        heap: &mut VmHeap,
        env: VmEnvHandle,
        slot_count: usize,
    ) -> VmEnvHandle {
        heap.alloc_env(Self {
            slots: vec![Value::Empty; slot_count],
            slot_bindings: HashMap::new(),
            store: HashMap::new(),
            exports: HashSet::new(),
            upvalue_cells: vec![None; slot_count],
            captured: false,
            prev: Some(env),
        })
    }
}

pub(super) fn define_vm_env(heap: &mut VmHeap, env: VmEnvHandle, name: SymbolId, value: Value) {
    heap.env_mut(env).store.insert(name, value);
}

pub(super) fn define_vm_slot_binding(
    heap: &mut VmHeap,
    env: VmEnvHandle,
    name: SymbolId,
    slot: usize,
) {
    heap.env_mut(env).slot_bindings.insert(name, slot);
}

pub(super) fn sync_locals_to_env(heap: &mut VmHeap, env: VmEnvHandle, locals: &[Value]) {
    heap.env_mut(env).slots.clone_from_slice(locals);
}

pub(super) fn mark_env_chain_captured(heap: &mut VmHeap, env: VmEnvHandle) {
    let mut current = Some(env);
    while let Some(env) = current {
        current = {
            let env = heap.env_mut(env);
            env.captured = true;
            env.prev
        };
    }
}

pub(super) fn capture_vm_upvalues(
    heap: &mut VmHeap,
    env: VmEnvHandle,
    specs: &[UpvalueSpec],
    span: Span,
) -> Result<Rc<[VmCellHandle]>, Error> {
    specs
        .iter()
        .map(|spec| capture_vm_upvalue(heap, env, spec.depth, spec.slot, span))
        .collect()
}

fn capture_vm_upvalue(
    heap: &mut VmHeap,
    env: VmEnvHandle,
    depth: usize,
    slot: usize,
    span: Span,
) -> Result<VmCellHandle, Error> {
    let target = vm_env_at_depth(heap, env, depth).ok_or_else(|| {
        Error::with_kind_span(
            ErrorKind::Runtime,
            "vm upvalue depth out of bound".to_string(),
            span,
        )
    })?;
    if let Some(cell) = heap
        .env(target)
        .upvalue_cells
        .get(slot)
        .and_then(|cell| *cell)
    {
        return Ok(cell);
    }
    let value = {
        let target = heap.env(target);
        target.slots.get(slot).cloned().ok_or_else(|| {
            Error::with_kind_span(
                ErrorKind::Runtime,
                format!(
                    "vm upvalue out of bound depth={depth} slot={slot} slots={}",
                    target.slots.len()
                ),
                span,
            )
        })
    }?;
    let cell = heap.alloc_cell(value);
    let target = heap.env_mut(target);
    let Some(target_slot) = target.upvalue_cells.get_mut(slot) else {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            format!(
                "vm upvalue out of bound depth={depth} slot={slot} slots={}",
                target.slots.len()
            ),
            span,
        ));
    };
    *target_slot = Some(cell);
    Ok(cell)
}

pub(super) fn has_current_vm_binding(heap: &VmHeap, env: VmEnvHandle, name: SymbolId) -> bool {
    let env = heap.env(env);
    env.store.contains_key(&name) || env.slot_bindings.contains_key(&name)
}

pub(super) fn from_vm_env(
    heap: &VmHeap,
    env: VmEnvHandle,
    name: SymbolId,
) -> Option<(Value, usize)> {
    let mut current = Some(env);
    let mut depth = 0;
    while let Some(env) = current {
        let borrowed = heap.env(env);
        if let Some(value) = borrowed.store.get(&name) {
            return Some((value.clone(), depth));
        }
        if let Some(slot) = borrowed.slot_bindings.get(&name) {
            if let Some(cell) = borrowed.upvalue_cells.get(*slot).and_then(|cell| *cell) {
                return Some((heap.cell(cell), depth));
            }
            return borrowed
                .slots
                .get(*slot)
                .cloned()
                .map(|value| (value, depth));
        }
        current = borrowed.prev;
        depth += 1;
    }
    None
}

pub(super) fn assign_vm_env(
    heap: &mut VmHeap,
    env: VmEnvHandle,
    name: SymbolId,
    value: Value,
) -> Option<usize> {
    let mut current = Some(env);
    let mut depth = 0;
    while let Some(env) = current {
        let slot = {
            let borrowed = heap.env(env);
            if borrowed.store.contains_key(&name) {
                None
            } else {
                borrowed.slot_bindings.get(&name).copied()
            }
        };
        if heap.env(env).store.contains_key(&name) {
            heap.env_mut(env).store.insert(name, value);
            return Some(depth);
        }
        if let Some(slot) = slot {
            set_vm_env_slot_value(heap, env, slot, value);
            return Some(depth);
        }
        current = heap.env(env).prev;
        depth += 1;
    }
    None
}

pub(super) fn set_vm_env_slot_value(
    heap: &mut VmHeap,
    env: VmEnvHandle,
    slot: usize,
    value: Value,
) {
    let env = heap.env_mut(env);
    env.slots[slot] = value.clone();
    if let Some(cell) = env.upvalue_cells.get(slot).and_then(|cell| *cell) {
        heap.set_cell(cell, value);
    }
}

pub(super) fn vm_env_at_depth(
    heap: &VmHeap,
    env: VmEnvHandle,
    depth: usize,
) -> Option<VmEnvHandle> {
    let mut current = env;
    for _ in 0..depth {
        let prev = heap.env(current).prev?;
        current = prev;
    }
    Some(current)
}

pub(super) fn mark_vm_export(heap: &mut VmHeap, env: VmEnvHandle, name: SymbolId) {
    heap.env_mut(env).exports.insert(name);
}

pub(super) fn vm_module_from_env(env: VmEnvHandle, heap: &mut VmHeap) -> Value {
    let exports = {
        let env = heap.env(env);
        env.exports
            .iter()
            .filter_map(|name| {
                let value = env.store.get(name).cloned().or_else(|| {
                    env.slot_bindings.get(name).and_then(|slot| {
                        env.upvalue_cells
                            .get(*slot)
                            .and_then(|cell| *cell)
                            .map(|cell| heap.cell(cell))
                            .or_else(|| env.slots.get(*slot).cloned())
                    })
                })?;
                Some((*name, value))
            })
            .collect::<Vec<_>>()
    };
    let mut exports = exports;
    exports.sort_by_key(|(name, _)| *name);
    let mut indexes = HashMap::new();
    let mut names = Vec::with_capacity(exports.len());
    let mut values = Vec::with_capacity(exports.len());
    for (index, (name, value)) in exports.into_iter().enumerate() {
        indexes.insert(name, index);
        names.push(name);
        values.push(value);
    }
    heap.alloc_module(VmModule {
        indexes,
        names,
        values,
    })
}

pub(super) fn inject_vm_module(
    heap: &mut VmHeap,
    env: VmEnvHandle,
    module: &Value,
) -> Result<(), Error> {
    let Value::Module(module) = module else {
        return Err(Error::with_kind(
            ErrorKind::Import,
            "cached module is not a module object".to_string(),
        ));
    };
    let exports = {
        let module = heap.module(*module);
        module
            .names
            .iter()
            .copied()
            .zip(module.values.iter().cloned())
            .collect::<Vec<_>>()
    };
    for (name, value) in exports {
        define_vm_env(heap, env, name, value);
    }
    Ok(())
}
