use super::env::{assign_vm_env, from_vm_env, set_vm_env_slot_value, vm_env_at_depth, VmEnvHandle};
use super::program::BuiltinOp;
use super::runtime::ops::eval_infix_value;
use super::runtime::value::Value;
use super::symbols::SymbolId;
use super::Vm;
use crate::error::{Error, ErrorKind};
use crate::lexer::Span;
use crate::token::Token;

impl Vm {
    pub(super) fn get_name(
        &mut self,
        env: VmEnvHandle,
        name: SymbolId,
        span: Span,
        cache_key: (usize, usize),
    ) -> Result<Value, Error> {
        if let Some(depth) = self.get_name_cache.get(&cache_key).copied() {
            if let Some(cached_env) = vm_env_at_depth(&self.heap, env, depth) {
                let value = {
                    let cached_env = self.heap.env(cached_env);
                    cached_env.store.get(&name).cloned().or_else(|| {
                        cached_env.slot_bindings.get(&name).and_then(|slot| {
                            cached_env
                                .upvalue_cells
                                .get(*slot)
                                .and_then(|cell| *cell)
                                .map(|cell| self.heap.cell(cell))
                                .or_else(|| cached_env.slots.get(*slot).cloned())
                        })
                    })
                };
                if let Some(value) = value {
                    return Ok(value);
                }
            }
        }
        if let Some((value, depth)) = from_vm_env(&self.heap, env, name) {
            self.get_name_cache.insert(cache_key, depth);
            return Ok(value);
        }
        let name_text = self.symbol_name(name);
        if let Some(op) = BuiltinOp::from_global_name(name_text.as_str()) {
            return Ok(Value::Builtin(op));
        }
        Err(Error::with_kind_span(
            ErrorKind::Name,
            format!("not found variable name '{name_text}'"),
            span,
        ))
    }

    pub(super) fn set_name(
        &mut self,
        env: VmEnvHandle,
        name: SymbolId,
        value: Value,
        span: Span,
        cache_key: (usize, usize),
    ) -> Result<(), Error> {
        if let Some(depth) = self.set_name_cache.get(&cache_key).copied() {
            if let Some(cached_env) = vm_env_at_depth(&self.heap, env, depth) {
                let target = {
                    let borrowed = self.heap.env(cached_env);
                    if borrowed.store.contains_key(&name) {
                        Some(None)
                    } else {
                        borrowed.slot_bindings.get(&name).copied().map(Some)
                    }
                };
                if let Some(slot) = target {
                    if let Some(slot) = slot {
                        set_vm_env_slot_value(&mut self.heap, cached_env, slot, value);
                    } else {
                        self.heap.env_mut(cached_env).store.insert(name, value);
                    }
                    return Ok(());
                }
            }
        }
        let depth = assign_vm_env(&mut self.heap, env, name, value).ok_or_else(|| {
            let name_text = self.symbol_name(name);
            Error::with_kind_span(
                ErrorKind::Name,
                format!("not found variable name '{name_text}'"),
                span,
            )
        })?;
        self.set_name_cache.insert(cache_key, depth);
        Ok(())
    }

    pub(super) fn update_name_infix(
        &mut self,
        env: VmEnvHandle,
        name: SymbolId,
        token: &Token,
        right: Value,
        span: Span,
        cache_key: (usize, usize),
    ) -> Result<Value, Error> {
        if let Some(depth) = self.set_name_cache.get(&cache_key).copied() {
            if let Some(cached_env) = vm_env_at_depth(&self.heap, env, depth) {
                let target = {
                    let borrowed = self.heap.env(cached_env);
                    if let Some(value) = borrowed.store.get(&name).cloned() {
                        Some((None, value))
                    } else {
                        borrowed
                            .slot_bindings
                            .get(&name)
                            .and_then(|slot| {
                                borrowed
                                    .upvalue_cells
                                    .get(*slot)
                                    .and_then(|cell| *cell)
                                    .map(|cell| (*slot, self.heap.cell(cell)))
                                    .or_else(|| {
                                        borrowed
                                            .slots
                                            .get(*slot)
                                            .cloned()
                                            .map(|value| (*slot, value))
                                    })
                            })
                            .map(|(slot, value)| (Some(slot), value))
                    }
                };
                if let Some((slot, left)) = target {
                    let value = eval_infix_value(&mut self.heap, token, left, right.clone(), span)?;
                    if let Some(slot) = slot {
                        set_vm_env_slot_value(&mut self.heap, cached_env, slot, value.clone());
                    } else {
                        self.heap
                            .env_mut(cached_env)
                            .store
                            .insert(name, value.clone());
                    }
                    return Ok(value);
                }
            }
        }
        let (left, depth) = from_vm_env(&self.heap, env, name).ok_or_else(|| {
            let name_text = self.symbol_name(name);
            Error::with_kind_span(
                ErrorKind::Name,
                format!("not found variable name '{name_text}'"),
                span,
            )
        })?;
        let value = eval_infix_value(&mut self.heap, token, left, right, span)?;
        assign_vm_env(&mut self.heap, env, name, value.clone()).ok_or_else(|| {
            let name_text = self.symbol_name(name);
            Error::with_kind_span(
                ErrorKind::Name,
                format!("not found variable name '{name_text}'"),
                span,
            )
        })?;
        self.set_name_cache.insert(cache_key, depth);
        Ok(value)
    }
}
