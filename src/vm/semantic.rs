use super::program::{BuiltinOp, UpvalueSpec};
use super::symbols::SymbolId;
use crate::error::{Error, ErrorKind};
use crate::lexer::Span;
use std::collections::HashMap;

#[derive(Clone, Default)]
struct CompileScope {
    bindings: HashMap<SymbolId, usize>,
    imported_builtin_modules: HashMap<SymbolId, HashMap<SymbolId, BuiltinOp>>,
    bare_imported_builtins: HashMap<SymbolId, BuiltinOp>,
    local_count: usize,
}

#[derive(Clone)]
pub(super) struct SemanticContext {
    scopes: Vec<CompileScope>,
    upvalues: Vec<UpvalueSpec>,
    capture_nonlocals: bool,
    capture_boundary: Option<usize>,
    scope_cleanup_depth: usize,
}

impl SemanticContext {
    pub(super) fn root() -> Self {
        Self {
            scopes: vec![CompileScope::default()],
            upvalues: Vec::new(),
            capture_nonlocals: false,
            capture_boundary: None,
            scope_cleanup_depth: 0,
        }
    }

    pub(super) fn function_child(&self) -> Self {
        let function_scope_index = self.scopes.len();
        let mut scopes = self.scopes.clone();
        scopes.push(CompileScope::default());
        Self {
            scopes,
            upvalues: Vec::new(),
            capture_nonlocals: true,
            capture_boundary: Some(function_scope_index),
            scope_cleanup_depth: 0,
        }
    }

    pub(super) fn current_scope_index(&self) -> usize {
        self.scopes.len() - 1
    }

    pub(super) fn local_count_at(&self, index: usize) -> usize {
        self.scopes[index].local_count
    }

    pub(super) fn upvalues(&self) -> &[UpvalueSpec] {
        &self.upvalues
    }

    pub(super) fn cleanup_depth(&self) -> usize {
        self.scope_cleanup_depth
    }

    pub(super) fn enter_scope(&mut self) {
        self.scopes.push(CompileScope::default());
        self.scope_cleanup_depth += 1;
    }

    pub(super) fn exit_scope(&mut self) -> usize {
        self.scope_cleanup_depth -= 1;
        self.scopes
            .pop()
            .expect("semantic scope must exist")
            .local_count
    }

    pub(super) fn push_for_scope(&mut self) {
        self.scopes.push(CompileScope::default());
    }

    pub(super) fn pop_for_scope(&mut self) -> usize {
        self.scopes
            .pop()
            .expect("for-in semantic scope must exist")
            .local_count
    }

    pub(super) fn declare_current(
        &mut self,
        name: SymbolId,
        name_text: String,
        span: Span,
    ) -> Result<usize, Error> {
        let scope = self
            .scopes
            .last_mut()
            .expect("semantic context must have a scope");
        if scope.bindings.contains_key(&name) {
            return Err(Error::with_kind_span(
                ErrorKind::Name,
                format!("cannot redeclare name '{name_text}' in the same scope"),
                span,
            ));
        }
        let slot = scope.local_count;
        scope.local_count += 1;
        scope.bindings.insert(name, slot);
        Ok(slot)
    }

    pub(super) fn register_imported_builtin_module(
        &mut self,
        alias: SymbolId,
        members: HashMap<SymbolId, BuiltinOp>,
    ) {
        if !members.is_empty() {
            self.scopes
                .last_mut()
                .expect("semantic context must have a scope")
                .imported_builtin_modules
                .insert(alias, members);
        }
    }

    pub(super) fn register_bare_imported_builtins(&mut self, builtins: Vec<(SymbolId, BuiltinOp)>) {
        if !builtins.is_empty() {
            self.scopes
                .last_mut()
                .expect("semantic context must have a scope")
                .bare_imported_builtins
                .extend(builtins);
        }
    }

    pub(super) fn resolve_imported_builtin_member(
        &self,
        module: SymbolId,
        member: SymbolId,
    ) -> Option<BuiltinOp> {
        for scope in self.scopes.iter().rev() {
            if scope.bindings.contains_key(&module) {
                return None;
            }
            if let Some(exports) = scope.imported_builtin_modules.get(&module) {
                return exports.get(&member).copied();
            }
        }
        None
    }

    pub(super) fn resolve_direct_builtin_call(
        &self,
        symbol: SymbolId,
        name: &str,
    ) -> Option<BuiltinOp> {
        if self.resolve(symbol).is_some() {
            return None;
        }
        self.resolve_bare_imported_builtin(symbol)
            .or_else(|| BuiltinOp::from_global_name(name))
    }

    pub(super) fn resolve(&self, name: SymbolId) -> Option<(usize, usize)> {
        for (depth, scope) in self.scopes.iter().rev().enumerate() {
            if let Some(slot) = scope.bindings.get(&name) {
                return Some((depth, *slot));
            }
        }
        None
    }

    pub(super) fn should_capture_depth(&self, depth: usize) -> bool {
        if !self.capture_nonlocals || depth == 0 {
            return false;
        }
        let Some(boundary) = self.capture_boundary else {
            return false;
        };
        let binding_scope = self.scopes.len() - 1 - depth;
        binding_scope < boundary
    }

    pub(super) fn upvalue_index(&mut self, depth: usize, slot: usize) -> usize {
        let binding_scope = self.scopes.len() - 1 - depth;
        let boundary = self
            .capture_boundary
            .expect("upvalue capture requires a function boundary");
        let capture_depth = boundary - 1 - binding_scope;
        if let Some(index) = self
            .upvalues
            .iter()
            .position(|upvalue| upvalue.depth == capture_depth && upvalue.slot == slot)
        {
            return index;
        }
        let index = self.upvalues.len();
        self.upvalues.push(UpvalueSpec {
            depth: capture_depth,
            slot,
        });
        index
    }

    fn resolve_bare_imported_builtin(&self, name: SymbolId) -> Option<BuiltinOp> {
        for scope in self.scopes.iter().rev() {
            if scope.bindings.contains_key(&name) {
                return None;
            }
            if let Some(op) = scope.bare_imported_builtins.get(&name) {
                return Some(*op);
            }
        }
        None
    }
}
