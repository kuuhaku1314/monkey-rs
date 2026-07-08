use super::compiler::Compiler;
use super::env::{vm_module_from_env, VmEnv, VmEnvHandle};
use super::runtime::value::Value;
use super::Vm;
use crate::error::{Error, ErrorKind};
use crate::lexer::{new_lexer, Span};
use crate::parser::new_parser;
use std::fs;
use std::path::{Path, PathBuf};
use std::rc::Rc;

impl Vm {
    pub(super) fn current_source_path(&self) -> Option<String> {
        self.source_stack.iter().rev().find_map(|path| path.clone())
    }

    pub(super) fn import_module(
        &mut self,
        path: &str,
        span: Span,
        env: VmEnvHandle,
    ) -> Result<Value, Error> {
        let resolved_path = resolve_import_path(path, self.current_source_path().as_deref())
            .map_err(|err| {
                Error::with_kind_source_span(
                    ErrorKind::Import,
                    format!("import {path} failed: {err}"),
                    self.current_source_path(),
                    span,
                )
            })?;
        let canonical_path = fs::canonicalize(resolved_path.as_path()).map_err(|err| {
            Error::with_kind_source_span(
                ErrorKind::Import,
                format!("import {} failed: {}", resolved_path.display(), err),
                self.current_source_path(),
                span,
            )
        })?;
        if let Some(module) = self.module_cache.get(&canonical_path).cloned() {
            self.record_import(true);
            return Ok(module);
        }
        self.record_import(false);
        if self.import_stack.contains(&canonical_path) {
            return Err(Error::with_kind_source_span(
                ErrorKind::Import,
                format!("cyclic import {}", canonical_path.display()),
                self.current_source_path(),
                span,
            ));
        }

        let source = fs::read_to_string(canonical_path.as_path()).map_err(|err| {
            Error::with_kind_source_span(
                ErrorKind::Import,
                format!("import {} failed: {}", canonical_path.display(), err),
                self.current_source_path(),
                span,
            )
        })?;
        let import_path = canonical_path.to_string_lossy().into_owned();
        let mut parser = new_parser(new_lexer(source, Some(import_path.to_string())));
        let program = parser
            .parse_program()
            .map_err(|err| err.with_source_path(Some(import_path.to_string())))?;
        let function = Compiler::compile_program(&program, Rc::clone(&self.interner))?;
        let module_env = VmEnv::alloc_child(&mut self.heap, env, function.local_count);

        self.import_stack.push(canonical_path.clone());
        self.source_stack.push(Some(import_path.clone()));
        let result = self.execute_register_function(Rc::clone(&function), module_env);
        self.source_stack.pop();
        self.import_stack.pop();
        result.map_err(|err| err.with_source_path(Some(import_path)))?;

        let module = vm_module_from_env(module_env, &mut self.heap);
        self.module_cache.insert(canonical_path, module.clone());
        Ok(module)
    }
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
