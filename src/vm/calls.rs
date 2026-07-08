use super::builtins;
use super::program::BuiltinOp;
use super::runtime::ops::operation_type_error;
use super::runtime::value::Value;
use super::Vm;
use crate::error::{CallFrame, Error};
use crate::lexer::Span;
use std::time::Instant;

impl Vm {
    pub(super) fn call_builtin_value(
        &mut self,
        function: Value,
        args: Vec<Value>,
        span: Span,
    ) -> Result<Value, Error> {
        match function {
            Value::Builtin(op) => self.call_builtin_direct(op, &args, span),
            _ => Err(operation_type_error(span)),
        }
    }

    pub(super) fn call_builtin_direct(
        &mut self,
        op: BuiltinOp,
        args: &[Value],
        span: Span,
    ) -> Result<Value, Error> {
        let name = op.name();
        self.record_builtin(name);
        self.push_call_frame(name.to_string(), span);
        let started = self.profile.as_ref().map(|_| Instant::now());
        let result = builtins::call(&mut self.heap, op, args, span);
        if let Some(started) = started {
            self.record_builtin_elapsed(name, started.elapsed());
        }
        let result = result.map_err(|err| self.attach_call_error(err, span));
        self.call_stack.pop();
        result
    }

    pub(super) fn push_call_frame(&mut self, name: String, span: Span) {
        self.record_call();
        self.call_stack.push(CallFrame {
            name,
            span,
            source_path: self.current_source_path(),
        });
    }

    pub(super) fn attach_call_error(&self, err: Error, span: Span) -> Error {
        err.with_source_path(self.current_source_path())
            .with_span_if_none(span)
            .with_stack(&self.call_stack)
    }
}
