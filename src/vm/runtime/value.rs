use crate::lexer::Span;
use crate::map_key::AbleToMapKey;
use crate::vm::env::VmEnvHandle;
use crate::vm::program::{BuiltinOp, CompiledFunction};
use crate::vm::symbols::SymbolId;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(in crate::vm) struct VmStringHandle(pub(in crate::vm) usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(in crate::vm) struct VmFunctionHandle(pub(in crate::vm) usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(in crate::vm) struct VmModuleHandle(pub(in crate::vm) usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(in crate::vm) struct VmListHandle(pub(in crate::vm) usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(in crate::vm) struct VmMapHandle(pub(in crate::vm) usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(in crate::vm) struct VmStructTypeHandle(pub(in crate::vm) usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(in crate::vm) struct VmStructInstanceHandle(pub(in crate::vm) usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(in crate::vm) struct VmCellHandle(pub(in crate::vm) usize);

#[derive(Clone)]
pub(in crate::vm) enum Value {
    Null,
    Empty,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(VmStringHandle),
    Builtin(BuiltinOp),
    Break(Span),
    Continue(Span),
    Function(VmFunctionHandle),
    Module(VmModuleHandle),
    Slice(VmListHandle),
    Map(VmMapHandle),
    StructType(VmStructTypeHandle),
    StructInstance(VmStructInstanceHandle),
}

#[derive(Clone)]
pub(in crate::vm) struct VmFunction {
    pub(in crate::vm) compiled: Rc<CompiledFunction>,
    pub(in crate::vm) env: VmEnvHandle,
    pub(in crate::vm) upvalues: Rc<[VmCellHandle]>,
}

#[derive(Clone)]
pub(in crate::vm) struct VmModule {
    pub(in crate::vm) indexes: HashMap<SymbolId, usize>,
    pub(in crate::vm) names: Vec<SymbolId>,
    pub(in crate::vm) values: Vec<Value>,
}

#[derive(Clone)]
pub(in crate::vm) struct VmStructType {
    pub(in crate::vm) name: String,
    pub(in crate::vm) fields: Vec<String>,
}

#[derive(Clone)]
pub(in crate::vm) struct VmStructInstance {
    pub(in crate::vm) type_name: String,
    pub(in crate::vm) fields: Vec<String>,
    pub(in crate::vm) values: HashMap<AbleToMapKey, Value>,
}

impl Value {
    pub(in crate::vm) fn type_name(&self) -> &'static str {
        match self {
            Value::Null => "null",
            Value::Empty => "empty",
            Value::Boolean(_) => "bool",
            Value::Integer(_) => "int",
            Value::Float(_) => "float",
            Value::String(_) => "string",
            Value::Builtin(_) => "builtin",
            Value::Break(_) => "break",
            Value::Continue(_) => "continue",
            Value::Function(_) => "function",
            Value::Module(_) => "module",
            Value::Slice(_) => "list",
            Value::Map(_) => "map",
            Value::StructType(_) => "struct",
            Value::StructInstance(_) => "record",
        }
    }
}
