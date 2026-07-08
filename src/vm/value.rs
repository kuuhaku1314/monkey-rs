use super::heap::VmHeap;
use super::runtime::value::Value;
use std::fmt::{Display, Formatter};

pub struct VmValue {
    value: Value,
    heap: VmHeap,
}

impl VmValue {
    pub(super) fn new(value: Value, heap: VmHeap) -> Self {
        Self { value, heap }
    }

    pub fn is_empty(&self) -> bool {
        self.view().is_empty()
    }

    pub fn view(&self) -> VmValueView {
        value_view(&self.value, &self.heap)
    }

    #[cfg(test)]
    pub fn expect_integer(&self) -> i64 {
        match self.view() {
            VmValueView::Integer(value) => value,
            _ => panic!("expected int, got {}", self),
        }
    }

    #[cfg(test)]
    pub fn expect_float(&self) -> f64 {
        match self.view() {
            VmValueView::Float(value) => value,
            _ => panic!("expected float, got {}", self),
        }
    }

    #[cfg(test)]
    pub fn expect_string(&self) -> String {
        match self.view() {
            VmValueView::String(value) => value,
            _ => panic!("expected string, got {}", self),
        }
    }
}

impl Display for VmValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.view())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum VmValueView {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Null,
    String(String),
    Empty,
    Function,
    Module,
    Builtin,
    Break,
    Continue,
    List,
    Map,
    StructType(String),
    StructInstance(String),
}

impl VmValueView {
    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }
}

impl Display for VmValueView {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(value) => write!(f, "{value}"),
            Self::Float(value) => write!(f, "{value}"),
            Self::Boolean(value) => write!(f, "{value}"),
            Self::Null => write!(f, "null"),
            Self::String(value) => write!(f, "{value}"),
            Self::Empty => Ok(()),
            Self::Function => write!(f, "function"),
            Self::Module => write!(f, "module"),
            Self::Builtin => write!(f, "builtin"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
            Self::List => write!(f, "list"),
            Self::Map => write!(f, "map"),
            Self::StructType(name) => write!(f, "struct {name}"),
            Self::StructInstance(name) => write!(f, "struct {name}"),
        }
    }
}

fn value_view(value: &Value, heap: &VmHeap) -> VmValueView {
    match value {
        Value::Integer(value) => VmValueView::Integer(*value),
        Value::Float(value) => VmValueView::Float(*value),
        Value::Boolean(value) => VmValueView::Boolean(*value),
        Value::Null => VmValueView::Null,
        Value::String(value) => VmValueView::String(heap.string(*value).to_string()),
        Value::Empty => VmValueView::Empty,
        Value::Function(_) => VmValueView::Function,
        Value::Module(_) => VmValueView::Module,
        Value::Builtin(_) => VmValueView::Builtin,
        Value::Break(span) => {
            let _ = span;
            VmValueView::Break
        }
        Value::Continue(span) => {
            let _ = span;
            VmValueView::Continue
        }
        Value::Slice(_) => VmValueView::List,
        Value::Map(_) => VmValueView::Map,
        Value::StructType(value) => VmValueView::StructType(heap.struct_type(*value).name.clone()),
        Value::StructInstance(value) => {
            VmValueView::StructInstance(heap.struct_instance(*value).type_name.clone())
        }
    }
}
