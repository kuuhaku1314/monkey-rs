use crate::ast::FunctionLiteral;
use crate::error::{Error, ErrorKind};
use crate::lexer::Span;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;

#[derive(Clone)]
pub enum Object {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Null,
    String(String),
    Empty,
    Function(Rc<FunctionObject>),
    StructType(Rc<StructTypeObject>),
    StructInstance(Rc<RefCell<StructInstanceObject>>),
    ReturnValue(Box<Object>),
    Break(Span),
    Continue(Span),
    BuildIn(String),
    Map(Rc<RefCell<HashMap<AbleToMapKey, Object>>>),
    Slice(Rc<RefCell<Vec<Object>>>),
}

pub struct FunctionObject {
    pub literal: FunctionLiteral,
    pub env: Env,
}

pub struct StructTypeObject {
    pub name: String,
    pub fields: Vec<String>,
}

pub struct StructInstanceObject {
    pub type_name: String,
    pub fields: Vec<String>,
    pub values: HashMap<AbleToMapKey, Object>,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum AbleToMapKey {
    Integer(i64),
    Boolean(bool),
    String(String),
}

impl TryFrom<Object> for AbleToMapKey {
    type Error = Error;

    fn try_from(value: Object) -> Result<Self, Error> {
        match value {
            Object::Integer(i) => Ok(AbleToMapKey::Integer(i)),
            Object::Boolean(i) => Ok(AbleToMapKey::Boolean(i)),
            Object::String(i) => Ok(AbleToMapKey::String(i)),
            _ => Err(Error::with_kind(
                ErrorKind::Type,
                "unable as map key".to_string(),
            )),
        }
    }
}

impl From<String> for AbleToMapKey {
    fn from(value: String) -> Self {
        AbleToMapKey::String(value)
    }
}

impl From<&str> for AbleToMapKey {
    fn from(value: &str) -> Self {
        AbleToMapKey::String(value.to_string())
    }
}

impl From<AbleToMapKey> for Object {
    fn from(value: AbleToMapKey) -> Object {
        match value {
            AbleToMapKey::Integer(i) => Object::Integer(i),
            AbleToMapKey::Boolean(i) => Object::Boolean(i),
            AbleToMapKey::String(i) => Object::String(i),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(v) => {
                write!(f, "{}", v)
            }
            Object::Float(v) => {
                write!(f, "{}", v)
            }
            Object::Boolean(v) => {
                write!(f, "{}", v)
            }
            Object::Null => {
                write!(f, "null")
            }
            Object::String(v) => {
                write!(f, "{}", v)
            }
            Object::Empty => Ok(()),
            Object::Function(_) => {
                write!(f, "{}", self.type_name())
            }
            Object::StructType(value) => {
                write!(f, "struct {}", value.name)
            }
            Object::StructInstance(value) => {
                write!(f, "struct {}", value.borrow().type_name)
            }
            Object::ReturnValue(v) => {
                write!(f, "{}", v)
            }
            Object::Break(_) => {
                write!(f, "{}", self.type_name())
            }
            Object::Continue(_) => {
                write!(f, "{}", self.type_name())
            }
            Object::BuildIn(v) => {
                write!(f, "build in func:{}", v)
            }
            Object::Map(_) => {
                write!(f, "{}", self.type_name())
            }
            Object::Slice(_) => {
                write!(f, "{}", self.type_name())
            }
        }
    }
}

impl Object {
    pub fn type_name(&self) -> &'static str {
        match self {
            Object::Integer(_) => "int",
            Object::Float(_) => "float",
            Object::Boolean(_) => "bool",
            Object::Null => "null",
            Object::String(_) => "string",
            Object::Empty => "empty",
            Object::Function(_) => "function",
            Object::StructType(_) => "struct",
            Object::StructInstance(_) => "record",
            Object::ReturnValue(_) => "return",
            Object::Break(_) => "break",
            Object::Continue(_) => "continue",
            Object::BuildIn(_) => "builtin",
            Object::Map(_) => "map",
            Object::Slice(_) => "list",
        }
    }

    pub fn clone_value(&self) -> Object {
        match self {
            Object::Integer(v) => Object::Integer(*v),
            Object::Float(v) => Object::Float(*v),
            Object::Boolean(v) => Object::Boolean(*v),
            Object::Null => Object::Null,
            Object::String(v) => Object::String(v.to_owned()),
            Object::Empty => Object::Empty,
            Object::Function(v) => Object::Function(Rc::clone(v)),
            Object::StructType(v) => Object::StructType(Rc::clone(v)),
            Object::StructInstance(v) => Object::StructInstance(Rc::clone(v)),
            Object::BuildIn(v) => Object::BuildIn(v.to_owned()),
            Object::Map(v) => Object::Map(Rc::clone(v)),
            Object::Slice(v) => Object::Slice(Rc::clone(v)),
            _ => unreachable!(),
        }
    }

    pub fn field_get(&self, key: AbleToMapKey) -> Result<Object, Error> {
        match self {
            Object::Map(map) => Ok(map.borrow().get(&key).cloned().unwrap_or(Object::Null)),
            Object::StructInstance(instance) => {
                let instance = instance.borrow();
                let field = match &key {
                    AbleToMapKey::String(field) => field,
                    _ => {
                        return Err(Error::with_kind(
                            ErrorKind::Type,
                            "struct field must be string".to_string(),
                        ))
                    }
                };
                if !instance.fields.iter().any(|allowed| allowed == field) {
                    return Err(Error::with_kind(
                        ErrorKind::Name,
                        format!(
                            "unknown field '{}' for struct {}",
                            field, instance.type_name
                        ),
                    ));
                }
                Ok(instance.values.get(&key).cloned().unwrap_or(Object::Null))
            }
            _ => Err(Error::with_kind(
                ErrorKind::Type,
                "operation type invalid".to_string(),
            )),
        }
    }

    pub fn field_set(&self, key: AbleToMapKey, value: Object) -> Result<(), Error> {
        match self {
            Object::Map(map) => {
                map.borrow_mut().insert(key, value);
                Ok(())
            }
            Object::StructInstance(instance) => {
                let mut instance = instance.borrow_mut();
                let field = match &key {
                    AbleToMapKey::String(field) => field,
                    _ => {
                        return Err(Error::with_kind(
                            ErrorKind::Type,
                            "struct field must be string".to_string(),
                        ))
                    }
                };
                if !instance.fields.iter().any(|allowed| allowed == field) {
                    return Err(Error::with_kind(
                        ErrorKind::Name,
                        format!(
                            "unknown field '{}' for struct {}",
                            field, instance.type_name
                        ),
                    ));
                }
                instance.values.insert(key, value);
                Ok(())
            }
            _ => Err(Error::with_kind(
                ErrorKind::Type,
                "operation type invalid".to_string(),
            )),
        }
    }

    pub fn list_get(&self, index: i64) -> Result<Object, Error> {
        match self {
            Object::Slice(vec) => {
                let vec = vec.borrow();
                if index < 0 || vec.len() <= index as usize {
                    return Err(Error::with_kind(
                        ErrorKind::Index,
                        "index out of bound".to_string(),
                    ));
                }
                Ok(vec[index as usize].clone())
            }
            _ => Err(Error::with_kind(
                ErrorKind::Type,
                "operation type invalid".to_string(),
            )),
        }
    }

    pub fn list_set(&self, index: i64, value: Object) -> Result<(), Error> {
        match self {
            Object::Slice(vec) => {
                let mut vec = vec.borrow_mut();
                if index < 0 || vec.len() <= index as usize {
                    return Err(Error::with_kind(
                        ErrorKind::Index,
                        "index out of bound".to_string(),
                    ));
                }
                vec[index as usize] = value;
                Ok(())
            }
            _ => Err(Error::with_kind(
                ErrorKind::Type,
                "operation type invalid".to_string(),
            )),
        }
    }
}

pub struct Environment {
    pub store: HashMap<String, Object>,
    exports: HashSet<String>,
    pub prev: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: Default::default(),
            exports: Default::default(),
            prev: None,
        }
    }

    pub fn with_prev(prev: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            store: Default::default(),
            exports: Default::default(),
            prev: Some(prev),
        }
    }

    fn define(&mut self, name: String, value: Object) {
        self.store.insert(name, value);
    }

    fn mark_export(&mut self, name: String) {
        self.exports.insert(name);
    }
}

pub type Env = Rc<RefCell<Environment>>;

pub fn new_env() -> Env {
    Rc::new(RefCell::new(Environment::new()))
}

pub fn enter_env(env: Env) -> Env {
    Rc::new(RefCell::new(Environment::with_prev(env)))
}

pub fn from_env(env: &Env, name: &String) -> Option<Object> {
    let env = env.borrow();
    if let Some(value) = env.store.get(name) {
        return Some(value.clone_value());
    }
    if let Some(ref prev_env) = env.prev {
        return from_env(prev_env, name);
    }
    None
}

pub fn define_env(env: &Env, name: String, value: Object) {
    env.borrow_mut().define(name, value);
}

pub fn mark_env_export(env: &Env, name: String) {
    env.borrow_mut().mark_export(name);
}

pub fn has_current_env_binding(env: &Env, name: &str) -> bool {
    env.borrow().store.contains_key(name)
}

pub fn current_env_exports(env: &Env) -> Vec<String> {
    env.borrow().exports.iter().cloned().collect()
}

pub fn to_env(env: &Env, name: String, value: Object) {
    let mut env = env.borrow_mut();
    if let std::collections::hash_map::Entry::Occupied(mut e) = env.store.entry(name.to_owned()) {
        e.insert(value);
    } else if let Some(ref prev_env) = env.prev {
        to_env(prev_env, name, value);
    } else {
        unreachable!()
    }
}

#[cfg(test)]
mod tests {
    use super::Object;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn list_display_uses_public_type_name() {
        let value = Object::Slice(Rc::new(RefCell::new(Vec::new())));

        assert_eq!(value.type_name(), "list");
        assert_eq!(value.to_string(), "list");
    }
}
