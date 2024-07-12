use crate::ast::FunctionLiteral;
use crate::error::Error;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;

#[derive(Clone)]
pub enum Object {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Empty,
    Function(Rc<FunctionLiteral>),
    ReturnValue(Box<Object>),
    BuildIn(String),
    Map(Rc<RefCell<HashMap<AbleToMapKey, Object>>>),
    Slice(Rc<RefCell<Vec<Object>>>),
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
            _ => Err(Error {
                msg: "unable as map key".to_string(),
            }),
        }
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
            Object::String(v) => {
                write!(f, "{}", v)
            }
            Object::Empty => Ok(()),
            Object::Function(_) => {
                write!(f, "function")
            }
            Object::ReturnValue(v) => {
                write!(f, "{}", v)
            }
            Object::BuildIn(v) => {
                write!(f, "build in func:{}", v)
            }
            Object::Map(_) => {
                write!(f, "map")
            }
            Object::Slice(_) => {
                write!(f, "slice")
            }
        }
    }
}

pub struct Environment {
    pub store: HashMap<String, Object>,
    pub prev: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: Default::default(),
            prev: None,
        }
    }

    pub fn with_prev(prev: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            store: Default::default(),
            prev: Some(prev),
        }
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
        return Some(match value {
            Object::Integer(v) => Object::Integer(*v),
            Object::Float(v) => Object::Float(*v),
            Object::Boolean(v) => Object::Boolean(*v),
            Object::String(v) => Object::String(v.to_owned()),
            Object::Empty => Object::Empty,
            Object::Function(v) => Object::Function(Rc::clone(v)),
            Object::Map(v) => Object::Map(Rc::clone(v)),
            Object::Slice(v) => Object::Slice(Rc::clone(v)),
            _ => unreachable!(),
        });
    }
    if let Some(ref prev_env) = env.prev {
        return from_env(prev_env, name);
    }
    None
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
