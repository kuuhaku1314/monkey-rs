use crate::ast::FunctionLiteral;
use crate::error::Error;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;

#[derive(Clone)]
pub enum Object {
    INTEGER(i64),
    FLOAT(f64),
    BOOLEAN(bool),
    STRING(String),
    EMPTY,
    FUNCTION(Rc<FunctionLiteral>),
    ReturnValue(Box<Object>),
    BuildIn(String),
    MAP(Rc<RefCell<HashMap<AbleToMapKey, Object>>>),
    SLICE(Rc<RefCell<Vec<Object>>>),
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum AbleToMapKey {
    INTEGER(i64),
    BOOLEAN(bool),
    STRING(String),
}

impl TryFrom<Object> for AbleToMapKey {
    type Error = Error;

    fn try_from(value: Object) -> Result<Self, Error> {
        match value {
            Object::INTEGER(i) => Ok(AbleToMapKey::INTEGER(i)),
            Object::BOOLEAN(i) => Ok(AbleToMapKey::BOOLEAN(i)),
            Object::STRING(i) => Ok(AbleToMapKey::STRING(i)),
            _ => Err(Error {
                msg: "unable as map key".to_string(),
            }),
        }
    }
}

impl From<AbleToMapKey> for Object {
    fn from(value: AbleToMapKey) -> Object {
        match value {
            AbleToMapKey::INTEGER(i) => Object::INTEGER(i),
            AbleToMapKey::BOOLEAN(i) => Object::BOOLEAN(i),
            AbleToMapKey::STRING(i) => Object::STRING(i),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::INTEGER(v) => {
                write!(f, "{}", v)
            }
            Object::FLOAT(v) => {
                write!(f, "{}", v)
            }
            Object::BOOLEAN(v) => {
                write!(f, "{}", v)
            }
            Object::STRING(v) => {
                write!(f, "{}", v)
            }
            Object::EMPTY => Ok(()),
            Object::FUNCTION(_) => {
                write!(f, "function")
            }
            Object::ReturnValue(v) => {
                write!(f, "{}", v)
            }
            Object::BuildIn(v) => {
                write!(f, "build in func:{}", v)
            }
            Object::MAP(_) => {
                write!(f, "map")
            }
            Object::SLICE(_) => {
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
            Object::INTEGER(v) => Object::INTEGER(*v),
            Object::FLOAT(v) => Object::FLOAT(*v),
            Object::BOOLEAN(v) => Object::BOOLEAN(*v),
            Object::STRING(v) => Object::STRING(v.to_owned()),
            Object::EMPTY => Object::EMPTY,
            Object::FUNCTION(v) => Object::FUNCTION(Rc::clone(v)),
            Object::MAP(v) => Object::MAP(Rc::clone(v)),
            Object::SLICE(v) => Object::SLICE(Rc::clone(v)),
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
    if env.store.contains_key(&name) {
        env.store.insert(name, value);
    } else if let Some(ref prev_env) = env.prev {
        to_env(prev_env, name, value);
    } else {
        unreachable!()
    }
}
