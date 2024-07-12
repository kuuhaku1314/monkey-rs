use crate::error::Error;
use crate::object::Object;
use std::collections::HashMap;
use crate::evaluator::BuildInFn;

pub fn new_builtin_function_map() -> HashMap<&'static str, BuildInFn>
{
    let mut map = HashMap::new();
    map.insert("print", _print as _);
    map.insert("len", _len as _);
    map.insert("str", _str as _);
    map.insert("int", _int as _);
    map.insert("float", _float as _);
    map.insert("append", _append as _);
    map.insert("delete", _delete as _);
    map
}

fn _print(args: &[Object]) -> Result<Object, Error> {
    let length = args.len();
    let mut space = " ";
    for (i, x) in args.iter().enumerate() {
        if length == i + 1 {
            space = "";
        }
        match x {
            Object::Integer(v) => {
                print!("{v}{space}")
            }
            Object::Float(v) => {
                print!("{v}{space}")
            }
            Object::Boolean(v) => {
                print!("{v}{space}")
            }
            Object::String(v) => {
                print!("{v}{space}")
            }
            Object::Empty => {}
            _ => {
                return Err(Error {
                    msg: "unsupported print type error".to_string(),
                })
            }
        }
    }
    Ok(Object::Empty)
}

fn _len(args: &[Object]) -> Result<Object, Error> {
    if args.len() != 1 {
        return Err(Error {
            msg: "the input parameter can only be one".to_string(),
        });
    }
    Ok(Object::Integer(match args[0] {
        Object::String(ref r) => r.len() as i64,
        Object::Map(ref r) => r.borrow().len() as i64,
        Object::Slice(ref r) => r.borrow().len() as i64,
        _ => {
            return Err(Error {
                msg: "param type error".to_string(),
            })
        }
    }))
}

fn _str(args: &[Object]) -> Result<Object, Error> {
    if args.len() != 1 {
        return Err(Error {
            msg: "the input parameter can only be one".to_string(),
        });
    }
    Ok(Object::String(match args[0] {
        Object::Integer(ref r) => r.to_string(),
        Object::Float(ref r) => r.to_string(),
        Object::String(ref r) => r.to_owned(),
        _ => {
            return Err(Error {
                msg: "param type error is not integer or float or string".to_string(),
            })
        }
    }))
}

fn _int(args: &[Object]) -> Result<Object, Error> {
    if args.len() != 1 {
        return Err(Error {
            msg: "the input parameter can only be one".to_string(),
        });
    }
    Ok(Object::Integer(match args[0] {
        Object::Integer(ref r) => *r,
        Object::Float(ref r) => *r as i64,
        Object::String(ref r) => r.parse::<i64>().map_err(|e| Error { msg: e.to_string() })?,
        _ => {
            return Err(Error {
                msg: "param type error is not integer or float or string".to_string(),
            })
        }
    }))
}

fn _float(args: &[Object]) -> Result<Object, Error> {
    if args.len() != 1 {
        return Err(Error {
            msg: "param number error".to_string(),
        });
    }
    Ok(Object::Float(match args[0] {
        Object::Integer(ref r) => *r as f64,
        Object::Float(ref r) => *r,
        Object::String(ref r) => r.parse::<f64>().map_err(|e| Error { msg: e.to_string() })?,
        _ => {
            return Err(Error {
                msg: "param type error is not integer or float or string".to_string(),
            })
        }
    }))
}

fn _delete(args: &[Object]) -> Result<Object, Error> {
    if args.len() != 2 {
        return Err(Error {
            msg: "param number error".to_string(),
        });
    }
    Ok(match (&args[0], &args[1]) {
        (Object::Map(map), key) => map
            .borrow_mut()
            .remove(&key.to_owned().try_into()?)
            .unwrap_or(Object::Empty),
        (Object::Slice(vec), Object::Integer(index)) => {
            if vec.borrow().len() <= *index as usize {
                return Err(Error {
                    msg: "index out of bound".to_string(),
                });
            }
            vec.borrow_mut().remove(*index as usize)
        }
        _ => {
            return Err(Error {
                msg: "param type error, index type error or left not be slice or map".to_string(),
            })
        }
    })
}

fn _append(args: &[Object]) -> Result<Object, Error> {
    if args.len() != 2 {
        return Err(Error {
            msg: "param number error".to_string(),
        });
    }
    Ok(match (&args[0], &args[1]) {
        (Object::Slice(vec), element) => {
            vec.borrow_mut().push(element.to_owned());
            Object::Empty
        }
        _ => {
            return Err(Error {
                msg: "param type error, not be slice".to_string(),
            })
        }
    })
}
