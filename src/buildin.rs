use crate::error::Error;
use crate::object::Object;
use std::collections::HashMap;

pub fn new_builtin_function_map() -> HashMap<&'static str, fn(&Vec<Object>) -> Result<Object, Error>>
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

fn _print(args: &Vec<Object>) -> Result<Object, Error> {
    let length = args.len();
    let mut space = " ";
    for (i, x) in args.iter().enumerate() {
        if length == i + 1 {
            space = "";
        }
        match x {
            Object::INTEGER(v) => {
                print!("{v}{space}")
            }
            Object::FLOAT(v) => {
                print!("{v}{space}")
            }
            Object::BOOLEAN(v) => {
                print!("{v}{space}")
            }
            Object::STRING(v) => {
                print!("{v}{space}")
            }
            Object::EMPTY => {}
            _ => {
                return Err(Error {
                    msg: "unsupported print type error".to_string(),
                })
            }
        }
    }
    Ok(Object::EMPTY)
}

fn _len(args: &Vec<Object>) -> Result<Object, Error> {
    if args.len() != 1 {
        return Err(Error {
            msg: "the input parameter can only be one".to_string(),
        });
    }
    Ok(Object::INTEGER(match args[0] {
        Object::STRING(ref r) => r.len() as i64,
        Object::MAP(ref r) => r.borrow().len() as i64,
        Object::SLICE(ref r) => r.borrow().len() as i64,
        _ => {
            return Err(Error {
                msg: "param type error".to_string(),
            })
        }
    }))
}

fn _str(args: &Vec<Object>) -> Result<Object, Error> {
    if args.len() != 1 {
        return Err(Error {
            msg: "the input parameter can only be one".to_string(),
        });
    }
    Ok(Object::STRING(match args[0] {
        Object::INTEGER(ref r) => r.to_string(),
        Object::FLOAT(ref r) => r.to_string(),
        Object::STRING(ref r) => r.to_owned(),
        _ => {
            return Err(Error {
                msg: "param type error is not integer or float or string".to_string(),
            })
        }
    }))
}

fn _int(args: &Vec<Object>) -> Result<Object, Error> {
    if args.len() != 1 {
        return Err(Error {
            msg: "the input parameter can only be one".to_string(),
        });
    }
    Ok(Object::INTEGER(match args[0] {
        Object::INTEGER(ref r) => *r,
        Object::FLOAT(ref r) => *r as i64,
        Object::STRING(ref r) => r.parse::<i64>().map_err(|e| Error { msg: e.to_string() })?,
        _ => {
            return Err(Error {
                msg: "param type error is not integer or float or string".to_string(),
            })
        }
    }))
}

fn _float(args: &Vec<Object>) -> Result<Object, Error> {
    if args.len() != 1 {
        return Err(Error {
            msg: "param number error".to_string(),
        });
    }
    Ok(Object::FLOAT(match args[0] {
        Object::INTEGER(ref r) => *r as f64,
        Object::FLOAT(ref r) => *r,
        Object::STRING(ref r) => r.parse::<f64>().map_err(|e| Error { msg: e.to_string() })?,
        _ => {
            return Err(Error {
                msg: "param type error is not integer or float or string".to_string(),
            })
        }
    }))
}

fn _delete(args: &Vec<Object>) -> Result<Object, Error> {
    if args.len() != 2 {
        return Err(Error {
            msg: "param number error".to_string(),
        });
    }
    Ok(match (&args[0], &args[1]) {
        (Object::MAP(map), key) => map
            .borrow_mut()
            .remove(&key.to_owned().try_into()?)
            .unwrap_or(Object::EMPTY),
        (Object::SLICE(vec), Object::INTEGER(index)) => {
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

fn _append(args: &Vec<Object>) -> Result<Object, Error> {
    if args.len() != 2 {
        return Err(Error {
            msg: "param number error".to_string(),
        });
    }
    Ok(match (&args[0], &args[1]) {
        (Object::SLICE(vec), element) => {
            vec.borrow_mut().push(element.to_owned());
            Object::EMPTY
        }
        _ => {
            return Err(Error {
                msg: "param type error, not be slice".to_string(),
            })
        }
    })
}
