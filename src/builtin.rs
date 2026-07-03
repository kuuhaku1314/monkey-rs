use crate::error::{Error, ErrorKind};
use crate::evaluator::BuildInFn;
use crate::object::{AbleToMapKey, Object};
use base64::{engine::general_purpose, Engine};
use crossterm::{
    cursor,
    event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers},
    execute,
    style::{Attribute, Color, ResetColor, SetAttribute, SetBackgroundColor, SetForegroundColor},
    terminal::{self as ct_terminal, ClearType},
};
use rand::Rng;
use sha2::{Digest, Sha256};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::{self, OpenOptions};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::rc::Rc;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

pub fn new_builtin_function_map() -> HashMap<&'static str, BuildInFn> {
    let mut map = HashMap::new();
    map.insert("print", _print as _);
    map.insert("len", _len as _);
    map.insert("byte_len", _byte_len as _);
    map.insert("str", _str as _);
    map.insert("int", _int as _);
    map.insert("float", _float as _);
    map.insert("parse_int", _parse_int as _);
    map.insert("parse_float", _parse_float as _);
    map.insert("append", _append as _);
    map.insert("delete", _delete as _);
    map.insert("join", _join as _);
    map.insert("read_file", _read_file as _);
    map.insert("write_file", _write_file as _);
    map.insert("append_file", _append_file as _);
    map.insert("file_exists", _file_exists as _);
    map.insert("read_dir", _read_dir as _);
    map.insert("mkdir", _mkdir as _);
    map.insert("remove_file", _remove_file as _);
    map.insert("remove_dir", _remove_dir as _);
    map.insert("copy_file", _copy_file as _);
    map.insert("rename", _rename as _);
    map.insert("metadata", _metadata as _);
    map.insert("path_join", _path_join as _);
    map.insert("path_dirname", _path_dirname as _);
    map.insert("path_basename", _path_basename as _);
    map.insert("path_ext", _path_ext as _);
    map.insert("path_exists", _path_exists as _);
    map.insert("path_is_file", _path_is_file as _);
    map.insert("path_is_dir", _path_is_dir as _);
    map.insert("env_get", _env_get as _);
    map.insert("env_set", _env_set as _);
    map.insert("cwd", _cwd as _);
    map.insert("set_cwd", _set_cwd as _);
    map.insert("exit", _exit as _);
    map.insert("time_ms", _time_ms as _);
    map.insert("now_ms", _now_ms as _);
    map.insert("sleep_ms", _sleep_ms as _);
    map.insert("clear", _clear as _);
    map.insert("home", _home as _);
    map.insert("hide_cursor", _hide_cursor as _);
    map.insert("show_cursor", _show_cursor as _);
    map.insert("enable_raw_mode", _enable_raw_mode as _);
    map.insert("disable_raw_mode", _disable_raw_mode as _);
    map.insert("read_key", _read_key as _);
    map.insert("read_key_timeout", _read_key_timeout as _);
    map.insert("read_key_latest_timeout", _read_key_latest_timeout as _);
    map.insert("move", _move_cursor as _);
    map.insert("clear_line", _clear_line as _);
    map.insert("size", _terminal_size as _);
    map.insert("enter_alt_screen", _enter_alt_screen as _);
    map.insert("leave_alt_screen", _leave_alt_screen as _);
    map.insert("fg", _terminal_fg as _);
    map.insert("bg", _terminal_bg as _);
    map.insert("bold", _terminal_bold as _);
    map.insert("reset_style", _terminal_reset_style as _);
    map.insert("paint", _terminal_paint as _);
    map.insert("paint_runs", _terminal_paint_runs as _);
    map.insert("abs", _abs as _);
    map.insert("floor", _floor as _);
    map.insert("ceil", _ceil as _);
    map.insert("round", _round as _);
    map.insert("sqrt", _sqrt as _);
    map.insert("pow", _pow as _);
    map.insert("min", _min as _);
    map.insert("max", _max as _);
    map.insert("random_int", _random_int as _);
    map.insert("random_float", _random_float as _);
    map.insert("sort", _sort as _);
    map.insert("http_get", _http_get as _);
    map.insert("http_post", _http_post as _);
    map.insert("http_request", _http_request as _);
    map.insert("exec", _exec as _);
    map.insert("url_encode", _url_encode as _);
    map.insert("url_decode", _url_decode as _);
    map.insert("base64_encode", _base64_encode as _);
    map.insert("base64_decode", _base64_decode as _);
    map.insert("sha256", _sha256 as _);
    map.insert("json_stringify", _json_stringify as _);
    map.insert("args", _args as _);
    map.insert("read_line", _read_line as _);
    map.insert("panic", _panic as _);
    map.insert("assert", _assert as _);
    map.insert("substr", _substr as _);
    map.insert("find", _find as _);
    map.insert("replace", _replace as _);
    map.insert("char_code", _char_code as _);
    map.insert("from_char_code", _from_char_code as _);
    map.insert("type", _type_of as _);
    map.insert("is_null", _is_null as _);
    map.insert("is_bool", _is_bool as _);
    map.insert("is_int", _is_int as _);
    map.insert("is_float", _is_float as _);
    map.insert("is_string", _is_string as _);
    map.insert("is_list", _is_list as _);
    map.insert("is_map", _is_map as _);
    map
}

pub fn new_global_builtin_function_map() -> HashMap<&'static str, BuildInFn> {
    let all = new_builtin_function_map();
    [
        "print",
        "len",
        "byte_len",
        "str",
        "int",
        "float",
        "parse_int",
        "parse_float",
        "append",
        "delete",
        "join",
        "args",
        "read_line",
        "panic",
        "assert",
        "substr",
        "find",
        "replace",
        "char_code",
        "from_char_code",
        "type",
        "is_null",
        "is_bool",
        "is_int",
        "is_float",
        "is_string",
        "is_list",
        "is_map",
    ]
    .into_iter()
    .filter_map(|name| all.get(name).map(|function| (name, *function)))
    .collect()
}

fn _print(args: &[Object]) -> Result<Object, Error> {
    for x in args.iter() {
        match x {
            Object::Integer(v) => {
                print!("{v}")
            }
            Object::Float(v) => {
                print!("{v}")
            }
            Object::Boolean(v) => {
                print!("{v}")
            }
            Object::Null => {
                print!("null")
            }
            Object::String(v) => {
                print!("{v}")
            }
            Object::Empty => {}
            _ => {
                return Err(Error::with_kind(
                    ErrorKind::Type,
                    "unsupported print type error".to_string(),
                ))
            }
        }
    }
    Ok(Object::Empty)
}

fn expect_arity(args: &[Object], function: &str, expected: usize) -> Result<(), Error> {
    if args.len() == expected {
        Ok(())
    } else {
        Err(Error::new(format!(
            "{function} expects {expected} argument{}, got {}",
            if expected == 1 { "" } else { "s" },
            args.len()
        )))
    }
}

fn _len(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "len", 1)?;
    Ok(Object::Integer(match args[0] {
        Object::String(ref r) => r.chars().count() as i64,
        Object::Map(ref r) => r.borrow().len() as i64,
        Object::Slice(ref r) => r.borrow().len() as i64,
        _ => {
            return Err(Error::with_kind(
                ErrorKind::Type,
                "param type error".to_string(),
            ))
        }
    }))
}

fn _byte_len(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "byte_len", 1)?;
    Ok(Object::Integer(match args[0] {
        Object::String(ref r) => r.len() as i64,
        _ => {
            return Err(Error::with_kind(
                ErrorKind::Type,
                "param type error".to_string(),
            ))
        }
    }))
}

fn _str(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "str", 1)?;
    Ok(Object::String(match args[0] {
        Object::Integer(ref r) => r.to_string(),
        Object::Float(ref r) => r.to_string(),
        Object::String(ref r) => r.to_owned(),
        Object::Null => "null".to_string(),
        _ => {
            return Err(Error::new(
                "param type error is not integer or float or string".to_string(),
            ))
        }
    }))
}

fn _int(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "int", 1)?;
    Ok(Object::Integer(match args[0] {
        Object::Integer(ref r) => *r,
        Object::Float(ref r) => *r as i64,
        Object::String(ref r) => r.parse::<i64>().map_err(|e| Error::new(e.to_string()))?,
        _ => {
            return Err(Error::new(
                "param type error is not integer or float or string".to_string(),
            ))
        }
    }))
}

fn _float(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "float", 1)?;
    Ok(Object::Float(match args[0] {
        Object::Integer(ref r) => *r as f64,
        Object::Float(ref r) => *r,
        Object::String(ref r) => r.parse::<f64>().map_err(|e| Error::new(e.to_string()))?,
        _ => {
            return Err(Error::new(
                "param type error is not integer or float or string".to_string(),
            ))
        }
    }))
}

fn _parse_int(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "parse_int", 1)?;
    let value = expect_string(&args[0], "value")?;
    Ok(match value.parse::<i64>() {
        Ok(value) => result_ok(Object::Integer(value)),
        Err(err) => result_err("parse", "invalid_int", err.to_string()),
    })
}

fn _parse_float(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "parse_float", 1)?;
    let value = expect_string(&args[0], "value")?;
    Ok(match value.parse::<f64>() {
        Ok(value) if value.is_finite() => result_ok(Object::Float(value)),
        Ok(_) => result_err("parse", "invalid_float", "non-finite float".to_string()),
        Err(err) => result_err("parse", "invalid_float", err.to_string()),
    })
}

fn _delete(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "delete", 2)?;
    Ok(match (&args[0], &args[1]) {
        (Object::Map(map), key) => map
            .borrow_mut()
            .remove(&key.to_owned().try_into()?)
            .unwrap_or(Object::Empty),
        (Object::Slice(vec), Object::Integer(index)) => {
            if vec.borrow().len() <= *index as usize {
                return Err(Error::new("index out of bound".to_string()));
            }
            vec.borrow_mut().remove(*index as usize)
        }
        _ => {
            return Err(Error::new(
                "param type error, index type error or left not be list or map".to_string(),
            ))
        }
    })
}

fn _append(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "append", 2)?;
    Ok(match (&args[0], &args[1]) {
        (Object::Slice(vec), element) => {
            vec.borrow_mut().push(element.to_owned());
            Object::Empty
        }
        _ => return Err(Error::new("param type error, not be list".to_string())),
    })
}

fn _join(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "join", 2)?;
    let separator = match &args[1] {
        Object::String(value) => value,
        _ => return Err(Error::new("separator must be string".to_string())),
    };
    match &args[0] {
        Object::Slice(vec) => {
            let values = vec
                .borrow()
                .iter()
                .map(stringify_join_element)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Object::String(values.join(separator)))
        }
        _ => Err(Error::new("first param must be list".to_string())),
    }
}

fn stringify_join_element(value: &Object) -> Result<String, Error> {
    match value {
        Object::Integer(value) => Ok(value.to_string()),
        Object::Float(value) => Ok(value.to_string()),
        Object::Boolean(value) => Ok(value.to_string()),
        Object::Null => Ok("null".to_string()),
        Object::String(value) => Ok(value.to_owned()),
        Object::Empty => Ok(String::new()),
        _ => Err(Error::new("unsupported join element type".to_string())),
    }
}

fn _read_file(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "read_file", 1)?;
    let path = expect_string(&args[0], "path")?;
    Ok(match fs::read_to_string(path) {
        Ok(content) => result_ok(Object::String(content)),
        Err(err) => io_error_result(err),
    })
}

fn _write_file(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "write_file", 2)?;
    let path = expect_string(&args[0], "path")?;
    let content = expect_string(&args[1], "content")?;
    Ok(match fs::write(path, content) {
        Ok(_) => result_ok(Object::Integer(content.len() as i64)),
        Err(err) => io_error_result(err),
    })
}

fn _append_file(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "append_file", 2)?;
    let path = expect_string(&args[0], "path")?;
    let content = expect_string(&args[1], "content")?;
    Ok(
        match OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .and_then(|mut file| file.write_all(content.as_bytes()))
        {
            Ok(_) => result_ok(Object::Integer(content.len() as i64)),
            Err(err) => io_error_result(err),
        },
    )
}

fn _file_exists(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "file_exists", 1)?;
    let path = expect_string(&args[0], "path")?;
    Ok(Object::Boolean(Path::new(path).exists()))
}

fn _read_dir(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "read_dir", 1)?;
    let path = expect_string(&args[0], "path")?;
    let entries_iter = match fs::read_dir(path) {
        Ok(entries) => entries,
        Err(err) => return Ok(io_error_result(err)),
    };
    let mut entries = Vec::new();
    for entry in entries_iter {
        match entry {
            Ok(entry) => entries.push(Object::String(
                entry.file_name().to_string_lossy().into_owned(),
            )),
            Err(err) => return Ok(io_error_result(err)),
        }
    }
    entries.sort_by_key(|entry| match entry {
        Object::String(value) => value.to_owned(),
        _ => String::new(),
    });
    Ok(result_ok(Object::Slice(Rc::new(RefCell::new(entries)))))
}

fn _mkdir(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "mkdir", 1)?;
    let path = expect_string(&args[0], "path")?;
    Ok(match fs::create_dir_all(path) {
        Ok(_) => result_ok(Object::Null),
        Err(err) => io_error_result(err),
    })
}

fn _remove_file(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "remove_file", 1)?;
    let path = expect_string(&args[0], "path")?;
    Ok(match fs::remove_file(path) {
        Ok(_) => result_ok(Object::Null),
        Err(err) => io_error_result(err),
    })
}

fn _remove_dir(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "remove_dir", 1)?;
    let path = expect_string(&args[0], "path")?;
    Ok(match fs::remove_dir(path) {
        Ok(_) => result_ok(Object::Null),
        Err(err) => io_error_result(err),
    })
}

fn _copy_file(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "copy_file", 2)?;
    let from = expect_string(&args[0], "from")?;
    let to = expect_string(&args[1], "to")?;
    Ok(match fs::copy(from, to) {
        Ok(bytes) => result_ok(Object::Integer(bytes as i64)),
        Err(err) => io_error_result(err),
    })
}

fn _rename(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "rename", 2)?;
    let from = expect_string(&args[0], "from")?;
    let to = expect_string(&args[1], "to")?;
    Ok(match fs::rename(from, to) {
        Ok(_) => result_ok(Object::Null),
        Err(err) => io_error_result(err),
    })
}

fn _metadata(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "metadata", 1)?;
    let path = expect_string(&args[0], "path")?;
    let metadata = match fs::metadata(path) {
        Ok(metadata) => metadata,
        Err(err) if err.kind() == io::ErrorKind::NotFound => {
            return Ok(result_ok(object_map([
                ("exists", Object::Boolean(false)),
                ("is_file", Object::Boolean(false)),
                ("is_dir", Object::Boolean(false)),
                ("size", Object::Integer(0)),
                ("modified_ms", Object::Integer(0)),
            ])));
        }
        Err(err) => return Ok(io_error_result(err)),
    };
    let modified_ms = metadata
        .modified()
        .ok()
        .and_then(|time| time.duration_since(UNIX_EPOCH).ok())
        .map(|duration| duration.as_millis() as i64)
        .unwrap_or(0);
    Ok(result_ok(object_map([
        ("exists", Object::Boolean(true)),
        ("is_file", Object::Boolean(metadata.is_file())),
        ("is_dir", Object::Boolean(metadata.is_dir())),
        ("size", Object::Integer(metadata.len() as i64)),
        ("modified_ms", Object::Integer(modified_ms)),
    ])))
}

fn _path_join(args: &[Object]) -> Result<Object, Error> {
    if args.is_empty() {
        return Err(Error::new(
            "path_join expects at least one argument".to_string(),
        ));
    }
    let mut path = PathBuf::new();
    for (index, arg) in args.iter().enumerate() {
        path.push(expect_string(arg, format!("path[{index}]").as_str())?);
    }
    Ok(Object::String(path.to_string_lossy().into_owned()))
}

fn _path_dirname(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "path_dirname", 1)?;
    let path = expect_string(&args[0], "path")?;
    Ok(Object::String(
        Path::new(path)
            .parent()
            .map(|path| path.to_string_lossy().into_owned())
            .unwrap_or_default(),
    ))
}

fn _path_basename(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "path_basename", 1)?;
    let path = expect_string(&args[0], "path")?;
    Ok(Object::String(
        Path::new(path)
            .file_name()
            .map(|name| name.to_string_lossy().into_owned())
            .unwrap_or_default(),
    ))
}

fn _path_ext(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "path_ext", 1)?;
    let path = expect_string(&args[0], "path")?;
    Ok(Object::String(
        Path::new(path)
            .extension()
            .map(|ext| ext.to_string_lossy().into_owned())
            .unwrap_or_default(),
    ))
}

fn _path_exists(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "path_exists", 1)?;
    let path = expect_string(&args[0], "path")?;
    Ok(Object::Boolean(Path::new(path).exists()))
}

fn _path_is_file(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "path_is_file", 1)?;
    let path = expect_string(&args[0], "path")?;
    Ok(Object::Boolean(Path::new(path).is_file()))
}

fn _path_is_dir(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "path_is_dir", 1)?;
    let path = expect_string(&args[0], "path")?;
    Ok(Object::Boolean(Path::new(path).is_dir()))
}

fn _env_get(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "env_get", 1)?;
    let name = expect_string(&args[0], "name")?;
    Ok(env::var(name).map(Object::String).unwrap_or(Object::Null))
}

fn _env_set(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "env_set", 2)?;
    let name = expect_string(&args[0], "name")?;
    let value = expect_string(&args[1], "value")?;
    env::set_var(name, value);
    Ok(Object::Empty)
}

fn _cwd(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "cwd", 0)?;
    Ok(match env::current_dir() {
        Ok(path) => result_ok(Object::String(path.to_string_lossy().into_owned())),
        Err(err) => io_error_result(err),
    })
}

fn _set_cwd(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "set_cwd", 1)?;
    let path = expect_string(&args[0], "path")?;
    Ok(match env::set_current_dir(path) {
        Ok(_) => result_ok(Object::Null),
        Err(err) => io_error_result(err),
    })
}

fn _exit(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "exit", 1)?;
    let code = expect_int(&args[0], "code")?;
    std::process::exit(code as i32);
}

fn _exec(args: &[Object]) -> Result<Object, Error> {
    if args.len() != 1 && args.len() != 2 {
        return Err(Error::new(format!(
            "exec expects 1 or 2 arguments, got {}",
            args.len()
        )));
    }
    let command = expect_string(&args[0], "command")?;
    let command_args = optional_string_list(args.get(1), "args")?;
    let output = match Command::new(command).args(command_args.iter()).output() {
        Ok(output) => output,
        Err(err) => return Ok(result_err("process", "spawn", err.to_string())),
    };
    Ok(result_ok(object_map([
        ("success", Object::Boolean(output.status.success())),
        (
            "status",
            Object::Integer(output.status.code().unwrap_or(-1) as i64),
        ),
        (
            "stdout",
            Object::String(String::from_utf8_lossy(&output.stdout).into_owned()),
        ),
        (
            "stderr",
            Object::String(String::from_utf8_lossy(&output.stderr).into_owned()),
        ),
    ])))
}

fn _time_ms(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "time_ms", 0)?;
    current_time_ms()
}

fn _now_ms(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "now_ms", 0)?;
    current_time_ms()
}

fn current_time_ms() -> Result<Object, Error> {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|err| Error::new(err.to_string()))?
        .as_millis();
    Ok(Object::Integer(millis as i64))
}

fn _sleep_ms(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "sleep_ms", 1)?;
    let millis = expect_int(&args[0], "millis")?;
    if millis < 0 {
        return Err(Error::new("millis must be non-negative".to_string()));
    }
    std::thread::sleep(Duration::from_millis(millis as u64));
    Ok(Object::Empty)
}

fn _clear(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "clear", 0)?;
    let mut stdout = io::stdout();
    execute!(
        stdout,
        ct_terminal::Clear(ClearType::All),
        cursor::MoveTo(0, 0)
    )
    .map_err(terminal_io_error)?;
    Ok(Object::Empty)
}

fn _home(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "home", 0)?;
    let mut stdout = io::stdout();
    execute!(stdout, cursor::MoveTo(0, 0)).map_err(terminal_io_error)?;
    Ok(Object::Empty)
}

fn _hide_cursor(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "hide_cursor", 0)?;
    let mut stdout = io::stdout();
    execute!(stdout, cursor::Hide).map_err(terminal_io_error)?;
    Ok(Object::Empty)
}

fn _show_cursor(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "show_cursor", 0)?;
    let mut stdout = io::stdout();
    execute!(stdout, cursor::Show).map_err(terminal_io_error)?;
    Ok(Object::Empty)
}

fn _enable_raw_mode(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "enable_raw_mode", 0)?;
    ct_terminal::enable_raw_mode().map_err(terminal_io_error)?;
    Ok(Object::Empty)
}

fn _disable_raw_mode(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "disable_raw_mode", 0)?;
    ct_terminal::disable_raw_mode().map_err(terminal_io_error)?;
    Ok(Object::Empty)
}

fn _read_key(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "read_key", 0)?;
    loop {
        match event::read() {
            Ok(Event::Key(key)) if is_key_press(key) => return Ok(result_ok(key_event_value(key))),
            Ok(_) => continue,
            Err(err) => return Ok(result_err("terminal", "read_key", err.to_string())),
        }
    }
}

fn _read_key_timeout(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "read_key_timeout", 1)?;
    let millis = expect_int(&args[0], "millis")?;
    if millis < 0 {
        return Err(Error::new("millis must be non-negative".to_string()));
    }

    let timeout = Duration::from_millis(millis as u64);
    let start = Instant::now();
    loop {
        let Some(remaining) = timeout.checked_sub(start.elapsed()) else {
            return Ok(result_ok(Object::Null));
        };
        match event::poll(remaining) {
            Ok(false) => return Ok(result_ok(Object::Null)),
            Ok(true) => match event::read() {
                Ok(Event::Key(key)) if is_key_press(key) => {
                    return Ok(result_ok(key_event_value(key)));
                }
                Ok(_) => continue,
                Err(err) => return Ok(result_err("terminal", "read_key", err.to_string())),
            },
            Err(err) => return Ok(result_err("terminal", "poll", err.to_string())),
        }
    }
}

fn _read_key_latest_timeout(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "read_key_latest_timeout", 1)?;
    let millis = expect_int(&args[0], "millis")?;
    if millis < 0 {
        return Err(Error::new("millis must be non-negative".to_string()));
    }

    let timeout = Duration::from_millis(millis as u64);
    let start = Instant::now();
    loop {
        let Some(remaining) = timeout.checked_sub(start.elapsed()) else {
            return Ok(result_ok(Object::Null));
        };
        match event::poll(remaining) {
            Ok(false) => return Ok(result_ok(Object::Null)),
            Ok(true) => match event::read() {
                Ok(Event::Key(key)) if is_key_press(key) => {
                    let mut latest = key;
                    loop {
                        match event::poll(Duration::from_millis(0)) {
                            Ok(false) => return Ok(result_ok(key_event_value(latest))),
                            Ok(true) => match event::read() {
                                Ok(Event::Key(key)) if is_key_press(key) => latest = key,
                                Ok(_) => continue,
                                Err(err) => {
                                    return Ok(result_err(
                                        "terminal",
                                        "read_key_latest_timeout",
                                        err.to_string(),
                                    ));
                                }
                            },
                            Err(err) => {
                                return Ok(result_err("terminal", "poll", err.to_string()));
                            }
                        }
                    }
                }
                Ok(_) => continue,
                Err(err) => {
                    return Ok(result_err(
                        "terminal",
                        "read_key_latest_timeout",
                        err.to_string(),
                    ));
                }
            },
            Err(err) => return Ok(result_err("terminal", "poll", err.to_string())),
        }
    }
}

fn _move_cursor(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "move", 2)?;
    let row = expect_terminal_coordinate(&args[0], "row")?;
    let col = expect_terminal_coordinate(&args[1], "col")?;
    let mut stdout = io::stdout();
    execute!(stdout, cursor::MoveTo(col, row)).map_err(terminal_io_error)?;
    Ok(Object::Empty)
}

fn _clear_line(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "clear_line", 0)?;
    let mut stdout = io::stdout();
    execute!(stdout, ct_terminal::Clear(ClearType::CurrentLine)).map_err(terminal_io_error)?;
    Ok(Object::Empty)
}

fn _terminal_size(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "size", 0)?;
    match ct_terminal::size() {
        Ok((cols, rows)) => Ok(result_ok(object_map([
            ("cols", Object::Integer(i64::from(cols))),
            ("rows", Object::Integer(i64::from(rows))),
        ]))),
        Err(err) => Ok(result_err("terminal", "size", err.to_string())),
    }
}

fn _enter_alt_screen(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "enter_alt_screen", 0)?;
    let mut stdout = io::stdout();
    execute!(stdout, ct_terminal::EnterAlternateScreen).map_err(terminal_io_error)?;
    Ok(Object::Empty)
}

fn _leave_alt_screen(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "leave_alt_screen", 0)?;
    let mut stdout = io::stdout();
    execute!(stdout, ct_terminal::LeaveAlternateScreen).map_err(terminal_io_error)?;
    Ok(Object::Empty)
}

fn _terminal_fg(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "fg", 1)?;
    let color = expect_terminal_color(&args[0], "color")?;
    let mut stdout = io::stdout();
    execute!(stdout, SetForegroundColor(color)).map_err(terminal_io_error)?;
    Ok(Object::Empty)
}

fn _terminal_bg(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "bg", 1)?;
    let color = expect_terminal_color(&args[0], "color")?;
    let mut stdout = io::stdout();
    execute!(stdout, SetBackgroundColor(color)).map_err(terminal_io_error)?;
    Ok(Object::Empty)
}

fn _terminal_bold(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "bold", 0)?;
    let mut stdout = io::stdout();
    execute!(stdout, SetAttribute(Attribute::Bold)).map_err(terminal_io_error)?;
    Ok(Object::Empty)
}

fn _terminal_reset_style(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "reset_style", 0)?;
    let mut stdout = io::stdout();
    execute!(stdout, ResetColor, SetAttribute(Attribute::Reset)).map_err(terminal_io_error)?;
    Ok(Object::Empty)
}

fn _terminal_paint(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "paint", 2)?;
    let code = expect_terminal_ansi_color_code(&args[0], "color")?;
    let text = expect_string(&args[1], "text")?;
    Ok(Object::String(format!("\x1b[{code}m{text}\x1b[0m")))
}

fn _terminal_paint_runs(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "paint_runs", 1)?;
    let Object::Slice(runs) = &args[0] else {
        return Err(Error::with_kind(
            ErrorKind::Type,
            "runs must be list".to_string(),
        ));
    };
    let mut out = String::new();
    for (index, run) in runs.borrow().iter().enumerate() {
        let Object::Map(map) = run else {
            return Err(Error::with_kind(
                ErrorKind::Type,
                format!("runs[{index}] must be map"),
            ));
        };
        let map = map.borrow();
        let color = map
            .get(&AbleToMapKey::from("color"))
            .ok_or_else(|| Error::new(format!("runs[{index}].color is required")))?;
        let text = map
            .get(&AbleToMapKey::from("text"))
            .ok_or_else(|| Error::new(format!("runs[{index}].text is required")))?;
        let code = expect_terminal_ansi_color_code(color, format!("runs[{index}].color").as_str())?;
        let text = expect_string(text, format!("runs[{index}].text").as_str())?;
        out.push_str(format!("\x1b[{code}m{text}\x1b[0m").as_str());
    }
    Ok(Object::String(out))
}

fn terminal_io_error(err: io::Error) -> Error {
    Error::with_kind(ErrorKind::Io, err.to_string())
}

fn expect_terminal_ansi_color_code(value: &Object, name: &str) -> Result<&'static str, Error> {
    let color = expect_string(value, name)?;
    match color {
        "black" => Ok("30"),
        "red" => Ok("31"),
        "green" => Ok("32"),
        "yellow" => Ok("33"),
        "blue" => Ok("34"),
        "magenta" => Ok("35"),
        "cyan" => Ok("36"),
        "white" => Ok("37"),
        "gray" | "grey" => Ok("37"),
        "dark_gray" | "dark_grey" => Ok("90"),
        "bright_red" => Ok("91"),
        "bright_green" => Ok("92"),
        "bright_yellow" => Ok("93"),
        "bright_blue" => Ok("94"),
        "bright_magenta" => Ok("95"),
        "bright_cyan" => Ok("96"),
        "bright_white" => Ok("97"),
        _ => Err(Error::with_kind(
            ErrorKind::Type,
            format!("unknown terminal color '{color}'"),
        )),
    }
}

fn expect_terminal_color(value: &Object, name: &str) -> Result<Color, Error> {
    let color = expect_string(value, name)?;
    match color {
        "black" => Ok(Color::Black),
        "red" => Ok(Color::DarkRed),
        "green" => Ok(Color::DarkGreen),
        "yellow" => Ok(Color::DarkYellow),
        "blue" => Ok(Color::DarkBlue),
        "magenta" => Ok(Color::DarkMagenta),
        "cyan" => Ok(Color::DarkCyan),
        "white" => Ok(Color::White),
        "gray" | "grey" => Ok(Color::Grey),
        "dark_gray" | "dark_grey" => Ok(Color::DarkGrey),
        "bright_red" => Ok(Color::Red),
        "bright_green" => Ok(Color::Green),
        "bright_yellow" => Ok(Color::Yellow),
        "bright_blue" => Ok(Color::Blue),
        "bright_magenta" => Ok(Color::Magenta),
        "bright_cyan" => Ok(Color::Cyan),
        "bright_white" => Ok(Color::White),
        _ => Err(Error::with_kind(
            ErrorKind::Type,
            format!("unknown terminal color '{color}'"),
        )),
    }
}

fn expect_terminal_coordinate(value: &Object, name: &str) -> Result<u16, Error> {
    let coordinate = expect_int(value, name)?;
    if coordinate < 0 {
        return Err(Error::new(format!("{name} must be non-negative")));
    }
    u16::try_from(coordinate).map_err(|_| Error::new(format!("{name} must fit in u16")))
}

fn key_event_value(event: KeyEvent) -> Object {
    let (kind, key) = key_code_parts(event.code);
    let modifiers = event.modifiers;
    object_map([
        ("kind", Object::String(kind)),
        ("key", Object::String(key)),
        (
            "ctrl",
            Object::Boolean(modifiers.contains(KeyModifiers::CONTROL)),
        ),
        (
            "alt",
            Object::Boolean(modifiers.contains(KeyModifiers::ALT)),
        ),
        (
            "shift",
            Object::Boolean(modifiers.contains(KeyModifiers::SHIFT)),
        ),
    ])
}

fn is_key_press(event: KeyEvent) -> bool {
    matches!(event.kind, KeyEventKind::Press | KeyEventKind::Repeat)
}

fn key_code_parts(code: KeyCode) -> (String, String) {
    match code {
        KeyCode::Backspace => key_name("backspace"),
        KeyCode::Enter => key_name("enter"),
        KeyCode::Left => key_name("left"),
        KeyCode::Right => key_name("right"),
        KeyCode::Up => key_name("up"),
        KeyCode::Down => key_name("down"),
        KeyCode::Home => key_name("home"),
        KeyCode::End => key_name("end"),
        KeyCode::PageUp => key_name("page_up"),
        KeyCode::PageDown => key_name("page_down"),
        KeyCode::Tab => key_name("tab"),
        KeyCode::BackTab => key_name("back_tab"),
        KeyCode::Delete => key_name("delete"),
        KeyCode::Insert => key_name("insert"),
        KeyCode::Esc => key_name("esc"),
        KeyCode::Null => key_name("null"),
        KeyCode::F(index) => ("function".to_string(), format!("f{index}")),
        KeyCode::Char(ch) => ("char".to_string(), ch.to_string()),
        other => ("other".to_string(), format!("{other:?}")),
    }
}

fn key_name(name: &str) -> (String, String) {
    (name.to_string(), name.to_string())
}

fn _args(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "args", 0)?;
    Ok(Object::Slice(std::rc::Rc::new(std::cell::RefCell::new(
        env::args().map(Object::String).collect(),
    ))))
}

fn _read_line(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "read_line", 0)?;
    let mut line = String::new();
    if let Err(err) = io::stdin().read_line(&mut line) {
        return Ok(io_error_result(err));
    }
    if line.ends_with('\n') {
        line.pop();
        if line.ends_with('\r') {
            line.pop();
        }
    }
    Ok(result_ok(Object::String(line)))
}

fn _panic(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "panic", 1)?;
    Err(Error::with_kind(
        ErrorKind::Panic,
        expect_string(&args[0], "message")?.to_string(),
    ))
}

fn _assert(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "assert", 2)?;
    match args[0] {
        Object::Boolean(true) => Ok(Object::Empty),
        Object::Boolean(false) => Err(Error::with_kind(
            ErrorKind::Assert,
            expect_string(&args[1], "message")?.to_string(),
        )),
        _ => Err(Error::with_kind(
            ErrorKind::Type,
            "condition must be bool".to_string(),
        )),
    }
}

fn _substr(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "substr", 3)?;
    let text = expect_string(&args[0], "text")?;
    let start = expect_int(&args[1], "start")?;
    let len = expect_int(&args[2], "len")?;
    if start < 0 || len < 0 {
        return Err(Error::new("start and len must be non-negative".to_string()));
    }
    Ok(Object::String(
        text.chars()
            .skip(start as usize)
            .take(len as usize)
            .collect(),
    ))
}

fn _find(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "find", 2)?;
    let text = expect_string(&args[0], "text")?;
    let needle = expect_string(&args[1], "needle")?;
    Ok(Object::Integer(
        text.find(needle)
            .map(|index| text[..index].chars().count() as i64)
            .unwrap_or(-1),
    ))
}

fn _replace(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "replace", 3)?;
    let text = expect_string(&args[0], "text")?;
    let from = expect_string(&args[1], "from")?;
    let to = expect_string(&args[2], "to")?;
    Ok(Object::String(text.replace(from, to)))
}

fn _char_code(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "char_code", 1)?;
    let text = expect_string(&args[0], "text")?;
    let mut chars = text.chars();
    let ch = chars
        .next()
        .ok_or_else(|| Error::new("text must contain one character".to_string()))?;
    if chars.next().is_some() {
        return Err(Error::new("text must contain one character".to_string()));
    }
    Ok(Object::Integer(ch as i64))
}

fn _from_char_code(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "from_char_code", 1)?;
    let code = expect_int(&args[0], "code")?;
    let ch = char::from_u32(code as u32)
        .ok_or_else(|| Error::new("invalid character code".to_string()))?;
    Ok(Object::String(ch.to_string()))
}

fn expect_string<'a>(value: &'a Object, name: &str) -> Result<&'a str, Error> {
    match value {
        Object::String(value) => Ok(value),
        _ => Err(Error::with_kind(
            ErrorKind::Type,
            format!("{name} must be string"),
        )),
    }
}

fn expect_int(value: &Object, name: &str) -> Result<i64, Error> {
    match value {
        Object::Integer(value) => Ok(*value),
        _ => Err(Error::with_kind(
            ErrorKind::Type,
            format!("{name} must be int"),
        )),
    }
}

fn optional_string_list(value: Option<&Object>, name: &str) -> Result<Vec<String>, Error> {
    match value {
        Some(Object::Slice(values)) => values
            .borrow()
            .iter()
            .enumerate()
            .map(|(index, value)| {
                expect_string(value, format!("{name}[{index}]").as_str()).map(str::to_string)
            })
            .collect(),
        Some(_) => Err(Error::with_kind(
            ErrorKind::Type,
            format!("{name} must be list"),
        )),
        None => Ok(Vec::new()),
    }
}

fn object_map<const N: usize>(entries: [(&str, Object); N]) -> Object {
    Object::Map(Rc::new(RefCell::new(
        entries
            .into_iter()
            .map(|(key, value)| (AbleToMapKey::from(key), value))
            .collect(),
    )))
}

fn result_ok(value: Object) -> Object {
    object_map([
        ("ok", Object::Boolean(true)),
        ("value", value),
        ("error", Object::Null),
    ])
}

fn result_err(kind: &str, code: &str, message: String) -> Object {
    object_map([
        ("ok", Object::Boolean(false)),
        ("value", Object::Null),
        (
            "error",
            object_map([
                ("kind", Object::String(kind.to_string())),
                ("code", Object::String(code.to_string())),
                ("message", Object::String(message)),
            ]),
        ),
    ])
}

fn io_error_result(err: io::Error) -> Object {
    let code = match err.kind() {
        io::ErrorKind::NotFound => "not_found",
        io::ErrorKind::PermissionDenied => "permission_denied",
        io::ErrorKind::AlreadyExists => "already_exists",
        io::ErrorKind::InvalidInput => "invalid_input",
        io::ErrorKind::InvalidData => "invalid_data",
        io::ErrorKind::TimedOut => "timed_out",
        io::ErrorKind::Interrupted => "interrupted",
        io::ErrorKind::WouldBlock => "would_block",
        io::ErrorKind::UnexpectedEof => "unexpected_eof",
        _ => "io_error",
    };
    result_err("io", code, err.to_string())
}

fn expect_number(value: &Object, name: &str) -> Result<(f64, Option<i64>), Error> {
    match value {
        Object::Integer(value) => Ok((*value as f64, Some(*value))),
        Object::Float(value) => Ok((*value, None)),
        _ => Err(Error::with_kind(
            ErrorKind::Type,
            format!("{name} must be int or float"),
        )),
    }
}

fn _abs(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "abs", 1)?;
    match args[0] {
        Object::Integer(value) => value
            .checked_abs()
            .map(Object::Integer)
            .ok_or_else(|| Error::new("abs overflow".to_string())),
        Object::Float(value) => Ok(Object::Float(value.abs())),
        _ => Err(Error::with_kind(
            ErrorKind::Type,
            "value must be int or float".to_string(),
        )),
    }
}

fn _floor(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "floor", 1)?;
    Ok(Object::Float(expect_number(&args[0], "value")?.0.floor()))
}

fn _ceil(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "ceil", 1)?;
    Ok(Object::Float(expect_number(&args[0], "value")?.0.ceil()))
}

fn _round(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "round", 1)?;
    Ok(Object::Float(expect_number(&args[0], "value")?.0.round()))
}

fn _sqrt(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "sqrt", 1)?;
    let value = expect_number(&args[0], "value")?.0;
    if value < 0.0 {
        return Err(Error::new("sqrt value must be non-negative".to_string()));
    }
    Ok(Object::Float(value.sqrt()))
}

fn _pow(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "pow", 2)?;
    let base = expect_number(&args[0], "base")?.0;
    let exponent = expect_number(&args[1], "exponent")?.0;
    Ok(Object::Float(base.powf(exponent)))
}

fn _min(args: &[Object]) -> Result<Object, Error> {
    min_or_max(args, "min", |candidate, best| candidate < best)
}

fn _max(args: &[Object]) -> Result<Object, Error> {
    min_or_max(args, "max", |candidate, best| candidate > best)
}

fn _random_int(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "random_int", 2)?;
    let min = expect_int(&args[0], "min")?;
    let max = expect_int(&args[1], "max")?;
    if min > max {
        return Err(Error::new("min must be <= max".to_string()));
    }
    Ok(Object::Integer(rand::thread_rng().gen_range(min..=max)))
}

fn _random_float(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "random_float", 0)?;
    Ok(Object::Float(rand::thread_rng().gen_range(0.0..1.0)))
}

fn _sort(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "sort", 1)?;
    let Object::Slice(values) = &args[0] else {
        return Err(Error::with_kind(
            ErrorKind::Type,
            "value must be list".to_string(),
        ));
    };
    let mut values = values
        .borrow()
        .iter()
        .map(Object::clone_value)
        .collect::<Vec<_>>();
    sort_values(&mut values)?;
    Ok(Object::Slice(Rc::new(RefCell::new(values))))
}

fn sort_values(values: &mut [Object]) -> Result<(), Error> {
    if values.is_empty() {
        return Ok(());
    }
    if values
        .iter()
        .all(|value| matches!(value, Object::Integer(_) | Object::Float(_)))
    {
        values.sort_by(|left, right| {
            number_for_sort(left)
                .partial_cmp(&number_for_sort(right))
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        return Ok(());
    }
    if values
        .iter()
        .all(|value| matches!(value, Object::String(_)))
    {
        values.sort_by(|left, right| match (left, right) {
            (Object::String(left), Object::String(right)) => left.cmp(right),
            _ => unreachable!(),
        });
        return Ok(());
    }
    if values
        .iter()
        .all(|value| matches!(value, Object::Boolean(_)))
    {
        values.sort_by_key(|value| match value {
            Object::Boolean(value) => *value,
            _ => unreachable!(),
        });
        return Ok(());
    }
    Err(Error::with_kind(
        ErrorKind::Type,
        "sort supports lists of numbers, strings, or bools".to_string(),
    ))
}

fn number_for_sort(value: &Object) -> f64 {
    match value {
        Object::Integer(value) => *value as f64,
        Object::Float(value) => *value,
        _ => unreachable!(),
    }
}

fn min_or_max(
    args: &[Object],
    function: &str,
    better: fn(candidate: f64, best: f64) -> bool,
) -> Result<Object, Error> {
    if args.is_empty() {
        return Err(Error::new(format!(
            "{function} expects at least one argument"
        )));
    }

    let (mut best, mut best_int) = expect_number(&args[0], "value")?;
    let mut all_ints = best_int.is_some();
    for value in &args[1..] {
        let (candidate, candidate_int) = expect_number(value, "value")?;
        all_ints &= candidate_int.is_some();
        if better(candidate, best) {
            best = candidate;
            best_int = candidate_int;
        }
    }

    match (all_ints, best_int) {
        (true, Some(value)) => Ok(Object::Integer(value)),
        _ => Ok(Object::Float(best)),
    }
}

fn _http_get(args: &[Object]) -> Result<Object, Error> {
    if args.len() != 1 && args.len() != 2 {
        return Err(Error::new(format!(
            "http_get expects 1 or 2 arguments, got {}",
            args.len()
        )));
    }
    let url = expect_string(&args[0], "url")?;
    let headers = optional_headers(args.get(1))?;
    http_get(url, &headers)
}

fn _http_post(args: &[Object]) -> Result<Object, Error> {
    if args.len() != 2 && args.len() != 3 {
        return Err(Error::new(format!(
            "http_post expects 2 or 3 arguments, got {}",
            args.len()
        )));
    }
    let url = expect_string(&args[0], "url")?;
    let body = expect_string(&args[1], "body")?;
    let headers = optional_headers(args.get(2))?;
    http_post(url, body, &headers)
}

fn _http_request(args: &[Object]) -> Result<Object, Error> {
    if args.len() != 3 && args.len() != 4 {
        return Err(Error::new(format!(
            "http_request expects 3 or 4 arguments, got {}",
            args.len()
        )));
    }
    let method = expect_string(&args[0], "method")?;
    let url = expect_string(&args[1], "url")?;
    let body = expect_string(&args[2], "body")?;
    let headers = optional_headers(args.get(3))?;
    http_request(method, url, body, &headers)
}

fn http_get(url: &str, headers: &[(String, String)]) -> Result<Object, Error> {
    let client = match http_client() {
        Ok(client) => client,
        Err(err) => return Ok(result_err("http", "client", err.to_string())),
    };
    match apply_headers(client.get(url), headers).send() {
        Ok(response) => response_to_object(response),
        Err(err) => Ok(result_err("http", "request", err.to_string())),
    }
}

fn http_post(url: &str, body: &str, headers: &[(String, String)]) -> Result<Object, Error> {
    let client = match http_client() {
        Ok(client) => client,
        Err(err) => return Ok(result_err("http", "client", err.to_string())),
    };
    let request = client
        .post(url)
        .header("content-type", "text/plain; charset=utf-8")
        .body(body.to_string());
    match apply_headers(request, headers).send() {
        Ok(response) => response_to_object(response),
        Err(err) => Ok(result_err("http", "request", err.to_string())),
    }
}

fn http_request(
    method: &str,
    url: &str,
    body: &str,
    headers: &[(String, String)],
) -> Result<Object, Error> {
    let client = match http_client() {
        Ok(client) => client,
        Err(err) => return Ok(result_err("http", "client", err.to_string())),
    };
    let method = match reqwest::Method::from_bytes(method.to_uppercase().as_bytes()) {
        Ok(method) => method,
        Err(err) => return Ok(result_err("http", "invalid_method", err.to_string())),
    };
    let mut request = client.request(method, url);
    if !body.is_empty() {
        request = request.body(body.to_string());
    }
    match apply_headers(request, headers).send() {
        Ok(response) => response_to_object(response),
        Err(err) => Ok(result_err("http", "request", err.to_string())),
    }
}

fn optional_headers(value: Option<&Object>) -> Result<Vec<(String, String)>, Error> {
    match value {
        Some(Object::Map(headers)) => headers
            .borrow()
            .iter()
            .map(|(key, value)| Ok((json_map_key(key), http_header_value(value)?)))
            .collect(),
        Some(_) => Err(Error::with_kind(
            ErrorKind::Type,
            "headers must be map".to_string(),
        )),
        None => Ok(Vec::new()),
    }
}

fn http_header_value(value: &Object) -> Result<String, Error> {
    match value {
        Object::Integer(value) => Ok(value.to_string()),
        Object::Float(value) => Ok(value.to_string()),
        Object::Boolean(value) => Ok(value.to_string()),
        Object::Null | Object::Empty => Ok(String::new()),
        Object::String(value) => Ok(value.to_owned()),
        _ => Err(Error::with_kind(
            ErrorKind::Type,
            "header value must be scalar".to_string(),
        )),
    }
}

fn apply_headers(
    mut request: reqwest::blocking::RequestBuilder,
    headers: &[(String, String)],
) -> reqwest::blocking::RequestBuilder {
    for (key, value) in headers {
        request = request.header(key, value);
    }
    request
}

fn http_client() -> Result<reqwest::blocking::Client, Error> {
    reqwest::blocking::Client::builder()
        .timeout(Duration::from_secs(10))
        .build()
        .map_err(|err| Error::with_kind(ErrorKind::Io, err.to_string()))
}

fn response_to_object(response: reqwest::blocking::Response) -> Result<Object, Error> {
    let status = response.status().as_u16() as i64;
    let mut headers = HashMap::new();
    for (name, value) in response.headers() {
        headers.insert(
            name.as_str().to_ascii_lowercase(),
            Object::String(value.to_str().unwrap_or("").to_string()),
        );
    }

    match response.text() {
        Ok(body) => Ok(result_ok(http_response_value(
            status,
            headers,
            body.as_str(),
        ))),
        Err(err) => Ok(result_err("http", "body", err.to_string())),
    }
}

fn http_response_value(status: i64, headers: HashMap<String, Object>, body: &str) -> Object {
    let header_map = headers
        .into_iter()
        .map(|(key, value)| (AbleToMapKey::from(key), value))
        .collect();
    let mut map = HashMap::new();
    map.insert(AbleToMapKey::from("status"), Object::Integer(status));
    map.insert(
        AbleToMapKey::from("status_ok"),
        Object::Boolean((200..400).contains(&status)),
    );
    map.insert(
        AbleToMapKey::from("headers"),
        Object::Map(Rc::new(RefCell::new(header_map))),
    );
    map.insert(AbleToMapKey::from("body"), Object::String(body.to_string()));
    Object::Map(Rc::new(RefCell::new(map)))
}

fn _url_encode(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "url_encode", 1)?;
    let value = expect_string(&args[0], "value")?;
    Ok(Object::String(urlencoding::encode(value).into_owned()))
}

fn _url_decode(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "url_decode", 1)?;
    let value = expect_string(&args[0], "value")?;
    Ok(match urlencoding::decode(value) {
        Ok(value) => result_ok(Object::String(value.into_owned())),
        Err(err) => result_err("decode", "invalid_url_encoding", err.to_string()),
    })
}

fn _base64_encode(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "base64_encode", 1)?;
    let value = expect_string(&args[0], "value")?;
    Ok(Object::String(general_purpose::STANDARD.encode(value)))
}

fn _base64_decode(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "base64_decode", 1)?;
    let value = expect_string(&args[0], "value")?;
    let bytes = match general_purpose::STANDARD.decode(value) {
        Ok(bytes) => bytes,
        Err(err) => return Ok(result_err("decode", "invalid_base64", err.to_string())),
    };
    Ok(match String::from_utf8(bytes) {
        Ok(value) => result_ok(Object::String(value)),
        Err(err) => result_err("decode", "invalid_utf8", err.to_string()),
    })
}

fn _sha256(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "sha256", 1)?;
    let value = expect_string(&args[0], "value")?;
    let mut hasher = Sha256::new();
    hasher.update(value.as_bytes());
    Ok(Object::String(format!("{:x}", hasher.finalize())))
}

fn _json_stringify(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "json_stringify", 1)?;
    Ok(Object::String(json_stringify_value(&args[0])?))
}

fn json_stringify_value(value: &Object) -> Result<String, Error> {
    json_stringify_value_with_seen(value, &mut HashSet::new())
}

fn json_stringify_value_with_seen(
    value: &Object,
    seen: &mut HashSet<usize>,
) -> Result<String, Error> {
    match value {
        Object::Integer(value) => Ok(value.to_string()),
        Object::Float(value) => {
            if !value.is_finite() {
                return Err(Error::new("cannot stringify non-finite float".to_string()));
            }
            Ok(value.to_string())
        }
        Object::Boolean(value) => Ok(value.to_string()),
        Object::Null | Object::Empty => Ok("null".to_string()),
        Object::String(value) => Ok(json_quote(value)),
        Object::Slice(values) => {
            let id = rc_id(values);
            if !seen.insert(id) {
                return Err(json_cycle_error());
            }
            let result = {
                let values = values.borrow();
                let values = values
                    .iter()
                    .map(|value| json_stringify_value_with_seen(value, seen))
                    .collect::<Result<Vec<_>, _>>();
                values.map(|values| format!("[{}]", values.join(",")))
            };
            seen.remove(&id);
            result
        }
        Object::Map(values) => {
            let id = rc_id(values);
            if !seen.insert(id) {
                return Err(json_cycle_error());
            }
            let result = json_stringify_map(&values.borrow(), seen);
            seen.remove(&id);
            result
        }
        Object::StructInstance(instance) => {
            let id = rc_id(instance);
            if !seen.insert(id) {
                return Err(json_cycle_error());
            }
            let result = {
                let instance = instance.borrow();
                let mut values = Vec::new();
                for field in instance.fields.iter() {
                    let key = AbleToMapKey::String(field.to_owned());
                    let value = instance.values.get(&key).unwrap_or(&Object::Null);
                    values.push(format!(
                        "{}:{}",
                        json_quote(field),
                        json_stringify_value_with_seen(value, seen)?
                    ));
                }
                Ok(format!("{{{}}}", values.join(",")))
            };
            seen.remove(&id);
            result
        }
        _ => Err(Error::with_kind(
            ErrorKind::Type,
            format!("cannot stringify {}", value.type_name()),
        )),
    }
}

fn json_stringify_map(
    values: &HashMap<AbleToMapKey, Object>,
    seen: &mut HashSet<usize>,
) -> Result<String, Error> {
    let mut entries = values
        .iter()
        .map(|(key, value)| {
            Ok((
                json_map_key(key),
                json_stringify_value_with_seen(value, seen)?,
            ))
        })
        .collect::<Result<Vec<_>, Error>>()?;
    entries.sort_by(|left, right| left.0.cmp(&right.0));
    Ok(format!(
        "{{{}}}",
        entries
            .into_iter()
            .map(|(key, value)| format!("{}:{}", json_quote(key.as_str()), value))
            .collect::<Vec<_>>()
            .join(",")
    ))
}

fn rc_id<T>(value: &Rc<RefCell<T>>) -> usize {
    Rc::as_ptr(value) as usize
}

fn json_cycle_error() -> Error {
    Error::with_kind(ErrorKind::Type, "cannot stringify cyclic value".to_string())
}

fn json_map_key(key: &AbleToMapKey) -> String {
    match key {
        AbleToMapKey::Integer(value) => value.to_string(),
        AbleToMapKey::Boolean(value) => value.to_string(),
        AbleToMapKey::String(value) => value.to_owned(),
    }
}

fn json_quote(value: &str) -> String {
    let mut out = String::from("\"");
    for ch in value.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{08}' => out.push_str("\\b"),
            '\u{0c}' => out.push_str("\\f"),
            ch if ch.is_control() => out.push_str(format!("\\u{:04x}", ch as u32).as_str()),
            ch => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn _type_of(args: &[Object]) -> Result<Object, Error> {
    expect_arity(args, "type", 1)?;
    Ok(Object::String(args[0].type_name().to_string()))
}

fn _is_null(args: &[Object]) -> Result<Object, Error> {
    is_type(args, |value| matches!(value, Object::Null))
}

fn _is_bool(args: &[Object]) -> Result<Object, Error> {
    is_type(args, |value| matches!(value, Object::Boolean(_)))
}

fn _is_int(args: &[Object]) -> Result<Object, Error> {
    is_type(args, |value| matches!(value, Object::Integer(_)))
}

fn _is_float(args: &[Object]) -> Result<Object, Error> {
    is_type(args, |value| matches!(value, Object::Float(_)))
}

fn _is_string(args: &[Object]) -> Result<Object, Error> {
    is_type(args, |value| matches!(value, Object::String(_)))
}

fn _is_list(args: &[Object]) -> Result<Object, Error> {
    is_type(args, |value| matches!(value, Object::Slice(_)))
}

fn _is_map(args: &[Object]) -> Result<Object, Error> {
    is_type(args, |value| matches!(value, Object::Map(_)))
}

fn is_type(args: &[Object], predicate: fn(&Object) -> bool) -> Result<Object, Error> {
    expect_arity(args, "is_*", 1)?;
    Ok(Object::Boolean(predicate(&args[0])))
}
