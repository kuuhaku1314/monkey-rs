use super::heap::VmHeap;
use super::program::BuiltinOp;
use super::runtime::value::Value;
use crate::error::{Error, ErrorKind};
use crate::lexer::{Position, Span};
use crate::map_key::AbleToMapKey;
use base64::{engine::general_purpose, Engine};
use crossterm::{
    cursor,
    event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers},
    execute,
    style::{Attribute, Color, ResetColor, SetAttribute, SetBackgroundColor, SetForegroundColor},
    terminal::{self as ct_terminal, ClearType},
};
use rand::Rng;
use serde_json::Value as JsonValue;
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::{self, OpenOptions};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

fn zero_span() -> Span {
    Span::point(Position(0, 0))
}

fn user_value(value: Value) -> Value {
    match value {
        Value::Empty => Value::Null,
        value => value,
    }
}

pub(super) fn call(
    heap: &mut VmHeap,
    op: BuiltinOp,
    args: &[Value],
    span: Span,
) -> Result<Value, Error> {
    match op {
        BuiltinOp::Print => vm_builtin_print(heap, args),
        BuiltinOp::Len => vm_builtin_len(heap, args, span),
        BuiltinOp::ByteLen => vm_builtin_byte_len(heap, args, span),
        BuiltinOp::Str => vm_builtin_str(heap, args, span),
        BuiltinOp::Int => vm_builtin_int(heap, args, span),
        BuiltinOp::Float => vm_builtin_float(heap, args, span),
        BuiltinOp::ParseInt => vm_builtin_parse_int(heap, args, span),
        BuiltinOp::ParseFloat => vm_builtin_parse_float(heap, args, span),
        BuiltinOp::Append => vm_builtin_append(heap, args),
        BuiltinOp::Delete => vm_builtin_delete(heap, args, span),
        BuiltinOp::Join => vm_builtin_join(heap, args, span),
        BuiltinOp::Args => vm_builtin_args(heap, args),
        BuiltinOp::ReadLine => vm_builtin_read_line(heap, args),
        BuiltinOp::Panic => vm_builtin_panic(heap, args, span),
        BuiltinOp::Assert => vm_builtin_assert(heap, args, span),
        BuiltinOp::Substr => vm_builtin_substr(heap, args, span),
        BuiltinOp::Find => vm_builtin_find(heap, args, span),
        BuiltinOp::Replace => vm_builtin_replace(heap, args, span),
        BuiltinOp::CharCode => vm_builtin_char_code(heap, args, span),
        BuiltinOp::FromCharCode => vm_builtin_from_char_code(heap, args, span),
        BuiltinOp::JsonParse => vm_builtin_json_parse(heap, args),
        BuiltinOp::JsonStringify => vm_builtin_json_stringify(heap, args),
        BuiltinOp::Type => vm_builtin_type(heap, args),
        BuiltinOp::IsNull => vm_builtin_is_type(args, |value| matches!(value, Value::Null)),
        BuiltinOp::IsBool => vm_builtin_is_type(args, |value| matches!(value, Value::Boolean(_))),
        BuiltinOp::IsInt => vm_builtin_is_type(args, |value| matches!(value, Value::Integer(_))),
        BuiltinOp::IsFloat => vm_builtin_is_type(args, |value| matches!(value, Value::Float(_))),
        BuiltinOp::IsString => vm_builtin_is_type(args, |value| matches!(value, Value::String(_))),
        BuiltinOp::IsList => vm_builtin_is_type(args, |value| matches!(value, Value::Slice(_))),
        BuiltinOp::IsMap => vm_builtin_is_type(args, |value| matches!(value, Value::Map(_))),
        BuiltinOp::ReadFile => vm_builtin_read_file(heap, args, span),
        BuiltinOp::WriteFile => vm_builtin_write_file(heap, args, span),
        BuiltinOp::AppendFile => vm_builtin_append_file(heap, args, span),
        BuiltinOp::FileExists => vm_builtin_file_exists(heap, args, span),
        BuiltinOp::ReadDir => vm_builtin_read_dir(heap, args, span),
        BuiltinOp::Mkdir => vm_builtin_mkdir(heap, args, span),
        BuiltinOp::RemoveFile => vm_builtin_remove_file(heap, args, span),
        BuiltinOp::RemoveDir => vm_builtin_remove_dir(heap, args, span),
        BuiltinOp::CopyFile => vm_builtin_copy_file(heap, args, span),
        BuiltinOp::Rename => vm_builtin_rename(heap, args, span),
        BuiltinOp::Metadata => vm_builtin_metadata(heap, args, span),
        BuiltinOp::PathJoin => vm_builtin_path_join(heap, args, span),
        BuiltinOp::PathDirname => vm_builtin_path_dirname(heap, args, span),
        BuiltinOp::PathBasename => vm_builtin_path_basename(heap, args, span),
        BuiltinOp::PathExt => vm_builtin_path_ext(heap, args, span),
        BuiltinOp::PathExists => vm_builtin_path_exists(heap, args, span),
        BuiltinOp::PathIsFile => vm_builtin_path_is_file(heap, args, span),
        BuiltinOp::PathIsDir => vm_builtin_path_is_dir(heap, args, span),
        BuiltinOp::EnvGet => vm_builtin_env_get(heap, args, span),
        BuiltinOp::EnvSet => vm_builtin_env_set(heap, args, span),
        BuiltinOp::Cwd => vm_builtin_cwd(heap, args),
        BuiltinOp::SetCwd => vm_builtin_set_cwd(heap, args, span),
        BuiltinOp::Exit => vm_builtin_exit(args, span),
        BuiltinOp::TimeMs | BuiltinOp::NowMs => vm_builtin_time_ms(args),
        BuiltinOp::SleepMs => vm_builtin_sleep_ms(args, span),
        BuiltinOp::Clear => vm_builtin_clear(args),
        BuiltinOp::Home => vm_builtin_home(args),
        BuiltinOp::HideCursor => vm_builtin_hide_cursor(args),
        BuiltinOp::ShowCursor => vm_builtin_show_cursor(args),
        BuiltinOp::EnableRawMode => vm_builtin_enable_raw_mode(args),
        BuiltinOp::DisableRawMode => vm_builtin_disable_raw_mode(args),
        BuiltinOp::ReadKey => vm_builtin_read_key(heap, args),
        BuiltinOp::ReadKeyTimeout => vm_builtin_read_key_timeout(heap, args, span, false),
        BuiltinOp::ReadKeyLatestTimeout => vm_builtin_read_key_timeout(heap, args, span, true),
        BuiltinOp::Move => vm_builtin_move_cursor(args, span),
        BuiltinOp::ClearLine => vm_builtin_clear_line(args),
        BuiltinOp::Size => vm_builtin_terminal_size(heap, args),
        BuiltinOp::EnterAltScreen => vm_builtin_enter_alt_screen(args),
        BuiltinOp::LeaveAltScreen => vm_builtin_leave_alt_screen(args),
        BuiltinOp::Fg => vm_builtin_terminal_fg(heap, args, span),
        BuiltinOp::Bg => vm_builtin_terminal_bg(heap, args, span),
        BuiltinOp::Bold => vm_builtin_terminal_bold(args),
        BuiltinOp::ResetStyle => vm_builtin_terminal_reset_style(args),
        BuiltinOp::Paint => vm_builtin_terminal_paint(heap, args, span),
        BuiltinOp::PaintRuns => vm_builtin_terminal_paint_runs(heap, args, span),
        BuiltinOp::Abs
        | BuiltinOp::Floor
        | BuiltinOp::Ceil
        | BuiltinOp::Round
        | BuiltinOp::Sqrt
        | BuiltinOp::Pow
        | BuiltinOp::Min
        | BuiltinOp::Max => vm_builtin_math_slice(op.name(), args, span),
        BuiltinOp::RandomInt => vm_builtin_random_int(args, span),
        BuiltinOp::RandomFloat => vm_builtin_random_float(args),
        BuiltinOp::Sort => vm_builtin_sort(heap, args, span),
        BuiltinOp::HttpGet => vm_builtin_http_get(heap, args, span),
        BuiltinOp::HttpPost => vm_builtin_http_post(heap, args, span),
        BuiltinOp::HttpRequest => vm_builtin_http_request(heap, args, span),
        BuiltinOp::Exec => vm_builtin_exec(heap, args, span),
        BuiltinOp::UrlEncode => vm_builtin_url_encode(heap, args, span),
        BuiltinOp::UrlDecode => vm_builtin_url_decode(heap, args, span),
        BuiltinOp::Base64Encode => vm_builtin_base64_encode(heap, args, span),
        BuiltinOp::Base64Decode => vm_builtin_base64_decode(heap, args, span),
        BuiltinOp::Sha256 => vm_builtin_sha256(heap, args, span),
    }
}

fn vm_builtin_print(heap: &mut VmHeap, args: &[Value]) -> Result<Value, Error> {
    for value in args {
        match value {
            Value::Integer(value) => print!("{value}"),
            Value::Float(value) => print!("{value}"),
            Value::Boolean(value) => print!("{value}"),
            Value::Null => print!("null"),
            Value::String(value) => print!("{}", heap.string(*value)),
            Value::Empty => {}
            _ => {
                return Err(Error::with_kind(
                    ErrorKind::Type,
                    format!("unsupported print type: {}", value.type_name()),
                ))
            }
        }
    }
    Ok(Value::Empty)
}

fn vm_builtin_len(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    vm_builtin_len_slice(heap, args, span)
}

fn vm_builtin_byte_len(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "byte_len", 1)?;
    Ok(Value::Integer(
        expect_vm_string(heap, &args[0], "value", span)?.len() as i64,
    ))
}

fn vm_builtin_str(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "str", 1)?;
    let value = match &args[0] {
        Value::Integer(value) => value.to_string(),
        Value::Float(value) => value.to_string(),
        Value::String(value) => heap.string(*value).to_string(),
        Value::Null => "null".to_string(),
        _ => {
            return Err(Error::with_kind_span(
                ErrorKind::Type,
                "param type error is not integer or float or string".to_string(),
                span,
            ))
        }
    };
    Ok(heap.alloc_string(value))
}

fn vm_builtin_int(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "int", 1)?;
    Ok(Value::Integer(match &args[0] {
        Value::Integer(value) => *value,
        Value::Float(value) => *value as i64,
        Value::String(value) => heap
            .string(*value)
            .parse::<i64>()
            .map_err(|err| Error::with_kind_span(ErrorKind::Runtime, err.to_string(), span))?,
        _ => {
            return Err(Error::with_kind_span(
                ErrorKind::Type,
                "param type error is not integer or float or string".to_string(),
                span,
            ))
        }
    }))
}

fn vm_builtin_float(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "float", 1)?;
    Ok(Value::Float(match &args[0] {
        Value::Integer(value) => *value as f64,
        Value::Float(value) => *value,
        Value::String(value) => heap
            .string(*value)
            .parse::<f64>()
            .map_err(|err| Error::with_kind_span(ErrorKind::Runtime, err.to_string(), span))?,
        _ => {
            return Err(Error::with_kind_span(
                ErrorKind::Type,
                "param type error is not integer or float or string".to_string(),
                span,
            ))
        }
    }))
}

fn vm_builtin_parse_int(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "parse_int", 1)?;
    let value = expect_vm_string(heap, &args[0], "value", span)?;
    Ok(match value.parse::<i64>() {
        Ok(value) => vm_result_ok(heap, Value::Integer(value)),
        Err(err) => vm_result_err(heap, "parse", "invalid_int", err.to_string()),
    })
}

fn vm_builtin_parse_float(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "parse_float", 1)?;
    let value = expect_vm_string(heap, &args[0], "value", span)?;
    Ok(match value.parse::<f64>() {
        Ok(value) if value.is_finite() => vm_result_ok(heap, Value::Float(value)),
        Ok(_) => vm_result_err(
            heap,
            "parse",
            "invalid_float",
            "non-finite float".to_string(),
        ),
        Err(err) => vm_result_err(heap, "parse", "invalid_float", err.to_string()),
    })
}

fn vm_builtin_len_slice(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "len", 1)?;
    let len = match &args[0] {
        Value::Slice(values) => heap.list(*values).len(),
        Value::Map(values) => heap.map(*values).len(),
        Value::String(value) => heap.string(*value).chars().count(),
        _ => {
            return Err(Error::with_kind_span(
                ErrorKind::Type,
                "param type error".to_string(),
                span,
            ))
        }
    };
    Ok(Value::Integer(len as i64))
}

fn vm_builtin_append(heap: &mut VmHeap, args: &[Value]) -> Result<Value, Error> {
    vm_builtin_append_slice(heap, args)
}

fn vm_builtin_append_slice(heap: &mut VmHeap, args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "append", 2)?;
    match &args[0] {
        Value::Slice(values) => {
            heap.list_mut(*values).push(args[1].clone());
            Ok(Value::Null)
        }
        _ => Err(Error::new("param type error, not be list".to_string())),
    }
}

fn vm_builtin_delete(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    vm_builtin_delete_slice(heap, args, span)
}

fn vm_builtin_delete_slice(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "delete", 2)?;
    match (&args[0], &args[1]) {
        (Value::Map(map), key) => {
            let key = vm_map_key_from_value(heap, key, span)?;
            Ok(user_value(
                heap.map_mut(*map).remove(&key).unwrap_or(Value::Empty),
            ))
        }
        (Value::Slice(values), Value::Integer(index)) => {
            let mut values = heap.list_mut(*values);
            if *index < 0 || values.len() <= *index as usize {
                return Err(Error::with_kind_span(
                    ErrorKind::Index,
                    "index out of bound".to_string(),
                    span,
                ));
            }
            Ok(user_value(values.remove(*index as usize)))
        }
        _ => Err(Error::with_kind_span(
            ErrorKind::Type,
            "delete expects map/list and key/index".to_string(),
            span,
        )),
    }
}

fn vm_builtin_join(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    vm_builtin_join_slice(heap, args, span)
}

fn vm_builtin_join_slice(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "join", 2)?;
    let separator = expect_vm_string(heap, &args[1], "separator", span)?;
    match &args[0] {
        Value::Slice(values) => {
            let values = heap
                .list(*values)
                .iter()
                .map(|value| stringify_vm_join_element(heap, value, span))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(heap.alloc_string(values.join(separator.as_str())))
        }
        _ => Err(Error::with_kind_span(
            ErrorKind::Type,
            "join expects list".to_string(),
            span,
        )),
    }
}

fn vm_builtin_json_stringify(heap: &mut VmHeap, args: &[Value]) -> Result<Value, Error> {
    vm_builtin_json_stringify_slice(heap, args)
}

fn vm_builtin_read_file(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "read_file", 1)?;
    let path = expect_vm_string(heap, &args[0], "path", span)?;
    Ok(match fs::read_to_string(path) {
        Ok(content) => {
            let content = heap.alloc_string(content);
            vm_result_ok(heap, content)
        }
        Err(err) => vm_io_error_result(heap, err),
    })
}

fn vm_builtin_write_file(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "write_file", 2)?;
    let path = expect_vm_string(heap, &args[0], "path", span)?;
    let content = expect_vm_string(heap, &args[1], "content", span)?;
    Ok(match fs::write(path, &content) {
        Ok(_) => vm_result_ok(heap, Value::Integer(content.len() as i64)),
        Err(err) => vm_io_error_result(heap, err),
    })
}

fn vm_builtin_append_file(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "append_file", 2)?;
    let path = expect_vm_string(heap, &args[0], "path", span)?;
    let content = expect_vm_string(heap, &args[1], "content", span)?;
    Ok(
        match OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .and_then(|mut file| file.write_all(content.as_bytes()))
        {
            Ok(_) => vm_result_ok(heap, Value::Integer(content.len() as i64)),
            Err(err) => vm_io_error_result(heap, err),
        },
    )
}

fn vm_builtin_file_exists(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "file_exists", 1)?;
    Ok(Value::Boolean(
        Path::new(&expect_vm_string(heap, &args[0], "path", span)?).exists(),
    ))
}

fn vm_builtin_read_dir(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "read_dir", 1)?;
    let path = expect_vm_string(heap, &args[0], "path", span)?;
    let entries_iter = match fs::read_dir(path) {
        Ok(entries) => entries,
        Err(err) => return Ok(vm_io_error_result(heap, err)),
    };
    let mut entries = Vec::new();
    for entry in entries_iter {
        match entry {
            Ok(entry) => entries.push(heap.alloc_string(entry.file_name().to_string_lossy())),
            Err(err) => return Ok(vm_io_error_result(heap, err)),
        }
    }
    entries.sort_by_key(|entry| match entry {
        Value::String(value) => heap.string(*value).to_string(),
        _ => String::new(),
    });
    let entries = heap.alloc_list(entries);
    Ok(vm_result_ok(heap, entries))
}

fn vm_builtin_mkdir(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "mkdir", 1)?;
    let path = expect_vm_string(heap, &args[0], "path", span)?;
    Ok(match fs::create_dir_all(path) {
        Ok(_) => vm_result_ok(heap, Value::Null),
        Err(err) => vm_io_error_result(heap, err),
    })
}

fn vm_builtin_remove_file(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "remove_file", 1)?;
    let path = expect_vm_string(heap, &args[0], "path", span)?;
    Ok(match fs::remove_file(path) {
        Ok(_) => vm_result_ok(heap, Value::Null),
        Err(err) => vm_io_error_result(heap, err),
    })
}

fn vm_builtin_remove_dir(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "remove_dir", 1)?;
    let path = expect_vm_string(heap, &args[0], "path", span)?;
    Ok(match fs::remove_dir(path) {
        Ok(_) => vm_result_ok(heap, Value::Null),
        Err(err) => vm_io_error_result(heap, err),
    })
}

fn vm_builtin_copy_file(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "copy_file", 2)?;
    let from = expect_vm_string(heap, &args[0], "from", span)?;
    let to = expect_vm_string(heap, &args[1], "to", span)?;
    Ok(match fs::copy(from, to) {
        Ok(bytes) => vm_result_ok(heap, Value::Integer(bytes as i64)),
        Err(err) => vm_io_error_result(heap, err),
    })
}

fn vm_builtin_rename(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "rename", 2)?;
    let from = expect_vm_string(heap, &args[0], "from", span)?;
    let to = expect_vm_string(heap, &args[1], "to", span)?;
    Ok(match fs::rename(from, to) {
        Ok(_) => vm_result_ok(heap, Value::Null),
        Err(err) => vm_io_error_result(heap, err),
    })
}

fn vm_builtin_metadata(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "metadata", 1)?;
    let path = expect_vm_string(heap, &args[0], "path", span)?;
    let metadata = match fs::metadata(path) {
        Ok(metadata) => metadata,
        Err(err) if err.kind() == io::ErrorKind::NotFound => {
            let value = vm_object_map(
                heap,
                [
                    ("exists", Value::Boolean(false)),
                    ("is_file", Value::Boolean(false)),
                    ("is_dir", Value::Boolean(false)),
                    ("size", Value::Integer(0)),
                    ("modified_ms", Value::Integer(0)),
                ],
            );
            return Ok(vm_result_ok(heap, value));
        }
        Err(err) => return Ok(vm_io_error_result(heap, err)),
    };
    let modified_ms = metadata
        .modified()
        .ok()
        .and_then(|time| time.duration_since(UNIX_EPOCH).ok())
        .map(|duration| duration.as_millis() as i64)
        .unwrap_or(0);
    let value = vm_object_map(
        heap,
        [
            ("exists", Value::Boolean(true)),
            ("is_file", Value::Boolean(metadata.is_file())),
            ("is_dir", Value::Boolean(metadata.is_dir())),
            ("size", Value::Integer(metadata.len() as i64)),
            ("modified_ms", Value::Integer(modified_ms)),
        ],
    );
    Ok(vm_result_ok(heap, value))
}

fn vm_builtin_path_join(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    if args.is_empty() {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "path_join expects at least one argument".to_string(),
            span,
        ));
    }
    let mut path = PathBuf::new();
    for (index, arg) in args.iter().enumerate() {
        path.push(expect_vm_string(
            heap,
            arg,
            format!("path[{index}]").as_str(),
            span,
        )?);
    }
    Ok(heap.alloc_string(path.to_string_lossy()))
}

fn vm_builtin_path_dirname(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "path_dirname", 1)?;
    let path = expect_vm_string(heap, &args[0], "path", span)?;
    Ok(heap.alloc_string(
        Path::new(&path)
            .parent()
            .map(|path| path.to_string_lossy().into_owned())
            .unwrap_or_default(),
    ))
}

fn vm_builtin_path_basename(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "path_basename", 1)?;
    let path = expect_vm_string(heap, &args[0], "path", span)?;
    Ok(heap.alloc_string(
        Path::new(&path)
            .file_name()
            .map(|name| name.to_string_lossy().into_owned())
            .unwrap_or_default(),
    ))
}

fn vm_builtin_path_ext(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "path_ext", 1)?;
    let path = expect_vm_string(heap, &args[0], "path", span)?;
    Ok(heap.alloc_string(
        Path::new(&path)
            .extension()
            .map(|ext| ext.to_string_lossy().into_owned())
            .unwrap_or_default(),
    ))
}

fn vm_builtin_path_exists(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "path_exists", 1)?;
    Ok(Value::Boolean(
        Path::new(&expect_vm_string(heap, &args[0], "path", span)?).exists(),
    ))
}

fn vm_builtin_path_is_file(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "path_is_file", 1)?;
    Ok(Value::Boolean(
        Path::new(&expect_vm_string(heap, &args[0], "path", span)?).is_file(),
    ))
}

fn vm_builtin_path_is_dir(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "path_is_dir", 1)?;
    Ok(Value::Boolean(
        Path::new(&expect_vm_string(heap, &args[0], "path", span)?).is_dir(),
    ))
}

fn vm_builtin_env_get(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "env_get", 1)?;
    let name = expect_vm_string(heap, &args[0], "name", span)?;
    Ok(env::var(name)
        .map(|value| heap.alloc_string(value))
        .unwrap_or(Value::Null))
}

fn vm_builtin_env_set(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "env_set", 2)?;
    let name = expect_vm_string(heap, &args[0], "name", span)?;
    let value = expect_vm_string(heap, &args[1], "value", span)?;
    env::set_var(name, value);
    Ok(Value::Empty)
}

fn vm_builtin_cwd(heap: &mut VmHeap, args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "cwd", 0)?;
    Ok(match env::current_dir() {
        Ok(path) => {
            let path = heap.alloc_string(path.to_string_lossy());
            vm_result_ok(heap, path)
        }
        Err(err) => vm_io_error_result(heap, err),
    })
}

fn vm_builtin_set_cwd(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "set_cwd", 1)?;
    let path = expect_vm_string(heap, &args[0], "path", span)?;
    Ok(match env::set_current_dir(path) {
        Ok(_) => vm_result_ok(heap, Value::Null),
        Err(err) => vm_io_error_result(heap, err),
    })
}

fn vm_builtin_exit(args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "exit", 1)?;
    std::process::exit(expect_vm_int(&args[0], "code", span)? as i32);
}

fn vm_builtin_time_ms(args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "time_ms", 0)?;
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|err| Error::new(err.to_string()))?
        .as_millis();
    Ok(Value::Integer(millis as i64))
}

fn vm_builtin_sleep_ms(args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "sleep_ms", 1)?;
    let millis = expect_vm_int(&args[0], "millis", span)?;
    if millis < 0 {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "millis must be non-negative".to_string(),
            span,
        ));
    }
    std::thread::sleep(Duration::from_millis(millis as u64));
    Ok(Value::Empty)
}

fn vm_builtin_clear(args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "clear", 0)?;
    let mut stdout = io::stdout();
    execute!(
        stdout,
        ct_terminal::Clear(ClearType::All),
        cursor::MoveTo(0, 0)
    )
    .map_err(terminal_io_error)?;
    Ok(Value::Empty)
}

fn vm_builtin_home(args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "home", 0)?;
    let mut stdout = io::stdout();
    execute!(stdout, cursor::MoveTo(0, 0)).map_err(terminal_io_error)?;
    Ok(Value::Empty)
}

fn vm_builtin_hide_cursor(args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "hide_cursor", 0)?;
    let mut stdout = io::stdout();
    execute!(stdout, cursor::Hide).map_err(terminal_io_error)?;
    Ok(Value::Empty)
}

fn vm_builtin_show_cursor(args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "show_cursor", 0)?;
    let mut stdout = io::stdout();
    execute!(stdout, cursor::Show).map_err(terminal_io_error)?;
    Ok(Value::Empty)
}

fn vm_builtin_enable_raw_mode(args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "enable_raw_mode", 0)?;
    ct_terminal::enable_raw_mode().map_err(terminal_io_error)?;
    Ok(Value::Empty)
}

fn vm_builtin_disable_raw_mode(args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "disable_raw_mode", 0)?;
    ct_terminal::disable_raw_mode().map_err(terminal_io_error)?;
    Ok(Value::Empty)
}

fn vm_builtin_read_key(heap: &mut VmHeap, args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "read_key", 0)?;
    loop {
        match event::read() {
            Ok(Event::Key(key)) if is_key_press(key) => {
                let key = key_event_value(heap, key);
                return Ok(vm_result_ok(heap, key));
            }
            Ok(_) => continue,
            Err(err) => return Ok(vm_result_err(heap, "terminal", "read_key", err.to_string())),
        }
    }
}

fn vm_builtin_read_key_timeout(
    heap: &mut VmHeap,
    args: &[Value],
    span: Span,
    latest_only: bool,
) -> Result<Value, Error> {
    expect_vm_arity(args, "read_key_timeout", 1)?;
    let millis = expect_vm_int(&args[0], "millis", span)?;
    if millis < 0 {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "millis must be non-negative".to_string(),
            span,
        ));
    }
    let timeout = Duration::from_millis(millis as u64);
    let start = Instant::now();
    loop {
        let Some(remaining) = timeout.checked_sub(start.elapsed()) else {
            return Ok(vm_result_ok(heap, Value::Null));
        };
        match event::poll(remaining) {
            Ok(false) => return Ok(vm_result_ok(heap, Value::Null)),
            Ok(true) => match event::read() {
                Ok(Event::Key(key)) if is_key_press(key) => {
                    if !latest_only {
                        let key = key_event_value(heap, key);
                        return Ok(vm_result_ok(heap, key));
                    }
                    let mut latest = key;
                    loop {
                        match event::poll(Duration::from_millis(0)) {
                            Ok(false) => {
                                let latest = key_event_value(heap, latest);
                                return Ok(vm_result_ok(heap, latest));
                            }
                            Ok(true) => match event::read() {
                                Ok(Event::Key(key)) if is_key_press(key) => latest = key,
                                Ok(_) => continue,
                                Err(err) => {
                                    return Ok(vm_result_err(
                                        heap,
                                        "terminal",
                                        "read_key_latest_timeout",
                                        err.to_string(),
                                    ))
                                }
                            },
                            Err(err) => {
                                return Ok(vm_result_err(heap, "terminal", "poll", err.to_string()))
                            }
                        }
                    }
                }
                Ok(_) => continue,
                Err(err) => {
                    return Ok(vm_result_err(
                        heap,
                        "terminal",
                        "read_key_timeout",
                        err.to_string(),
                    ))
                }
            },
            Err(err) => return Ok(vm_result_err(heap, "terminal", "poll", err.to_string())),
        }
    }
}

fn vm_builtin_move_cursor(args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "move", 2)?;
    let row = expect_terminal_coordinate(&args[0], "row", span)?;
    let col = expect_terminal_coordinate(&args[1], "col", span)?;
    let mut stdout = io::stdout();
    execute!(stdout, cursor::MoveTo(col, row)).map_err(terminal_io_error)?;
    Ok(Value::Empty)
}

fn vm_builtin_clear_line(args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "clear_line", 0)?;
    let mut stdout = io::stdout();
    execute!(stdout, ct_terminal::Clear(ClearType::CurrentLine)).map_err(terminal_io_error)?;
    Ok(Value::Empty)
}

fn vm_builtin_terminal_size(heap: &mut VmHeap, args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "size", 0)?;
    Ok(match ct_terminal::size() {
        Ok((cols, rows)) => {
            let value = vm_object_map(
                heap,
                [
                    ("cols", Value::Integer(i64::from(cols))),
                    ("rows", Value::Integer(i64::from(rows))),
                ],
            );
            vm_result_ok(heap, value)
        }
        Err(err) => vm_result_err(heap, "terminal", "size", err.to_string()),
    })
}

fn vm_builtin_enter_alt_screen(args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "enter_alt_screen", 0)?;
    let mut stdout = io::stdout();
    execute!(stdout, ct_terminal::EnterAlternateScreen).map_err(terminal_io_error)?;
    Ok(Value::Empty)
}

fn vm_builtin_leave_alt_screen(args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "leave_alt_screen", 0)?;
    let mut stdout = io::stdout();
    execute!(stdout, ct_terminal::LeaveAlternateScreen).map_err(terminal_io_error)?;
    Ok(Value::Empty)
}

fn vm_builtin_terminal_fg(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "fg", 1)?;
    let color = expect_terminal_color(heap, &args[0], "color", span)?;
    let mut stdout = io::stdout();
    execute!(stdout, SetForegroundColor(color)).map_err(terminal_io_error)?;
    Ok(Value::Empty)
}

fn vm_builtin_terminal_bg(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "bg", 1)?;
    let color = expect_terminal_color(heap, &args[0], "color", span)?;
    let mut stdout = io::stdout();
    execute!(stdout, SetBackgroundColor(color)).map_err(terminal_io_error)?;
    Ok(Value::Empty)
}

fn vm_builtin_terminal_bold(args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "bold", 0)?;
    let mut stdout = io::stdout();
    execute!(stdout, SetAttribute(Attribute::Bold)).map_err(terminal_io_error)?;
    Ok(Value::Empty)
}

fn vm_builtin_terminal_reset_style(args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "reset_style", 0)?;
    let mut stdout = io::stdout();
    execute!(stdout, ResetColor, SetAttribute(Attribute::Reset)).map_err(terminal_io_error)?;
    Ok(Value::Empty)
}

fn vm_builtin_terminal_paint(
    heap: &mut VmHeap,
    args: &[Value],
    span: Span,
) -> Result<Value, Error> {
    expect_vm_arity(args, "paint", 2)?;
    let code = expect_terminal_ansi_color_code(heap, &args[0], "color", span)?;
    let text = expect_vm_string(heap, &args[1], "text", span)?;
    Ok(heap.alloc_string(format!("\x1b[{code}m{text}\x1b[0m")))
}

fn vm_builtin_terminal_paint_runs(
    heap: &mut VmHeap,
    args: &[Value],
    span: Span,
) -> Result<Value, Error> {
    expect_vm_arity(args, "paint_runs", 1)?;
    let runs = expect_vm_list(heap, &args[0], "runs", span)?;
    let mut out = String::new();
    for (index, run) in runs.iter().enumerate() {
        let color = vm_field(heap, run, "color")
            .ok_or_else(|| Error::new(format!("runs[{index}].color is required")))?;
        let text = vm_field(heap, run, "text")
            .ok_or_else(|| Error::new(format!("runs[{index}].text is required")))?;
        let code = expect_terminal_ansi_color_code(
            heap,
            &color,
            format!("runs[{index}].color").as_str(),
            span,
        )?;
        let text = expect_vm_string(heap, &text, format!("runs[{index}].text").as_str(), span)?;
        out.push_str(format!("\x1b[{code}m{text}\x1b[0m").as_str());
    }
    Ok(heap.alloc_string(out))
}

fn vm_builtin_args(heap: &mut VmHeap, args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "args", 0)?;
    let values = env::args().map(|arg| heap.alloc_string(arg)).collect();
    Ok(heap.alloc_list(values))
}

fn vm_builtin_read_line(heap: &mut VmHeap, args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "read_line", 0)?;
    let mut line = String::new();
    if let Err(err) = io::stdin().read_line(&mut line) {
        return Ok(vm_io_error_result(heap, err));
    }
    if line.ends_with('\n') {
        line.pop();
        if line.ends_with('\r') {
            line.pop();
        }
    }
    let line = heap.alloc_string(line);
    Ok(vm_result_ok(heap, line))
}

fn vm_builtin_panic(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "panic", 1)?;
    Err(Error::with_kind_span(
        ErrorKind::Panic,
        expect_vm_string(heap, &args[0], "message", span)?.to_string(),
        span,
    ))
}

fn vm_builtin_assert(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "assert", 2)?;
    match &args[0] {
        Value::Boolean(true) => Ok(Value::Empty),
        Value::Boolean(false) => Err(Error::with_kind_span(
            ErrorKind::Assert,
            expect_vm_string(heap, &args[1], "message", span)?.to_string(),
            span,
        )),
        _ => Err(Error::with_kind_span(
            ErrorKind::Type,
            "condition must be bool".to_string(),
            span,
        )),
    }
}

fn vm_builtin_substr(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "substr", 3)?;
    let text = expect_vm_string(heap, &args[0], "text", span)?;
    let start = expect_vm_int(&args[1], "start", span)?;
    let len = expect_vm_int(&args[2], "len", span)?;
    if start < 0 || len < 0 {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "start and len must be non-negative".to_string(),
            span,
        ));
    }
    let value: String = text
        .chars()
        .skip(start as usize)
        .take(len as usize)
        .collect();
    Ok(heap.alloc_string(value))
}

fn vm_builtin_find(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "find", 2)?;
    let text = expect_vm_string(heap, &args[0], "text", span)?;
    let needle = expect_vm_string(heap, &args[1], "needle", span)?;
    Ok(Value::Integer(
        text.find(needle.as_str())
            .map(|index| text[..index].chars().count() as i64)
            .unwrap_or(-1),
    ))
}

fn vm_builtin_replace(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "replace", 3)?;
    let text = expect_vm_string(heap, &args[0], "text", span)?;
    let from = expect_vm_string(heap, &args[1], "from", span)?;
    let to = expect_vm_string(heap, &args[2], "to", span)?;
    Ok(heap.alloc_string(text.replace(from.as_str(), to.as_str())))
}

fn vm_builtin_char_code(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "char_code", 1)?;
    let text = expect_vm_string(heap, &args[0], "text", span)?;
    let mut chars = text.chars();
    let ch = chars.next().ok_or_else(|| {
        Error::with_kind_span(
            ErrorKind::Runtime,
            "text must contain one character".to_string(),
            span,
        )
    })?;
    if chars.next().is_some() {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "text must contain one character".to_string(),
            span,
        ));
    }
    Ok(Value::Integer(ch as i64))
}

fn vm_builtin_from_char_code(
    heap: &mut VmHeap,
    args: &[Value],
    span: Span,
) -> Result<Value, Error> {
    expect_vm_arity(args, "from_char_code", 1)?;
    let code = expect_vm_int(&args[0], "code", span)?;
    let ch = char::from_u32(code as u32).ok_or_else(|| {
        Error::with_kind_span(
            ErrorKind::Runtime,
            "invalid character code".to_string(),
            span,
        )
    })?;
    Ok(heap.alloc_string(ch.to_string()))
}

fn vm_builtin_json_parse(heap: &mut VmHeap, args: &[Value]) -> Result<Value, Error> {
    vm_builtin_json_parse_slice(heap, args)
}

fn vm_builtin_json_parse_slice(heap: &mut VmHeap, args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "json_parse", 1)?;
    let source = match expect_vm_string(heap, &args[0], "source", zero_span()) {
        Ok(source) => source,
        Err(_) => {
            return Ok(vm_result_err(
                heap,
                "parse",
                "invalid_json",
                "json_parse expects string".to_string(),
            ))
        }
    };
    match serde_json::from_str::<JsonValue>(&source) {
        Ok(value) => {
            let value = json_value_to_vm(heap, value);
            Ok(vm_result_ok(heap, value))
        }
        Err(err) => Ok(vm_result_err(
            heap,
            "parse",
            "invalid_json",
            err.to_string(),
        )),
    }
}

fn vm_result_ok(heap: &mut VmHeap, value: Value) -> Value {
    vm_object_map(
        heap,
        [
            ("ok", Value::Boolean(true)),
            ("value", value),
            ("error", Value::Null),
        ],
    )
}

fn vm_result_err(heap: &mut VmHeap, kind: &str, code: &str, message: String) -> Value {
    let kind = heap.alloc_string(kind);
    let code = heap.alloc_string(code);
    let message = heap.alloc_string(message);
    let error = vm_object_map(heap, [("kind", kind), ("code", code), ("message", message)]);
    vm_object_map(
        heap,
        [
            ("ok", Value::Boolean(false)),
            ("value", Value::Null),
            ("error", error),
        ],
    )
}

fn vm_object_map<const N: usize>(heap: &mut VmHeap, entries: [(&str, Value); N]) -> Value {
    heap.alloc_map(
        entries
            .into_iter()
            .map(|(key, value)| (AbleToMapKey::from(key), value))
            .collect(),
    )
}

fn json_value_to_vm(heap: &mut VmHeap, value: JsonValue) -> Value {
    match value {
        JsonValue::Null => Value::Null,
        JsonValue::Bool(value) => Value::Boolean(value),
        JsonValue::Number(value) => {
            if let Some(value) = value.as_i64() {
                Value::Integer(value)
            } else if let Some(value) = value.as_f64() {
                Value::Float(value)
            } else {
                Value::Float(0.0)
            }
        }
        JsonValue::String(value) => heap.alloc_string(value),
        JsonValue::Array(values) => {
            let values = values
                .into_iter()
                .map(|value| json_value_to_vm(heap, value))
                .collect();
            heap.alloc_list(values)
        }
        JsonValue::Object(values) => {
            let values = values
                .into_iter()
                .map(|(key, value)| {
                    (
                        AbleToMapKey::from(key.as_str()),
                        json_value_to_vm(heap, value),
                    )
                })
                .collect();
            heap.alloc_map(values)
        }
    }
}

fn vm_builtin_json_stringify_slice(heap: &mut VmHeap, args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "json_stringify", 1)?;
    let value = vm_json_stringify_value(heap, &args[0], &mut HashSet::new())?;
    Ok(heap.alloc_string(value))
}

fn vm_json_stringify_value(
    heap: &VmHeap,
    value: &Value,
    seen: &mut HashSet<usize>,
) -> Result<String, Error> {
    match value {
        Value::Integer(value) => Ok(value.to_string()),
        Value::Float(value) => {
            if !value.is_finite() {
                return Err(Error::new("cannot stringify non-finite float".to_string()));
            }
            Ok(value.to_string())
        }
        Value::Boolean(value) => Ok(value.to_string()),
        Value::Null | Value::Empty => Ok("null".to_string()),
        Value::String(value) => Ok(vm_json_quote(heap.string(*value))),
        Value::Slice(values) => {
            let id = heap.list_identity(*values);
            if !seen.insert(id) {
                return Err(vm_json_cycle_error());
            }
            let result = {
                let values = heap.list(*values);
                values
                    .iter()
                    .map(|value| vm_json_stringify_value(heap, value, seen))
                    .collect::<Result<Vec<_>, _>>()
                    .map(|values| format!("[{}]", values.join(",")))
            };
            seen.remove(&id);
            result
        }
        Value::Map(values) => {
            let id = heap.map_identity(*values);
            if !seen.insert(id) {
                return Err(vm_json_cycle_error());
            }
            let result = vm_json_stringify_map(heap, &heap.map(*values), seen);
            seen.remove(&id);
            result
        }
        Value::StructInstance(instance) => {
            let id = heap.struct_instance_identity(*instance);
            if !seen.insert(id) {
                return Err(vm_json_cycle_error());
            }
            let result = {
                let instance = heap.struct_instance(*instance);
                let mut values = Vec::new();
                for field in instance.fields.iter() {
                    let key = AbleToMapKey::String(field.to_owned());
                    let value = instance.values.get(&key).unwrap_or(&Value::Null);
                    values.push(format!(
                        "{}:{}",
                        vm_json_quote(field),
                        vm_json_stringify_value(heap, value, seen)?
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

fn vm_json_stringify_map(
    heap: &VmHeap,
    values: &HashMap<AbleToMapKey, Value>,
    seen: &mut HashSet<usize>,
) -> Result<String, Error> {
    values
        .iter()
        .map(|(key, value)| {
            Ok((
                vm_json_map_key(key),
                vm_json_stringify_value(heap, value, seen)?,
            ))
        })
        .collect::<Result<Vec<_>, Error>>()
        .map(vm_json_format_entries)
}

pub(super) fn vm_json_format_entries(mut entries: Vec<(String, String)>) -> String {
    entries.sort_by(|left, right| left.0.cmp(&right.0));
    format!(
        "{{{}}}",
        entries
            .into_iter()
            .map(|(key, value)| format!("{}:{}", vm_json_quote(key.as_str()), value))
            .collect::<Vec<_>>()
            .join(",")
    )
}

pub(super) fn vm_json_cycle_error() -> Error {
    Error::with_kind(ErrorKind::Type, "cannot stringify cyclic value".to_string())
}

pub(super) fn vm_json_map_key(key: &AbleToMapKey) -> String {
    match key {
        AbleToMapKey::Integer(value) => value.to_string(),
        AbleToMapKey::Boolean(value) => value.to_string(),
        AbleToMapKey::String(value) => value.to_owned(),
    }
}

pub(super) fn vm_json_quote(value: &str) -> String {
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

fn stringify_vm_join_element(heap: &VmHeap, value: &Value, span: Span) -> Result<String, Error> {
    match value {
        Value::Integer(value) => Ok(value.to_string()),
        Value::Float(value) => Ok(value.to_string()),
        Value::Boolean(value) => Ok(value.to_string()),
        Value::Null => Ok("null".to_string()),
        Value::String(value) => Ok(heap.string(*value).to_string()),
        Value::Empty => Ok(String::new()),
        _ => Err(Error::with_kind_span(
            ErrorKind::Type,
            "unsupported join element type".to_string(),
            span,
        )),
    }
}

fn vm_builtin_type(heap: &mut VmHeap, args: &[Value]) -> Result<Value, Error> {
    vm_builtin_type_slice(heap, args)
}

fn vm_builtin_type_slice(heap: &mut VmHeap, args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "type", 1)?;
    Ok(heap.alloc_string(args[0].type_name()))
}

fn vm_builtin_is_type(args: &[Value], predicate: fn(&Value) -> bool) -> Result<Value, Error> {
    vm_builtin_is_type_slice(args, predicate)
}

fn vm_builtin_is_type_slice(args: &[Value], predicate: fn(&Value) -> bool) -> Result<Value, Error> {
    expect_vm_arity(args, "type predicate", 1)?;
    Ok(Value::Boolean(predicate(&args[0])))
}

fn vm_builtin_math_slice(name: &str, args: &[Value], span: Span) -> Result<Value, Error> {
    let value = match name {
        "abs" => {
            expect_vm_arity(args, "abs", 1)?;
            match &args[0] {
                Value::Integer(value) => value
                    .checked_abs()
                    .map(Value::Integer)
                    .ok_or_else(|| Error::new("abs overflow".to_string()))?,
                Value::Float(value) => Value::Float(value.abs()),
                _ => {
                    return Err(Error::with_kind_span(
                        ErrorKind::Type,
                        "value must be int or float".to_string(),
                        span,
                    ))
                }
            }
        }
        "floor" => {
            expect_vm_arity(args, "floor", 1)?;
            Value::Float(expect_vm_number(&args[0], "value", span)?.0.floor())
        }
        "ceil" => {
            expect_vm_arity(args, "ceil", 1)?;
            Value::Float(expect_vm_number(&args[0], "value", span)?.0.ceil())
        }
        "round" => {
            expect_vm_arity(args, "round", 1)?;
            Value::Float(expect_vm_number(&args[0], "value", span)?.0.round())
        }
        "sqrt" => {
            expect_vm_arity(args, "sqrt", 1)?;
            let value = expect_vm_number(&args[0], "value", span)?.0;
            if value < 0.0 {
                return Err(Error::with_kind_span(
                    ErrorKind::Runtime,
                    "sqrt value must be non-negative".to_string(),
                    span,
                ));
            }
            Value::Float(value.sqrt())
        }
        "pow" => {
            expect_vm_arity(args, "pow", 2)?;
            let base = expect_vm_number(&args[0], "base", span)?.0;
            let exponent = expect_vm_number(&args[1], "exponent", span)?.0;
            Value::Float(base.powf(exponent))
        }
        "min" => vm_min_or_max(args, "min", span, |candidate, best| candidate < best)?,
        "max" => vm_min_or_max(args, "max", span, |candidate, best| candidate > best)?,
        _ => unreachable!("unknown math builtin"),
    };
    Ok(value)
}

fn expect_vm_number(value: &Value, name: &str, span: Span) -> Result<(f64, Option<i64>), Error> {
    match value {
        Value::Integer(value) => Ok((*value as f64, Some(*value))),
        Value::Float(value) => Ok((*value, None)),
        _ => Err(Error::with_kind_span(
            ErrorKind::Type,
            format!("{name} must be int or float"),
            span,
        )),
    }
}

fn vm_min_or_max(
    args: &[Value],
    function: &str,
    span: Span,
    better: fn(candidate: f64, best: f64) -> bool,
) -> Result<Value, Error> {
    if args.is_empty() {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            format!("{function} expects at least one argument"),
            span,
        ));
    }

    let (mut best, mut best_int) = expect_vm_number(&args[0], "value", span)?;
    let mut all_ints = best_int.is_some();
    for value in &args[1..] {
        let (candidate, candidate_int) = expect_vm_number(value, "value", span)?;
        all_ints &= candidate_int.is_some();
        if better(candidate, best) {
            best = candidate;
            best_int = candidate_int;
        }
    }

    match (all_ints, best_int) {
        (true, Some(value)) => Ok(Value::Integer(value)),
        _ => Ok(Value::Float(best)),
    }
}

fn vm_builtin_random_int(args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "random_int", 2)?;
    let min = expect_vm_int(&args[0], "min", span)?;
    let max = expect_vm_int(&args[1], "max", span)?;
    if min > max {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            "min must be <= max".to_string(),
            span,
        ));
    }
    Ok(Value::Integer(rand::thread_rng().gen_range(min..=max)))
}

fn vm_builtin_random_float(args: &[Value]) -> Result<Value, Error> {
    expect_vm_arity(args, "random_float", 0)?;
    Ok(Value::Float(rand::thread_rng().gen_range(0.0..1.0)))
}

fn vm_builtin_sort(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "sort", 1)?;
    let mut values = expect_vm_list(heap, &args[0], "value", span)?;
    sort_vm_values(heap, &mut values, span)?;
    Ok(heap.alloc_list(values))
}

fn sort_vm_values(heap: &VmHeap, values: &mut [Value], span: Span) -> Result<(), Error> {
    if values.is_empty() {
        return Ok(());
    }
    if values
        .iter()
        .all(|value| matches!(value, Value::Integer(_) | Value::Float(_)))
    {
        values.sort_by(|left, right| {
            number_for_sort_value(left)
                .partial_cmp(&number_for_sort_value(right))
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        return Ok(());
    }
    if values.iter().all(|value| matches!(value, Value::String(_))) {
        values.sort_by(|left, right| {
            expect_vm_string(heap, left, "value", span)
                .unwrap_or_default()
                .cmp(&expect_vm_string(heap, right, "value", span).unwrap_or_default())
        });
        return Ok(());
    }
    if values
        .iter()
        .all(|value| matches!(value, Value::Boolean(_)))
    {
        values.sort_by_key(|value| matches!(value, Value::Boolean(true)));
        return Ok(());
    }
    Err(Error::with_kind_span(
        ErrorKind::Type,
        "sort supports lists of numbers, strings, or bools".to_string(),
        span,
    ))
}

fn number_for_sort_value(value: &Value) -> f64 {
    match value {
        Value::Integer(value) => *value as f64,
        Value::Float(value) => *value,
        _ => unreachable!(),
    }
}

fn vm_builtin_http_get(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    if args.len() != 1 && args.len() != 2 {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            format!("http_get expects 1 or 2 arguments, got {}", args.len()),
            span,
        ));
    }
    let url = expect_vm_string(heap, &args[0], "url", span)?;
    let headers = optional_vm_headers(heap, args.get(1), span)?;
    http_get(heap, url.as_str(), &headers)
}

fn vm_builtin_http_post(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    if args.len() != 2 && args.len() != 3 {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            format!("http_post expects 2 or 3 arguments, got {}", args.len()),
            span,
        ));
    }
    let url = expect_vm_string(heap, &args[0], "url", span)?;
    let body = expect_vm_string(heap, &args[1], "body", span)?;
    let headers = optional_vm_headers(heap, args.get(2), span)?;
    http_post(heap, url.as_str(), body.as_str(), &headers)
}

fn vm_builtin_http_request(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    if args.len() != 3 && args.len() != 4 {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            format!("http_request expects 3 or 4 arguments, got {}", args.len()),
            span,
        ));
    }
    let method = expect_vm_string(heap, &args[0], "method", span)?;
    let url = expect_vm_string(heap, &args[1], "url", span)?;
    let body = expect_vm_string(heap, &args[2], "body", span)?;
    let headers = optional_vm_headers(heap, args.get(3), span)?;
    http_request(heap, method.as_str(), url.as_str(), body.as_str(), &headers)
}

fn http_get(heap: &mut VmHeap, url: &str, headers: &[(String, String)]) -> Result<Value, Error> {
    let client = match http_client() {
        Ok(client) => client,
        Err(err) => return Ok(vm_result_err(heap, "http", "client", err.to_string())),
    };
    match apply_headers(client.get(url), headers).send() {
        Ok(response) => response_to_vm(heap, response),
        Err(err) => Ok(vm_result_err(heap, "http", "request", err.to_string())),
    }
}

fn http_post(
    heap: &mut VmHeap,
    url: &str,
    body: &str,
    headers: &[(String, String)],
) -> Result<Value, Error> {
    let client = match http_client() {
        Ok(client) => client,
        Err(err) => return Ok(vm_result_err(heap, "http", "client", err.to_string())),
    };
    let request = client
        .post(url)
        .header("content-type", "text/plain; charset=utf-8")
        .body(body.to_string());
    match apply_headers(request, headers).send() {
        Ok(response) => response_to_vm(heap, response),
        Err(err) => Ok(vm_result_err(heap, "http", "request", err.to_string())),
    }
}

fn http_request(
    heap: &mut VmHeap,
    method: &str,
    url: &str,
    body: &str,
    headers: &[(String, String)],
) -> Result<Value, Error> {
    let client = match http_client() {
        Ok(client) => client,
        Err(err) => return Ok(vm_result_err(heap, "http", "client", err.to_string())),
    };
    let method = match reqwest::Method::from_bytes(method.to_uppercase().as_bytes()) {
        Ok(method) => method,
        Err(err) => {
            return Ok(vm_result_err(
                heap,
                "http",
                "invalid_method",
                err.to_string(),
            ))
        }
    };
    let mut request = client.request(method, url);
    if !body.is_empty() {
        request = request.body(body.to_string());
    }
    match apply_headers(request, headers).send() {
        Ok(response) => response_to_vm(heap, response),
        Err(err) => Ok(vm_result_err(heap, "http", "request", err.to_string())),
    }
}

fn optional_vm_headers(
    heap: &VmHeap,
    value: Option<&Value>,
    span: Span,
) -> Result<Vec<(String, String)>, Error> {
    match value {
        Some(Value::Map(headers)) => heap
            .map(*headers)
            .iter()
            .map(|(key, value)| Ok((vm_json_map_key(key), vm_header_value(heap, value, span)?)))
            .collect(),
        Some(_) => Err(Error::with_kind_span(
            ErrorKind::Type,
            "headers must be map".to_string(),
            span,
        )),
        None => Ok(Vec::new()),
    }
}

fn vm_header_value(heap: &VmHeap, value: &Value, span: Span) -> Result<String, Error> {
    match value {
        Value::Integer(value) => Ok(value.to_string()),
        Value::Float(value) => Ok(value.to_string()),
        Value::Boolean(value) => Ok(value.to_string()),
        Value::Null | Value::Empty => Ok(String::new()),
        Value::String(value) => Ok(heap.string(*value).to_string()),
        _ => Err(Error::with_kind_span(
            ErrorKind::Type,
            "header value must be scalar".to_string(),
            span,
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

fn response_to_vm(
    heap: &mut VmHeap,
    response: reqwest::blocking::Response,
) -> Result<Value, Error> {
    let status = response.status().as_u16() as i64;
    let mut headers = HashMap::new();
    for (name, value) in response.headers() {
        headers.insert(
            name.as_str().to_ascii_lowercase(),
            heap.alloc_string(value.to_str().unwrap_or("")),
        );
    }

    match response.text() {
        Ok(body) => {
            let value = http_response_value(heap, status, headers, body.as_str());
            Ok(vm_result_ok(heap, value))
        }
        Err(err) => Ok(vm_result_err(heap, "http", "body", err.to_string())),
    }
}

fn http_response_value(
    heap: &mut VmHeap,
    status: i64,
    headers: HashMap<String, Value>,
    body: &str,
) -> Value {
    let header_map = headers
        .into_iter()
        .map(|(key, value)| (AbleToMapKey::from(key), value))
        .collect();
    let headers = heap.alloc_map(header_map);
    let body = heap.alloc_string(body);
    vm_object_map(
        heap,
        [
            ("status", Value::Integer(status)),
            ("status_ok", Value::Boolean((200..400).contains(&status))),
            ("headers", headers),
            ("body", body),
        ],
    )
}

fn vm_builtin_exec(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    if args.len() != 1 && args.len() != 2 {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            format!("exec expects 1 or 2 arguments, got {}", args.len()),
            span,
        ));
    }
    let command = expect_vm_string(heap, &args[0], "command", span)?;
    let command_args = optional_vm_string_list(heap, args.get(1), "args", span)?;
    match Command::new(command).args(command_args.iter()).output() {
        Ok(output) => {
            let stdout = heap.alloc_string(String::from_utf8_lossy(&output.stdout));
            let stderr = heap.alloc_string(String::from_utf8_lossy(&output.stderr));
            let value = vm_object_map(
                heap,
                [
                    ("success", Value::Boolean(output.status.success())),
                    (
                        "status",
                        Value::Integer(output.status.code().map(i64::from).unwrap_or(-1)),
                    ),
                    ("stdout", stdout),
                    ("stderr", stderr),
                ],
            );
            Ok(vm_result_ok(heap, value))
        }
        Err(err) => Ok(vm_result_err(heap, "process", "spawn", err.to_string())),
    }
}

fn vm_builtin_url_encode(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "url_encode", 1)?;
    let value = expect_vm_string(heap, &args[0], "value", span)?;
    Ok(heap.alloc_string(urlencoding::encode(value.as_str()).into_owned()))
}

fn vm_builtin_url_decode(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "url_decode", 1)?;
    let value = expect_vm_string(heap, &args[0], "value", span)?;
    Ok(match urlencoding::decode(value.as_str()) {
        Ok(value) => {
            let value = heap.alloc_string(value.into_owned());
            vm_result_ok(heap, value)
        }
        Err(err) => vm_result_err(heap, "decode", "invalid_url_encoding", err.to_string()),
    })
}

fn vm_builtin_base64_encode(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "base64_encode", 1)?;
    let value = expect_vm_string(heap, &args[0], "value", span)?;
    Ok(heap.alloc_string(general_purpose::STANDARD.encode(value)))
}

fn vm_builtin_base64_decode(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "base64_decode", 1)?;
    let value = expect_vm_string(heap, &args[0], "value", span)?;
    let bytes = match general_purpose::STANDARD.decode(value) {
        Ok(bytes) => bytes,
        Err(err) => {
            return Ok(vm_result_err(
                heap,
                "decode",
                "invalid_base64",
                err.to_string(),
            ))
        }
    };
    Ok(match String::from_utf8(bytes) {
        Ok(value) => {
            let value = heap.alloc_string(value);
            vm_result_ok(heap, value)
        }
        Err(err) => vm_result_err(heap, "decode", "invalid_utf8", err.to_string()),
    })
}

fn vm_builtin_sha256(heap: &mut VmHeap, args: &[Value], span: Span) -> Result<Value, Error> {
    expect_vm_arity(args, "sha256", 1)?;
    let value = expect_vm_string(heap, &args[0], "value", span)?;
    let mut hasher = Sha256::new();
    hasher.update(value.as_bytes());
    Ok(heap.alloc_string(format!("{:x}", hasher.finalize())))
}

fn expect_vm_string(heap: &VmHeap, value: &Value, name: &str, span: Span) -> Result<String, Error> {
    match value {
        Value::String(value) => Ok(heap.string(*value).to_string()),
        _ => Err(Error::with_kind_span(
            ErrorKind::Type,
            format!("{name} must be string"),
            span,
        )),
    }
}

fn expect_vm_int(value: &Value, name: &str, span: Span) -> Result<i64, Error> {
    match value {
        Value::Integer(value) => Ok(*value),
        _ => Err(Error::with_kind_span(
            ErrorKind::Type,
            format!("{name} must be int"),
            span,
        )),
    }
}

fn vm_map_key_from_value(heap: &VmHeap, value: &Value, span: Span) -> Result<AbleToMapKey, Error> {
    match value {
        Value::Integer(value) => Ok(AbleToMapKey::Integer(*value)),
        Value::Boolean(value) => Ok(AbleToMapKey::Boolean(*value)),
        Value::String(value) => Ok(AbleToMapKey::String(heap.string(*value).to_string())),
        _ => Err(Error::with_kind_span(
            ErrorKind::Type,
            "unable as map key".to_string(),
            span,
        )),
    }
}

fn expect_vm_list(
    heap: &VmHeap,
    value: &Value,
    name: &str,
    span: Span,
) -> Result<Vec<Value>, Error> {
    match value {
        Value::Slice(values) => Ok(heap.list(*values).iter().cloned().collect()),
        _ => Err(Error::with_kind_span(
            ErrorKind::Type,
            format!("{name} must be list"),
            span,
        )),
    }
}

fn optional_vm_string_list(
    heap: &VmHeap,
    value: Option<&Value>,
    name: &str,
    span: Span,
) -> Result<Vec<String>, Error> {
    match value {
        Some(value) => expect_vm_list(heap, value, name, span)?
            .iter()
            .enumerate()
            .map(|(index, value)| {
                expect_vm_string(heap, value, format!("{name}[{index}]").as_str(), span)
            })
            .collect(),
        None => Ok(Vec::new()),
    }
}

fn vm_field(heap: &VmHeap, value: &Value, key: &str) -> Option<Value> {
    let key = AbleToMapKey::from(key);
    match value {
        Value::Map(values) => heap.map(*values).get(&key).cloned(),
        Value::StructInstance(instance) => {
            heap.struct_instance(*instance).values.get(&key).cloned()
        }
        _ => None,
    }
}

fn vm_io_error_result(heap: &mut VmHeap, err: io::Error) -> Value {
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
    vm_result_err(heap, "io", code, err.to_string())
}

fn terminal_io_error(err: io::Error) -> Error {
    Error::with_kind(ErrorKind::Io, err.to_string())
}

fn expect_terminal_ansi_color_code(
    heap: &VmHeap,
    value: &Value,
    name: &str,
    span: Span,
) -> Result<&'static str, Error> {
    let color = expect_vm_string(heap, value, name, span)?;
    match color.as_str() {
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
        _ => Err(Error::with_kind_span(
            ErrorKind::Type,
            format!("unknown terminal color '{color}'"),
            span,
        )),
    }
}

fn expect_terminal_color(
    heap: &VmHeap,
    value: &Value,
    name: &str,
    span: Span,
) -> Result<Color, Error> {
    let color = expect_vm_string(heap, value, name, span)?;
    match color.as_str() {
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
        _ => Err(Error::with_kind_span(
            ErrorKind::Type,
            format!("unknown terminal color '{color}'"),
            span,
        )),
    }
}

fn expect_terminal_coordinate(value: &Value, name: &str, span: Span) -> Result<u16, Error> {
    let coordinate = expect_vm_int(value, name, span)?;
    if coordinate < 0 {
        return Err(Error::with_kind_span(
            ErrorKind::Runtime,
            format!("{name} must be non-negative"),
            span,
        ));
    }
    u16::try_from(coordinate).map_err(|_| {
        Error::with_kind_span(ErrorKind::Runtime, format!("{name} must fit in u16"), span)
    })
}

fn key_event_value(heap: &mut VmHeap, event: KeyEvent) -> Value {
    let (kind, key) = key_code_parts(event.code);
    let modifiers = event.modifiers;
    let kind = heap.alloc_string(kind);
    let key = heap.alloc_string(key);
    vm_object_map(
        heap,
        [
            ("kind", kind),
            ("key", key),
            (
                "ctrl",
                Value::Boolean(modifiers.contains(KeyModifiers::CONTROL)),
            ),
            ("alt", Value::Boolean(modifiers.contains(KeyModifiers::ALT))),
            (
                "shift",
                Value::Boolean(modifiers.contains(KeyModifiers::SHIFT)),
            ),
        ],
    )
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

fn expect_vm_arity(args: &[Value], function: &str, expected: usize) -> Result<(), Error> {
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
