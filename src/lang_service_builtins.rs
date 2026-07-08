use crate::lang_service::ValueShape;

#[derive(Clone)]
pub(crate) struct BuiltinSignature {
    pub(crate) params: &'static str,
    pub(crate) return_type: Option<&'static str>,
    pub(crate) return_shape: Option<ValueShape>,
}

impl BuiltinSignature {
    pub(crate) fn detail(&self, name: &str) -> String {
        let signature = format!("fn {name}({})", self.params);
        match self.return_type {
            Some(return_type) => format!("{signature} -> {return_type}"),
            None => signature,
        }
    }

    pub(crate) fn builtin_detail(&self, name: &str) -> String {
        format!("builtin {}", self.detail(name))
    }
}

pub(crate) fn builtin_signature(name: &str) -> Option<BuiltinSignature> {
    let primitive = ValueShape::Primitive;
    let result = |value| Some(ValueShape::Result(Box::new(value)));
    Some(match name {
        "print" => BuiltinSignature {
            params: "values...",
            return_type: Some("empty"),
            return_shape: None,
        },
        "len" | "byte_len" => BuiltinSignature {
            params: "value",
            return_type: Some("int"),
            return_shape: None,
        },
        "str" => BuiltinSignature {
            params: "value",
            return_type: Some("string"),
            return_shape: None,
        },
        "int" => BuiltinSignature {
            params: "value",
            return_type: Some("int"),
            return_shape: None,
        },
        "float" => BuiltinSignature {
            params: "value",
            return_type: Some("float"),
            return_shape: None,
        },
        "parse_int" => BuiltinSignature {
            params: "value",
            return_type: Some("Result<int>"),
            return_shape: result(primitive("int")),
        },
        "parse_float" => BuiltinSignature {
            params: "value",
            return_type: Some("Result<float>"),
            return_shape: result(primitive("float")),
        },
        "append" => BuiltinSignature {
            params: "list, value",
            return_type: Some("empty"),
            return_shape: None,
        },
        "delete" => BuiltinSignature {
            params: "list_or_map, key",
            return_type: Some("value"),
            return_shape: None,
        },
        "join" => BuiltinSignature {
            params: "list, separator",
            return_type: Some("string"),
            return_shape: None,
        },
        "read_file" => BuiltinSignature {
            params: "path",
            return_type: Some("Result<string>"),
            return_shape: result(primitive("string")),
        },
        "write_file" | "append_file" => BuiltinSignature {
            params: "path, content",
            return_type: Some("Result<int>"),
            return_shape: result(primitive("int")),
        },
        "file_exists" => BuiltinSignature {
            params: "path",
            return_type: Some("bool"),
            return_shape: None,
        },
        "read_dir" => BuiltinSignature {
            params: "path",
            return_type: Some("Result<list>"),
            return_shape: result(primitive("list")),
        },
        "mkdir" | "remove_file" | "remove_dir" => BuiltinSignature {
            params: "path",
            return_type: Some("Result<null>"),
            return_shape: result(primitive("null")),
        },
        "copy_file" => BuiltinSignature {
            params: "from, to",
            return_type: Some("Result<int>"),
            return_shape: result(primitive("int")),
        },
        "rename" => BuiltinSignature {
            params: "from, to",
            return_type: Some("Result<null>"),
            return_shape: result(primitive("null")),
        },
        "metadata" => BuiltinSignature {
            params: "path",
            return_type: Some("Result<{exists, is_file, is_dir, size, modified_ms}>"),
            return_shape: result(ValueShape::Metadata),
        },
        "path_join" => BuiltinSignature {
            params: "parts...",
            return_type: Some("string"),
            return_shape: None,
        },
        "path_dirname" | "path_basename" | "path_ext" => BuiltinSignature {
            params: "path",
            return_type: Some("string"),
            return_shape: None,
        },
        "path_exists" | "path_is_file" | "path_is_dir" => BuiltinSignature {
            params: "path",
            return_type: Some("bool"),
            return_shape: None,
        },
        "env_get" => BuiltinSignature {
            params: "name",
            return_type: Some("string|null"),
            return_shape: None,
        },
        "env_set" => BuiltinSignature {
            params: "name, value",
            return_type: Some("empty"),
            return_shape: None,
        },
        "cwd" => BuiltinSignature {
            params: "",
            return_type: Some("Result<string>"),
            return_shape: result(primitive("string")),
        },
        "set_cwd" => BuiltinSignature {
            params: "path",
            return_type: Some("Result<null>"),
            return_shape: result(primitive("null")),
        },
        "exit" => BuiltinSignature {
            params: "code",
            return_type: Some("never"),
            return_shape: None,
        },
        "time_ms" | "now_ms" => BuiltinSignature {
            params: "",
            return_type: Some("int"),
            return_shape: None,
        },
        "sleep_ms" => BuiltinSignature {
            params: "ms",
            return_type: Some("empty"),
            return_shape: None,
        },
        "sleep" => BuiltinSignature {
            params: "seconds",
            return_type: Some("empty"),
            return_shape: None,
        },
        "clear" | "home" | "hide_cursor" | "show_cursor" | "enable_raw_mode"
        | "disable_raw_mode" | "clear_line" | "enter_alt_screen" | "leave_alt_screen" | "bold"
        | "reset_style" => BuiltinSignature {
            params: "",
            return_type: Some("empty"),
            return_shape: None,
        },
        "fg" | "bg" => BuiltinSignature {
            params: "color",
            return_type: Some("empty"),
            return_shape: None,
        },
        "paint" => BuiltinSignature {
            params: "color, text",
            return_type: Some("string"),
            return_shape: None,
        },
        "paint_runs" => BuiltinSignature {
            params: "runs",
            return_type: Some("string"),
            return_shape: None,
        },
        "read_key" => BuiltinSignature {
            params: "",
            return_type: Some("Result<{kind, key, ctrl, alt, shift}>"),
            return_shape: result(ValueShape::TerminalKey),
        },
        "read_key_timeout" => BuiltinSignature {
            params: "ms",
            return_type: Some("Result<{kind, key, ctrl, alt, shift}|null>"),
            return_shape: result(ValueShape::TerminalKey),
        },
        "read_key_latest_timeout" => BuiltinSignature {
            params: "ms",
            return_type: Some("Result<{kind, key, ctrl, alt, shift}|null>"),
            return_shape: result(ValueShape::TerminalKey),
        },
        "move" => BuiltinSignature {
            params: "row, col",
            return_type: Some("empty"),
            return_shape: None,
        },
        "size" => BuiltinSignature {
            params: "",
            return_type: Some("Result<{cols, rows}>"),
            return_shape: result(ValueShape::TerminalSize),
        },
        "abs" | "floor" | "ceil" | "round" | "sqrt" => BuiltinSignature {
            params: "value",
            return_type: Some("number"),
            return_shape: None,
        },
        "pow" => BuiltinSignature {
            params: "base, exponent",
            return_type: Some("number"),
            return_shape: None,
        },
        "min" | "max" => BuiltinSignature {
            params: "values...",
            return_type: Some("number"),
            return_shape: None,
        },
        "random_int" => BuiltinSignature {
            params: "min, max",
            return_type: Some("int"),
            return_shape: None,
        },
        "random_float" => BuiltinSignature {
            params: "",
            return_type: Some("float"),
            return_shape: None,
        },
        "sort" => BuiltinSignature {
            params: "list",
            return_type: Some("list"),
            return_shape: None,
        },
        "http_get" => BuiltinSignature {
            params: "url, headers?",
            return_type: Some("Result<{status, status_ok, headers, body}>"),
            return_shape: result(ValueShape::HttpResponse),
        },
        "http_post" => BuiltinSignature {
            params: "url, body, headers?",
            return_type: Some("Result<{status, status_ok, headers, body}>"),
            return_shape: result(ValueShape::HttpResponse),
        },
        "http_request" => BuiltinSignature {
            params: "method, url, body, headers?",
            return_type: Some("Result<{status, status_ok, headers, body}>"),
            return_shape: result(ValueShape::HttpResponse),
        },
        "exec" => BuiltinSignature {
            params: "command, args?",
            return_type: Some("Result<{success, status, stdout, stderr}>"),
            return_shape: result(ValueShape::ExecResult),
        },
        "url_encode" => BuiltinSignature {
            params: "value",
            return_type: Some("string"),
            return_shape: None,
        },
        "url_decode" => BuiltinSignature {
            params: "value",
            return_type: Some("Result<string>"),
            return_shape: result(primitive("string")),
        },
        "base64_encode" => BuiltinSignature {
            params: "value",
            return_type: Some("string"),
            return_shape: None,
        },
        "base64_decode" => BuiltinSignature {
            params: "value",
            return_type: Some("Result<string>"),
            return_shape: result(primitive("string")),
        },
        "sha256" => BuiltinSignature {
            params: "value",
            return_type: Some("string"),
            return_shape: None,
        },
        "json_parse" => BuiltinSignature {
            params: "src",
            return_type: Some("Result<value>"),
            return_shape: result(ValueShape::Unknown),
        },
        "json_stringify" => BuiltinSignature {
            params: "value",
            return_type: Some("string"),
            return_shape: None,
        },
        "args" => BuiltinSignature {
            params: "",
            return_type: Some("list"),
            return_shape: None,
        },
        "read_line" => BuiltinSignature {
            params: "",
            return_type: Some("Result<string>"),
            return_shape: result(primitive("string")),
        },
        "panic" => BuiltinSignature {
            params: "message",
            return_type: Some("never"),
            return_shape: None,
        },
        "assert" => BuiltinSignature {
            params: "condition, message?",
            return_type: Some("empty"),
            return_shape: None,
        },
        "substr" => BuiltinSignature {
            params: "text, start, length?",
            return_type: Some("string"),
            return_shape: None,
        },
        "find" => BuiltinSignature {
            params: "text, pattern",
            return_type: Some("int"),
            return_shape: None,
        },
        "replace" => BuiltinSignature {
            params: "text, from, to",
            return_type: Some("string"),
            return_shape: None,
        },
        "char_code" => BuiltinSignature {
            params: "text",
            return_type: Some("int"),
            return_shape: None,
        },
        "from_char_code" => BuiltinSignature {
            params: "code",
            return_type: Some("string"),
            return_shape: None,
        },
        "type" => BuiltinSignature {
            params: "value",
            return_type: Some("string"),
            return_shape: None,
        },
        "is_null" | "is_bool" | "is_int" | "is_float" | "is_string" | "is_list" | "is_map" => {
            BuiltinSignature {
                params: "value",
                return_type: Some("bool"),
                return_shape: None,
            }
        }
        _ => return None,
    })
}
