fn json_ok(value, next) {
    return {ok: true, value: value, next: next, error: ""};
}
fn json_err(message, next) {
    return {ok: false, value: null, next: next, error: message};
}
fn json_is_space(ch) {
    if ch == " " {
        true
    } else if ch == "\n" {
        true
    } else if ch == "\r" {
        true
    } else if ch == "\t" {
        true
    } else {
        false
    }
}
fn json_skip_space(src, i) {
    let n = len(src);
    while i < n {
        if json_is_space(src[i]) {
            i = i + 1;
        } else {
            break;
        }
    }
    i
}
fn json_is_digit(ch) {
    if ch >= "0" {
        ch <= "9"
    } else {
        false
    }
}
fn json_is_nonzero_digit(ch) {
    if ch >= "1" {
        ch <= "9"
    } else {
        false
    }
}
fn json_hex_value(ch) {
    if ch >= "0" && ch <= "9" {
        char_code(ch) - char_code("0")
    } else if ch >= "a" && ch <= "f" {
        char_code(ch) - char_code("a") + 10
    } else if ch >= "A" && ch <= "F" {
        char_code(ch) - char_code("A") + 10
    } else {
        -1
    }
}
fn json_parse_hex4(src, i) {
    let value = 0;
    let j = 0;
    while j < 4 {
        let at = i + j;
        if at >= len(src) {
            return json_err("unfinished unicode escape sequence", at);
        }
        let digit = json_hex_value(src[at]);
        if digit < 0 {
            return json_err("invalid unicode escape sequence", at);
        }
        value = value * 16 + digit;
        j = j + 1;
    }
    json_ok(value, i + 4)
}
fn json_parse_unicode_escape(src, i) {
    let parsed = json_parse_hex4(src, i);
    if !parsed["ok"] {
        return parsed;
    }
    let value = parsed["value"];
    let next = parsed["next"];
    if value >= 55296 && value <= 56319 {
        if next + 5 >= len(src) {
            return json_err("unfinished unicode surrogate pair", next);
        }
        if src[next] != "\\" {
            return json_err("expected low unicode surrogate", next);
        }
        if src[next + 1] != "u" {
            return json_err("expected low unicode surrogate", next + 1);
        }
        let low_result = json_parse_hex4(src, next + 2);
        if !low_result["ok"] {
            return low_result;
        }
        let low = low_result["value"];
        if low < 56320 || low > 57343 {
            return json_err("invalid low unicode surrogate", next + 2);
        }
        let code = 65536 + (value - 55296) * 1024 + (low - 56320);
        return json_ok(from_char_code(code), low_result["next"]);
    }
    if value >= 56320 && value <= 57343 {
        return json_err("unexpected low unicode surrogate", i);
    }
    json_ok(from_char_code(value), next)
}
fn json_parse_literal(src, i, literal, value) {
    let n = len(literal);
    let j = 0;
    while j < n {
        let at = i + j;
        if at >= len(src) {
            return json_err("unexpected end while reading literal", at);
        }
        if src[at] != literal[j] {
            return json_err("invalid literal", at);
        }
        j = j + 1;
    }
    json_ok(value, i + n)
}
fn json_parse_string(src, i) {
    if src[i] != "\"" {
        return json_err("expected string", i);
    }
    i = i + 1;
    let out = "";
    while i < len(src) {
        let ch = src[i];
        if ch == "\"" {
            return json_ok(out, i + 1);
        }
        if ch == "\\" {
            i = i + 1;
            if i >= len(src) {
                return json_err("unfinished escape sequence", i);
            }
            let esc = src[i];
            if esc == "\"" {
                out = out + "\"";
            } else if esc == "\\" {
                out = out + "\\";
            } else if esc == "/" {
                out = out + "/";
            } else if esc == "n" {
                out = out + "\n";
            } else if esc == "r" {
                out = out + "\r";
            } else if esc == "t" {
                out = out + "\t";
            } else if esc == "b" {
                out = out + from_char_code(8);
            } else if esc == "f" {
                out = out + from_char_code(12);
            } else if esc == "u" {
                let parsed = json_parse_unicode_escape(src, i + 1);
                if !parsed["ok"] {
                    return parsed;
                }
                out = out + parsed["value"];
                i = parsed["next"] - 1;
            } else {
                return json_err("unsupported escape sequence", i);
            }
        } else {
            out = out + ch;
        }
        i = i + 1;
    }
    json_err("unterminated string", i)
}
fn json_parse_number(src, i) {
    let start = i;
    let raw_start = i;
    let has_dot = false;
    let has_exp = false;
    if src[i] == "-" {
        i = i + 1;
    }
    if i >= len(src) {
        return json_err("expected digit", i);
    }
    if src[i] == "0" {
        i = i + 1;
        if i < len(src) && json_is_digit(src[i]) {
            return json_err("leading zero is not allowed", i);
        }
    } else if json_is_nonzero_digit(src[i]) {
        while i < len(src) && json_is_digit(src[i]) {
            i = i + 1;
        }
    } else {
        return json_err("expected digit", i);
    }
    if i < len(src) && src[i] == "." {
        has_dot = true;
        i = i + 1;
        if i >= len(src) {
            return json_err("expected digit after decimal point", i);
        }
        if !json_is_digit(src[i]) {
            return json_err("expected digit after decimal point", i);
        }
        while i < len(src) && json_is_digit(src[i]) {
            i = i + 1;
        }
    }
    if i < len(src) {
        if src[i] == "e" || src[i] == "E" {
            has_exp = true;
            i = i + 1;
            if i < len(src) {
                if src[i] == "+" || src[i] == "-" {
                    i = i + 1;
                }
            }
            if i >= len(src) {
                return json_err("expected digit in exponent", i);
            }
            if !json_is_digit(src[i]) {
                return json_err("expected digit in exponent", i);
            }
            while i < len(src) && json_is_digit(src[i]) {
                i = i + 1;
            }
        }
    }
    let raw = "";
    while raw_start < i {
        raw = raw + src[raw_start];
        raw_start = raw_start + 1;
    }
    let parsed = null;
    if has_dot || has_exp {
        parsed = parse_float(raw);
    } else {
        parsed = parse_int(raw);
    }
    if !parsed.ok {
        return json_err(parsed.error.message, start);
    }
    json_ok(parsed.value, i)
}
fn json_parse_array(src, i) {
    if src[i] != "[" {
        return json_err("expected array", i);
    }
    i = json_skip_space(src, i + 1);
    let items = [];
    if i < len(src) {
        if src[i] == "]" {
            return json_ok(items, i + 1);
        }
    }
    while i < len(src) {
        let parsed = json_parse_value(src, i);
        if !parsed["ok"] {
            return parsed;
        }
        append(items, parsed["value"]);
        i = json_skip_space(src, parsed["next"]);
        if i >= len(src) {
            return json_err("unterminated array", i);
        }
        if src[i] == "]" {
            return json_ok(items, i + 1);
        }
        if src[i] != "," {
            return json_err("expected comma in array", i);
        }
        i = json_skip_space(src, i + 1);
    }
    json_err("unterminated array", i)
}
fn json_parse_object(src, i) {
    if src[i] != "{" {
        return json_err("expected object", i);
    }
    i = json_skip_space(src, i + 1);
    let obj = {};
    if i < len(src) {
        if src[i] == "}" {
            return json_ok(obj, i + 1);
        }
    }
    while i < len(src) {
        let key_result = json_parse_string(src, i);
        if !key_result["ok"] {
            return key_result;
        }
        let key = key_result["value"];
        i = json_skip_space(src, key_result["next"]);
        if i >= len(src) {
            return json_err("expected colon", i);
        }
        if src[i] != ":" {
            return json_err("expected colon", i);
        }
        i = json_skip_space(src, i + 1);
        let value_result = json_parse_value(src, i);
        if !value_result["ok"] {
            return value_result;
        }
        obj[key] = value_result["value"];
        i = json_skip_space(src, value_result["next"]);
        if i >= len(src) {
            return json_err("unterminated object", i);
        }
        if src[i] == "}" {
            return json_ok(obj, i + 1);
        }
        if src[i] != "," {
            return json_err("expected comma in object", i);
        }
        i = json_skip_space(src, i + 1);
    }
    json_err("unterminated object", i)
}
fn json_parse_value(src, i) {
    i = json_skip_space(src, i);
    if i >= len(src) {
        return json_err("unexpected end of input", i);
    }
    let ch = src[i];
    if ch == "\"" {
        json_parse_string(src, i)
    } else if ch == "{" {
        json_parse_object(src, i)
    } else if ch == "[" {
        json_parse_array(src, i)
    } else if ch == "t" {
        json_parse_literal(src, i, "true", true)
    } else if ch == "f" {
        json_parse_literal(src, i, "false", false)
    } else if ch == "n" {
        json_parse_literal(src, i, "null", null)
    } else if ch == "-" {
        json_parse_number(src, i)
    } else if json_is_digit(ch) {
        json_parse_number(src, i)
    } else {
        json_err("unexpected character", i)
    }
}
fn json_parse_result(src) {
    let parsed = json_parse_value(src, 0);
    if !parsed["ok"] {
        return parsed;
    }
    let next = json_skip_space(src, parsed["next"]);
    if next != len(src) {
        return json_err("trailing characters after JSON value", next);
    }
    parsed
}
fn json_parse(src) {
    let parsed = json_parse_result(src);
    if parsed["ok"] {
        return {ok: true, value: parsed["value"], error: null};
    } else {
        return {
            ok: false,
            value: null,
            error: {kind: "parse", code: "invalid_json", message: parsed["error"], at: parsed["next"]}
        };
    }
}
export json_parse;
export json_stringify;
