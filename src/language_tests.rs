use crate::error::ErrorKind;
use crate::evaluator::eval;
use crate::lexer::new_lexer;
use crate::object::{new_env, Object};
use crate::parser::new_parser;
use std::fs;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

fn eval_input(input: &str) -> Result<Object, crate::error::Error> {
    let mut parser = new_parser(new_lexer(input.to_string(), Some("test".to_string())));
    let program = parser.parse_program()?;
    eval(&program, Rc::clone(&new_env()))
}

fn nonzero_exec_expression() -> &'static str {
    if cfg!(windows) {
        r#"process.exec("cmd", ["/C", "exit /B 7"])"#
    } else {
        r#"process.exec("sh", ["-c", "exit 7"])"#
    }
}

fn monkey_string_content(value: &str) -> String {
    let mut output = String::new();
    for ch in value.chars() {
        match ch {
            '\\' => output.push_str("\\\\"),
            '"' => output.push_str("\\\""),
            '\n' => output.push_str("\\n"),
            '\r' => output.push_str("\\r"),
            '\t' => output.push_str("\\t"),
            '\0' => output.push_str("\\0"),
            _ => output.push(ch),
        }
    }
    output
}

fn monkey_string_literal(value: &str) -> String {
    format!("\"{}\"", monkey_string_content(value))
}

#[test]
fn unicode_len_counts_chars_and_byte_len_counts_bytes() {
    match eval_input(
        r#"
        let chars = len("中文");
        let bytes = byte_len("中文");
        if chars == 2 {
            bytes
        } else {
            0
        }
        "#,
    )
    .unwrap()
    {
        Object::Integer(actual) => assert_eq!(actual, 6),
        other => panic!("expected integer 6, got {other}"),
    }
}

#[test]
fn numeric_mixed_operations_are_supported() {
    match eval_input(
        r#"
        let result = 1 + 2.5;
        result = result + (5.0 - 1);
        result = result + (2 * 1.5);
        result = result + (5 / 2.0);
        if true { result }
        "#,
    )
    .unwrap()
    {
        Object::Float(actual) => assert_eq!(actual, 13.0),
        other => panic!("expected float 13.0, got {other}"),
    }
}

#[test]
fn operator_precedence_and_public_list_type_are_stable() {
    match eval_input(
        r#"
        let logic = false || true && false;
        let math = 1 + 2 * 3;
        let c = 0;
        let d = 0;
        c = d = 3;
        if !logic && math == 7 && c == 3 && d == 3 && type([1, 2]) == "list" {
            1
        } else {
            0
        }
        "#,
    )
    .unwrap()
    {
        Object::Integer(actual) => assert_eq!(actual, 1),
        other => panic!("expected integer 1, got {other}"),
    }
}

#[test]
fn map_literal_can_be_implicit_tail_return_value() {
    match eval_input(
        r#"
        fn result() {
            {ok: true, value: 42, error: null}
        }
        let r = result();
        if r.ok {
            r.value
        } else {
            0
        }
        "#,
    )
    .unwrap()
    {
        Object::Integer(actual) => assert_eq!(actual, 42),
        other => panic!("expected integer 42, got {other}"),
    }
}

#[test]
fn standalone_block_statement_keeps_its_own_scope() {
    match eval_input(
        r#"
        let value = 1;
        {
            let value = 2;
        }
        if true {
            value
        }
        "#,
    )
    .unwrap()
    {
        Object::Integer(actual) => assert_eq!(actual, 1),
        other => panic!("expected integer 1, got {other}"),
    }
}

#[test]
fn division_by_zero_has_runtime_error_kind() {
    let err = match eval_input("1 / 0") {
        Ok(value) => panic!("expected division error, got {value}"),
        Err(err) => err,
    };
    assert_eq!(err.kind, ErrorKind::Runtime);
    assert!(err.msg.contains("division by zero"), "{}", err.msg);
}

#[test]
fn namespace_import_exposes_module_members() {
    let path = temp_file_path("monkey_namespace_import.monkey");
    let path_literal = monkey_string_literal(path.as_str());
    fs::write(
        path.as_str(),
        r#"
        fn add_one(x) {
            x + 1
        }
        let name = "math";
        export add_one;
        export name;
        "#,
    )
    .unwrap();
    let source = format!(
        r#"
        import math from {path_literal};
        let result = math.add_one(2);
        if math.name == "math" {{
            result
        }} else {{
            0
        }}
        "#
    );

    match eval_input(source.as_str()).unwrap() {
        Object::Integer(actual) => assert_eq!(actual, 3),
        other => panic!("expected integer 3, got {other}"),
    }
    let _ = fs::remove_file(path);
}

#[test]
fn assignment_statement_handles_identifier_index_and_member_targets() {
    match eval_input(
        r#"
        let x = 1;
        x = 2;
        let data = {items: [0, 0, 0]};
        let i = 1;
        data.items[i + 1] = x + 5;
        data.name = "ok";
        if data.name == "ok" {
            data.items[2]
        } else {
            0
        }
        "#,
    )
    .unwrap()
    {
        Object::Integer(actual) => assert_eq!(actual, 7),
        other => panic!("expected integer 7, got {other}"),
    }
}

#[test]
fn struct_definition_initialization_and_assignment_work() {
    match eval_input(
        r#"
        struct User {
            name;
            age;
        }
        let user = User{name: "tom", age: 18};
        user.age = user.age + 1;
        user["name"] = user.name + "-cat";
        if user.name == "tom-cat" {
            user.age
        } else {
            0
        }
        "#,
    )
    .unwrap()
    {
        Object::Integer(actual) => assert_eq!(actual, 19),
        other => panic!("expected integer 19, got {other}"),
    }
}

#[test]
fn struct_definition_rejects_comma_separated_fields() {
    let err = match eval_input(
        r#"
        struct User { name, age };
        "#,
    ) {
        Ok(value) => panic!("expected struct syntax error, got {value}"),
        Err(err) => err,
    };
    assert_eq!(err.kind, ErrorKind::Syntax);
}

#[test]
fn struct_definition_requires_semicolon_after_each_field() {
    let err = match eval_input(
        r#"
        struct User { name }
        "#,
    ) {
        Ok(value) => panic!("expected struct syntax error, got {value}"),
        Err(err) => err,
    };
    assert_eq!(err.kind, ErrorKind::Syntax);
}

#[test]
fn struct_positional_initialization_uses_field_order() {
    match eval_input(
        r#"
        struct User {
            name;
            age;
        }
        let user = User{"tom", 18};
        if user.name == "tom" {
            user.age
        } else {
            0
        }
        "#,
    )
    .unwrap()
    {
        Object::Integer(actual) => assert_eq!(actual, 18),
        other => panic!("expected integer 18, got {other}"),
    }
}

#[test]
fn struct_positional_initialization_requires_all_fields() {
    let err = match eval_input(
        r#"
        struct User { name; age; }
        let user = User{"tom"};
        "#,
    ) {
        Ok(value) => panic!("expected positional struct error, got {value}"),
        Err(err) => err,
    };
    assert_eq!(err.kind, ErrorKind::Type);
    assert!(err.msg.contains("expects 2 values"), "{}", err.msg);
}

#[test]
fn struct_literal_rejects_mixed_named_and_positional_fields() {
    let err = match eval_input(
        r#"
        struct User { name; age; }
        let user = User{name: "tom", 18};
        "#,
    ) {
        Ok(value) => panic!("expected mixed struct literal error, got {value}"),
        Err(err) => err,
    };
    assert_eq!(err.kind, ErrorKind::Syntax);
}

#[test]
fn builtin_functions_can_be_stored_and_called() {
    match eval_input(
        r#"
        let to_text = str;
        to_text(12)
        "#,
    )
    .unwrap()
    {
        Object::String(actual) => assert_eq!(actual, "12"),
        other => panic!("expected string 12, got {other}"),
    }
}

#[test]
fn struct_instances_can_be_reassigned() {
    match eval_input(
        r#"
        struct User { name; }
        let user = User{name: "a"};
        user = User{name: "b"};
        user.name
        "#,
    )
    .unwrap()
    {
        Object::String(actual) => assert_eq!(actual, "b"),
        other => panic!("expected string b, got {other}"),
    }
}

#[test]
fn struct_fields_can_change_runtime_type() {
    match eval_input(
        r#"
        struct User { age; }
        let user = User{age: 18};
        user.age = "18";
        user.age
        "#,
    )
    .unwrap()
    {
        Object::String(actual) => assert_eq!(actual, "18"),
        other => panic!("expected string 18, got {other}"),
    }
}

#[test]
fn assignment_allows_dynamic_type_changes() {
    match eval_input(
        r#"
        let value = 1;
        value = "1";
        value
        "#,
    )
    .unwrap()
    {
        Object::String(actual) => assert_eq!(actual, "1"),
        other => panic!("expected string 1, got {other}"),
    }
}

#[test]
fn struct_missing_fields_default_to_null() {
    match eval_input(
        r#"
        struct User { name; age; }
        let user = User{name: "tom"};
        if user.age == null {
            1
        } else {
            0
        }
        "#,
    )
    .unwrap()
    {
        Object::Integer(actual) => assert_eq!(actual, 1),
        other => panic!("expected integer 1, got {other}"),
    }
}

#[test]
fn struct_rejects_unknown_fields() {
    let err = match eval_input(
        r#"
        struct User { name; }
        let user = User{name: "tom", age: 18};
        "#,
    ) {
        Ok(value) => panic!("expected struct field error, got {value}"),
        Err(err) => err,
    };
    assert_eq!(err.kind, ErrorKind::Name);
    assert!(err.msg.contains("unknown field"), "{}", err.msg);
}

#[test]
fn struct_rejects_duplicate_literal_fields() {
    let err = match eval_input(
        r#"
        struct User { name; }
        let user = User{name: "a", name: "b"};
        "#,
    ) {
        Ok(value) => panic!("expected duplicate field error, got {value}"),
        Err(err) => err,
    };
    assert_eq!(err.kind, ErrorKind::Name);
    assert!(err.msg.contains("duplicate field"), "{}", err.msg);
}

#[test]
fn struct_rejects_unknown_field_reads() {
    let err = match eval_input(
        r#"
        struct User { name; }
        let user = User{name: "tom"};
        user.age
        "#,
    ) {
        Ok(value) => panic!("expected unknown field error, got {value}"),
        Err(err) => err,
    };
    assert_eq!(err.kind, ErrorKind::Name);
    assert!(err.msg.contains("unknown field"), "{}", err.msg);
}

#[test]
fn struct_literal_is_allowed_inside_condition_call_arguments() {
    match eval_input(
        r#"
        struct User { name; }
        fn ok(user) {
            user.name == "tom"
        }
        if ok(User{name: "tom"}) {
            1
        } else {
            0
        }
        "#,
    )
    .unwrap()
    {
        Object::Integer(actual) => assert_eq!(actual, 1),
        other => panic!("expected integer 1, got {other}"),
    }
}

#[test]
fn alias_and_bare_import_share_cache_without_reexecuting() {
    let path = temp_file_path("monkey_mixed_import_cache.monkey");
    let path_literal = monkey_string_literal(path.as_str());
    fs::write(
        path.as_str(),
        "counter = counter + 1;\nfn value() { counter }\nexport value;\n",
    )
    .unwrap();
    let source = format!(
        r#"
        let counter = 0;
        import m from {path_literal};
        import {path_literal};
        let result = m.value();
        result = result + counter;
        if true {{ result }}
        "#
    );

    match eval_input(source.as_str()).unwrap() {
        Object::Integer(actual) => assert_eq!(actual, 2),
        other => panic!("expected integer 2, got {other}"),
    }
    let _ = fs::remove_file(path);
}

#[test]
fn cached_bare_import_injects_exports_into_each_scope() {
    let path = temp_file_path("monkey_cached_bare_import_scope.monkey");
    let path_literal = monkey_string_literal(path.as_str());
    fs::write(path.as_str(), "fn value() { 7 }\nexport value;\n").unwrap();
    let source = format!(
        r#"
        if true {{
            import {path_literal};
        }}
        if true {{
            import {path_literal};
            value()
        }}
        "#
    );

    match eval_input(source.as_str()).unwrap() {
        Object::Integer(actual) => assert_eq!(actual, 7),
        other => panic!("expected integer 7, got {other}"),
    }
    let _ = fs::remove_file(path);
}

#[test]
fn import_respects_explicit_exports() {
    let path = temp_file_path("monkey_explicit_export.monkey");
    let path_literal = monkey_string_literal(path.as_str());
    fs::write(
        path.as_str(),
        r#"
        let public_value = 7;
        let private_value = 9;
        export public_value;
        "#,
    )
    .unwrap();
    let source = format!(
        r#"
        import m from {path_literal};
        m.public_value + m.private_value
        "#
    );

    let err = match eval_input(source.as_str()) {
        Ok(value) => panic!("expected private export error, got {value}"),
        Err(err) => err,
    };
    assert_eq!(err.kind, ErrorKind::Name);
    let _ = fs::remove_file(path);
}

#[test]
fn namespace_import_struct_can_be_instantiated_with_qualified_name() {
    let path = temp_file_path("monkey_namespace_struct.monkey");
    let path_literal = monkey_string_literal(path.as_str());
    fs::write(
        path.as_str(),
        r#"
        struct User {
            name;
            age;
        }
        export User;
        "#,
    )
    .unwrap();
    let source = format!(
        r#"
        import models from {path_literal};
        let user = models.User{{name: "tom", age: 18}};
        user.name + ":" + str(user.age)
        "#
    );

    match eval_input(source.as_str()) {
        Ok(Object::String(actual)) => assert_eq!(actual, "tom:18"),
        Ok(value) => panic!("expected string tom:18, got {value}"),
        Err(err) => panic!("expected qualified struct literal to evaluate, got {err}"),
    }
    let _ = fs::remove_file(path);
}

#[test]
fn bare_import_respects_explicit_exports() {
    let path = temp_file_path("monkey_bare_explicit_export.monkey");
    let path_literal = monkey_string_literal(path.as_str());
    fs::write(
        path.as_str(),
        r#"
        let public_value = 7;
        let private_value = 9;
        export public_value;
        "#,
    )
    .unwrap();
    let source = format!(
        r#"
        import {path_literal};
        public_value
        "#
    );

    match eval_input(source.as_str()).unwrap() {
        Object::Integer(actual) => assert_eq!(actual, 7),
        other => panic!("expected integer 7, got {other}"),
    }

    let private_source = format!(
        r#"
        import {path_literal};
        private_value
        "#
    );
    let err = match eval_input(private_source.as_str()) {
        Ok(value) => panic!("expected missing private name, got {value}"),
        Err(err) => err,
    };
    assert_eq!(err.kind, ErrorKind::Name);
    let _ = fs::remove_file(path);
}

#[test]
fn modules_without_explicit_exports_do_not_export_bindings() {
    let path = temp_file_path("monkey_no_export.monkey");
    let path_literal = monkey_string_literal(path.as_str());
    fs::write(
        path.as_str(),
        r#"
        let value = 7;
        "#,
    )
    .unwrap();
    let source = format!(
        r#"
        import m from {path_literal};
        m.value
        "#
    );

    let err = match eval_input(source.as_str()) {
        Ok(value) => panic!("expected missing export error, got {value}"),
        Err(err) => err,
    };
    assert_eq!(err.kind, ErrorKind::Name);
    let _ = fs::remove_file(path);
}

#[test]
fn crlf_source_parses_like_lf_source() {
    match eval_input("let x = 1;\r\nlet y = 2;\r\nx + y") {
        Ok(Object::Integer(actual)) => assert_eq!(actual, 3),
        Ok(value) => panic!("expected integer 3, got {value}"),
        Err(err) => panic!("expected CRLF source to parse, got {err}"),
    }
}

#[test]
fn leading_blank_lines_are_preserved_in_error_spans() {
    let err = match eval_input("\n\nlet x = ;") {
        Ok(value) => panic!("expected parse error, got {value}"),
        Err(err) => err,
    };
    assert_eq!(err.span.unwrap().start.0, 3);
}

#[test]
fn file_failures_return_result_instead_of_runtime_error() {
    let missing = temp_file_path("missing_result_file.txt");
    let missing_literal = monkey_string_literal(missing.as_str());
    let source = format!(
        r#"
        import fs from "stdlib/fs.monkey";
        let result = fs.read_file({missing_literal});
        if result.ok {{
            0
        }} else if result.error.kind == "io" {{
            if result.error.code == "not_found" {{
                1
            }} else {{
                2
            }}
        }} else {{
            3
        }}
        "#
    );

    match eval_input(source.as_str()) {
        Ok(Object::Integer(actual)) => assert_eq!(actual, 1),
        Ok(value) => panic!("expected integer 1, got {value}"),
        Err(err) => panic!("expected result object, got error {err}"),
    }
}

#[test]
fn exec_nonzero_status_is_successful_result_value() {
    let source = r#"
        import process from "stdlib/process.monkey";
        let result = __NONZERO_EXEC__;
        if result.ok && !result.value.success {
            result.value.status
        } else {
            0
        }
        "#
    .replace("__NONZERO_EXEC__", nonzero_exec_expression());

    match eval_input(source.as_str()) {
        Ok(Object::Integer(actual)) => assert_eq!(actual, 7),
        Ok(value) => panic!("expected integer 7, got {value}"),
        Err(err) => panic!("expected exec result object, got error {err}"),
    }
}

#[test]
fn json_parse_returns_standard_result() {
    match eval_input(
        r#"
        import json from "stdlib/json.monkey";
        let good = json.json_parse("{\"answer\":42}");
        let bad = json.json_parse("{bad");
        if good.ok && good.value.answer == 42 && !bad.ok && bad.error.kind == "parse" {
            1
        } else {
            0
        }
        "#,
    ) {
        Ok(Object::Integer(actual)) => assert_eq!(actual, 1),
        Ok(value) => panic!("expected integer 1, got {value}"),
        Err(err) => panic!("expected json result objects, got error {err}"),
    }
}

#[test]
fn json_parse_number_errors_stay_in_result() {
    match eval_input(
        r#"
        import json from "stdlib/json.monkey";
        let overflow = json.json_parse("999999999999999999999999999999999999");
        let exponent = json.json_parse("1.5e2");
        let leading_zero = json.json_parse("01");
        if !overflow.ok &&
            overflow.error.kind == "parse" &&
            exponent.ok &&
            exponent.value == 150.0 &&
            !leading_zero.ok {
            1
        } else {
            0
        }
        "#,
    ) {
        Ok(Object::Integer(actual)) => assert_eq!(actual, 1),
        Ok(value) => panic!("expected integer 1, got {value}"),
        Err(err) => panic!("expected json result objects, got error {err}"),
    }
}

#[test]
fn parse_number_helpers_return_result() {
    match eval_input(
        r#"
        let int_ok = parse_int("12");
        let int_bad = parse_int("abc");
        let float_ok = parse_float("1.25e2");
        if int_ok.ok &&
            int_ok.value == 12 &&
            !int_bad.ok &&
            int_bad.error.code == "invalid_int" &&
            float_ok.ok &&
            float_ok.value == 125.0 {
            1
        } else {
            0
        }
        "#,
    ) {
        Ok(Object::Integer(actual)) => assert_eq!(actual, 1),
        Ok(value) => panic!("expected integer 1, got {value}"),
        Err(err) => panic!("expected parse result objects, got error {err}"),
    }
}

#[test]
fn http_invalid_method_returns_result_error() {
    match eval_input(
        r#"
        import http from "stdlib/http.monkey";
        let result = http.http_request("BAD METHOD", "http://127.0.0.1", "", {});
        if !result.ok && result.error.kind == "http" && result.error.code == "invalid_method" {
            1
        } else {
            0
        }
        "#,
    ) {
        Ok(Object::Integer(actual)) => assert_eq!(actual, 1),
        Ok(value) => panic!("expected integer 1, got {value}"),
        Err(err) => panic!("expected http result object, got error {err}"),
    }
}

fn temp_file_path(name: &str) -> String {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    std::env::temp_dir()
        .join(format!("{nanos}_{name}"))
        .to_string_lossy()
        .into_owned()
}
