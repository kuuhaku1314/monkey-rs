use crate::ast::{Program, Statement};
use crate::lexer::new_lexer;
use crate::parser::new_parser;
use crate::vm::eval_vm;
use std::io;

pub const PROMPT: &str = ">> ";
pub const CONTINUATION_PROMPT: &str = ".. ";

pub const MONKEY_FACE: &str = r#"            __,__
    .--.  .-"     "-.  .--.
/ .. \/  .-. .-.  \/ .. \
| |  '|  /   Y   \  |'  | |
| \   \  \ 0 | 0 /  /  /  |
\ '- ,\.-"""""""-./, -'  /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
    """#;

pub fn start<R: io::BufRead, W: io::Write>(input: R, output: &mut W) {
    let mut scanner = input.lines();
    let mut history = String::new();
    loop {
        write!(output, "{}", PROMPT).unwrap();
        output.flush().unwrap();
        let mut line = match scanner.next() {
            Some(Ok(line)) => line,
            _ => return,
        };
        while needs_more_input(line.as_str()) {
            write!(output, "{}", CONTINUATION_PROMPT).unwrap();
            output.flush().unwrap();
            match scanner.next() {
                Some(Ok(next_line)) => {
                    line.push('\n');
                    line.push_str(next_line.as_str());
                }
                _ => return,
            }
        }
        let source = repl_source(history.as_str(), line.as_str());
        let mut p = new_parser(new_lexer(source.to_owned(), Some(String::from("repl"))));
        match p.parse_program() {
            Ok(ref program) => match eval_vm(program) {
                Ok(v) => {
                    if !v.is_empty() {
                        writeln!(output, "{}", v).unwrap();
                    }
                    if should_persist_repl_input(program) {
                        append_repl_history(&mut history, line.as_str());
                    }
                }
                Err(e) => writeln!(output, "{}", e.render(&source, Some("repl"))).unwrap(),
            },
            Err(err) => writeln!(
                output,
                "{MONKEY_FACE}\n{}",
                err.render(&source, Some("repl"))
            )
            .unwrap(),
        }
    }
}

fn repl_source(history: &str, line: &str) -> String {
    if history.is_empty() {
        line.to_string()
    } else {
        format!("{history}\n{line}")
    }
}

fn append_repl_history(history: &mut String, line: &str) {
    if !history.is_empty() {
        history.push('\n');
    }
    history.push_str(line);
}

fn should_persist_repl_input(program: &Program) -> bool {
    program.statements.iter().any(|statement| {
        matches!(
            statement,
            Statement::Let(_)
                | Statement::Struct(_)
                | Statement::Import(_)
                | Statement::Export(_)
                | Statement::Assign(_)
        )
    })
}

fn needs_more_input(input: &str) -> bool {
    let mut paren = 0i64;
    let mut brace = 0i64;
    let mut bracket = 0i64;
    let mut chars = input.chars().peekable();
    let mut in_string = false;
    let mut escaped = false;
    while let Some(ch) = chars.next() {
        if in_string {
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == '"' {
                in_string = false;
            }
            continue;
        }
        if ch == '/' && chars.peek() == Some(&'/') {
            for next in chars.by_ref() {
                if next == '\n' {
                    break;
                }
            }
            continue;
        }
        match ch {
            '"' => in_string = true,
            '(' => paren += 1,
            ')' => paren -= 1,
            '{' => brace += 1,
            '}' => brace -= 1,
            '[' => bracket += 1,
            ']' => bracket -= 1,
            _ => {}
        }
    }
    in_string || paren > 0 || brace > 0 || bracket > 0
}

#[cfg(test)]
mod tests {
    use super::{needs_more_input, should_persist_repl_input};
    use crate::lexer::new_lexer;
    use crate::parser::new_parser;

    #[test]
    fn multiline_detection_ignores_strings_and_comments() {
        assert!(needs_more_input("fn f() {"));
        assert!(!needs_more_input("fn f() {\n1\n}"));
        assert!(!needs_more_input("let s = \"{\";"));
        assert!(!needs_more_input("// {\n1"));
    }

    #[test]
    fn repl_persists_bindings_but_not_plain_expressions() {
        assert!(should_persist("let x = 1;"));
        assert!(should_persist("x = x + 1;"));
        assert!(!should_persist("x + 1"));
        assert!(!should_persist("print(x);"));
    }

    fn should_persist(source: &str) -> bool {
        let mut parser = new_parser(new_lexer(source.to_string(), Some("repl_test".to_string())));
        should_persist_repl_input(&parser.parse_program().unwrap())
    }
}
