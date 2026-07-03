use crate::evaluator::eval;
use crate::lexer::new_lexer;
use crate::object::new_env;
use crate::parser::new_parser;
use std::io;
use std::rc::Rc;

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
    let env = new_env();
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
        let mut p = new_parser(new_lexer(line.to_owned(), Some(String::from("repl"))));
        match p.parse_program() {
            Ok(ref program) => match eval(program, Rc::clone(&env)) {
                Ok(v) => writeln!(output, "{}", v).unwrap(),
                Err(e) => writeln!(output, "{}", e.render(&line, Some("repl"))).unwrap(),
            },
            Err(err) => {
                writeln!(output, "{MONKEY_FACE}\n{}", err.render(&line, Some("repl"))).unwrap()
            }
        }
    }
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
    use super::needs_more_input;

    #[test]
    fn multiline_detection_ignores_strings_and_comments() {
        assert!(needs_more_input("fn f() {"));
        assert!(!needs_more_input("fn f() {\n1\n}"));
        assert!(!needs_more_input("let s = \"{\";"));
        assert!(!needs_more_input("// {\n1"));
    }
}
