use crate::evaluator::eval;
use crate::lexer::new_lexer;
use crate::object::new_env;
use crate::parser::new_parser;
use std::io;
use std::rc::Rc;

pub const PROMPT: &str = ">> ";

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
        let line = match scanner.next() {
            Some(Ok(line)) => line,
            _ => return,
        };
        let mut p = new_parser(new_lexer(line, Some(String::from("repl"))));
        match p.parse_program() {
            Ok(ref program) => match eval(program, Rc::clone(&env)) {
                Ok(v) => writeln!(output, "{}", v).unwrap(),
                Err(e) => writeln!(output, "{}", e.msg).unwrap(),
            },
            Err(err) => writeln!(output, "{MONKEY_FACE}\n{}", err.msg).unwrap(),
        }
    }
}
