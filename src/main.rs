use crate::evaluator::eval;
use crate::lexer::new_lexer;
use crate::object::new_env;
use crate::parser::new_parser;
use std::io::BufRead;
use std::rc::Rc;
use std::thread::sleep;
use std::{env, fs, io, time};

mod ast;
mod buildin;
mod error;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        println!("Usage: {} <filename> or {}", args[0], args[0]);
        sleep(time::Duration::from_millis(30000));
        return;
    }
    if args.len() == 2 {
        parse_file(&args[1]);
    } else {
        run_repl();
    }
}

fn parse_file(filename: &String) {
    match fs::read_to_string(filename) {
        Ok(mut content) => {
            content = content.replace("\r\n", "\n").trim().to_owned();
            let env = new_env();
            let mut parser = new_parser(new_lexer(content.to_owned(), Some(filename.to_owned())));
            match parser.parse_program() {
                Ok(ref program) => match eval(program, Rc::clone(&env)) {
                    Ok(result) => {
                        println!("{result}")
                    }
                    Err(err) => {
                        println!("{}\n\nParsed Program Code:\n{}", err.msg, program.string());
                        println!("--------------------------------------------");
                        println!("{}\n\nYOUR SOURCE CODE:\n{}", err.msg, content)
                    }
                },
                Err(err) => {
                    println!("{}\n\nYOUR SOURCE CODE:\n{}", err.msg, content)
                }
            }
        }
        Err(err) => {
            println!("Error reading file {} {}", filename, err)
        }
    }
    let _ = &io::stdin().lock().lines().next();
}

fn run_repl() {
    println!("Hello! This is the Monkey programming language!");
    println!("Feel free to type in commands");

    let stdin = io::stdin();
    let stdout = io::stdout();
    repl::start(stdin.lock(), &mut stdout.lock());
}
