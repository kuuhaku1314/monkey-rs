use crate::evaluator::eval;
use crate::formatter::format_source;
use crate::lang_service::{
    completion_json, definition_json, diagnostics_json, format_source_for_editor, references_json,
    symbols_json,
};
use crate::lexer::new_lexer;
use crate::object::new_env;
use crate::parser::new_parser;
use std::io::Read;
use std::rc::Rc;
use std::{env, fs, io};

mod ast;
mod builtin;
mod error;
mod evaluator;
mod formatter;
mod lang_service;
#[cfg(test)]
mod language_tests;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 3 && args[1] == "--format" {
        if !format_file(&args[2]) {
            std::process::exit(1);
        }
        return;
    }
    if args.len() == 3 && args[1] == "--format-check" {
        if !format_check_file(&args[2]) {
            std::process::exit(1);
        }
        return;
    }
    if args.len() == 3 && args[1] == "--format-stdin" {
        format_stdin(&args[2]);
        return;
    }
    if args.len() == 3 && args[1] == "--diagnose-stdin" {
        diagnose_stdin(&args[2]);
        return;
    }
    if args.len() == 3 && args[1] == "--symbols-stdin" {
        symbols_stdin(&args[2]);
        return;
    }
    if args.len() == 5 && args[1] == "--complete-stdin" {
        complete_stdin(&args[2], &args[3], &args[4]);
        return;
    }
    if args.len() == 5 && args[1] == "--definition-stdin" {
        definition_stdin(&args[2], &args[3], &args[4]);
        return;
    }
    if args.len() == 6 && args[1] == "--references-stdin" {
        references_stdin(&args[2], &args[3], &args[4], &args[5]);
        return;
    }
    if args.len() > 2 {
        eprintln!(
            "Usage: {} <filename>, {} --format <filename>, {} --format-check <filename>, {} --format-stdin <path>, {} --diagnose-stdin <path>, {} --symbols-stdin <path>, {} --complete-stdin <path> <line> <character>, {} --definition-stdin <path> <line> <character>, {} --references-stdin <path> <line> <character> <includeDeclaration>, or {}",
            args[0], args[0], args[0], args[0], args[0], args[0], args[0], args[0], args[0], args[0]
        );
        std::process::exit(2);
    }
    if args.len() == 2 {
        if !parse_file(&args[1]) {
            std::process::exit(1);
        }
    } else {
        run_repl();
    }
}

fn read_stdin() -> Result<String, io::Error> {
    let mut content = String::new();
    io::stdin().read_to_string(&mut content)?;
    Ok(content)
}

fn format_stdin(path: &str) {
    match read_stdin().and_then(|content| {
        format_source_for_editor(&content, Some(path.to_string()))
            .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err.to_string()))
    }) {
        Ok(formatted) => println!("{formatted}"),
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    }
}

fn diagnose_stdin(path: &str) {
    match read_stdin() {
        Ok(content) => println!("{}", diagnostics_json(&content, Some(path.to_string()))),
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    }
}

fn symbols_stdin(path: &str) {
    match read_stdin() {
        Ok(content) => println!("{}", symbols_json(&content, Some(path.to_string()))),
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    }
}

fn complete_stdin(path: &str, line: &str, character: &str) {
    let line = line.parse::<usize>().unwrap_or(0);
    let character = character.parse::<usize>().unwrap_or(0);
    match read_stdin() {
        Ok(content) => println!(
            "{}",
            completion_json(&content, Some(path.to_string()), line, character)
        ),
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    }
}

fn definition_stdin(path: &str, line: &str, character: &str) {
    let line = line.parse::<usize>().unwrap_or(0);
    let character = character.parse::<usize>().unwrap_or(0);
    match read_stdin() {
        Ok(content) => println!(
            "{}",
            definition_json(&content, Some(path.to_string()), line, character)
        ),
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    }
}

fn references_stdin(path: &str, line: &str, character: &str, include_declaration: &str) {
    let line = line.parse::<usize>().unwrap_or(0);
    let character = character.parse::<usize>().unwrap_or(0);
    let include_declaration = include_declaration == "true";
    match read_stdin() {
        Ok(content) => println!(
            "{}",
            references_json(
                &content,
                Some(path.to_string()),
                line,
                character,
                include_declaration
            )
        ),
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    }
}

fn format_file(filename: &String) -> bool {
    match fs::read_to_string(filename) {
        Ok(content) => match format_source(&content, Some(filename.to_owned())) {
            Ok(formatted) => {
                println!("{formatted}");
                true
            }
            Err(err) => {
                println!("{}", err.render(&content, Some(filename.as_str())));
                false
            }
        },
        Err(err) => {
            println!("Error reading file {} {}", filename, err);
            false
        }
    }
}

fn format_check_file(filename: &String) -> bool {
    match fs::read_to_string(filename) {
        Ok(content) => match format_source(&content, Some(filename.to_owned())) {
            Ok(formatted) => {
                if normalize_trailing_newline(&content) == normalize_trailing_newline(&formatted) {
                    true
                } else {
                    println!("{filename} is not formatted");
                    false
                }
            }
            Err(err) => {
                println!("{}", err.render(&content, Some(filename.as_str())));
                false
            }
        },
        Err(err) => {
            println!("Error reading file {} {}", filename, err);
            false
        }
    }
}

fn normalize_trailing_newline(value: &str) -> &str {
    value.trim_end_matches(['\r', '\n'])
}

fn parse_file(filename: &String) -> bool {
    match fs::read_to_string(filename) {
        Ok(content) => {
            let env = new_env();
            let mut parser = new_parser(new_lexer(content.to_owned(), Some(filename.to_owned())));
            match parser.parse_program() {
                Ok(ref program) => match eval(program, Rc::clone(&env)) {
                    Ok(result) => {
                        println!("{result}");
                        true
                    }
                    Err(err) => {
                        println!("{}", err.render(&content, Some(filename.as_str())));
                        false
                    }
                },
                Err(err) => {
                    println!("{}", err.render(&content, Some(filename.as_str())));
                    false
                }
            }
        }
        Err(err) => {
            println!("Error reading file {} {}", filename, err);
            false
        }
    }
}

fn run_repl() {
    println!("Hello! This is the Monkey programming language!");
    println!("Feel free to type in commands");

    let stdin = io::stdin();
    let stdout = io::stdout();
    repl::start(stdin.lock(), &mut stdout.lock());
}
