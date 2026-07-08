use crate::formatter::format_source;
use crate::lang_service::{
    completion_json, definition_json, diagnostics_json, format_source_for_editor, references_json,
    symbols_json,
};
use crate::lexer::new_lexer;
use crate::parser::new_parser;
use crate::vm::{benchmark_vm, dump_vm_bytecode, dump_vm_ir, eval_vm, eval_vm_profile, VmProfile};
use std::io::Read;
use std::time::{Duration, Instant};
use std::{env, fs, io};

mod ast;
mod error;
mod formatter;
mod lang_service;
mod lang_service_builtins;
#[cfg(test)]
mod language_tests;
mod lexer;
mod map_key;
mod parser;
mod repl;
mod token;
mod vm;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 2 && (args[1] == "--version" || args[1] == "-V") {
        println!("monkey-rs {}", env!("CARGO_PKG_VERSION"));
        return;
    }
    if args.len() == 3 && args[1] == "--format" {
        if !format_file(&args[2]) {
            std::process::exit(1);
        }
        return;
    }
    if args.len() == 3 && args[1] == "--profile-vm" {
        if !profile_file_with_vm(&args[2]) {
            std::process::exit(1);
        }
        return;
    }
    if args.len() == 3 && args[1] == "--dump-vm-ir" {
        if !dump_file_with_vm(&args[2], VmDumpKind::Hir) {
            std::process::exit(1);
        }
        return;
    }
    if args.len() == 3 && args[1] == "--dump-bytecode" {
        if !dump_file_with_vm(&args[2], VmDumpKind::Bytecode) {
            std::process::exit(1);
        }
        return;
    }
    if args.len() == 2 && args[1] == "--bench-vm" {
        if !bench_vm_examples() {
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
        print_usage(&args[0]);
        std::process::exit(2);
    }
    if args.len() == 2 {
        if !run_file(&args[1]) {
            std::process::exit(1);
        }
    } else {
        run_repl();
    }
}

fn print_usage(binary: &str) {
    eprintln!("Usage:");
    eprintln!("  {binary} <filename>");
    eprintln!("  {binary} --profile-vm <filename>");
    eprintln!("  {binary} --dump-vm-ir <filename>");
    eprintln!("  {binary} --dump-bytecode <filename>");
    eprintln!("  {binary} --bench-vm");
    eprintln!("  {binary} --format <filename>");
    eprintln!("  {binary} --format-check <filename>");
    eprintln!("  {binary} --format-stdin <path>");
    eprintln!("  {binary} --diagnose-stdin <path>");
    eprintln!("  {binary} --symbols-stdin <path>");
    eprintln!("  {binary} --complete-stdin <path> <line> <character>");
    eprintln!("  {binary} --definition-stdin <path> <line> <character>");
    eprintln!("  {binary} --references-stdin <path> <line> <character> <includeDeclaration>");
    eprintln!("  {binary} --version");
    eprintln!("  {binary}");
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
                println!("{}", err.render(&content, Some(filename)));
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
                println!("{}", err.render(&content, Some(filename)));
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

enum VmDumpKind {
    Hir,
    Bytecode,
}

fn dump_file_with_vm(filename: &String, kind: VmDumpKind) -> bool {
    match fs::read_to_string(filename) {
        Ok(content) => {
            let lexer = new_lexer(content.to_string(), Some(filename.to_string()));
            let mut parser = new_parser(lexer);
            let program = match parser.parse_program() {
                Ok(program) => program,
                Err(err) => {
                    println!("{}", err.render(&content, Some(filename)));
                    return false;
                }
            };
            let dumped = match kind {
                VmDumpKind::Hir => dump_vm_ir(&program),
                VmDumpKind::Bytecode => dump_vm_bytecode(&program),
            };
            match dumped {
                Ok(output) => {
                    print!("{output}");
                    true
                }
                Err(err) => {
                    println!("{}", err.render(&content, Some(filename)));
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

fn run_file(filename: &String) -> bool {
    match fs::read_to_string(filename) {
        Ok(content) => {
            let lexer = new_lexer(content.to_string(), Some(filename.to_string()));
            let mut parser = new_parser(lexer);
            match parser.parse_program() {
                Ok(program) => match eval_vm(&program) {
                    Ok(_) => true,
                    Err(err) => {
                        println!("{}", err.render(&content, Some(filename)));
                        false
                    }
                },
                Err(err) => {
                    println!("{}", err.render(&content, Some(filename)));
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

fn profile_file_with_vm(filename: &String) -> bool {
    match fs::read_to_string(filename) {
        Ok(content) => {
            let lexer = new_lexer(content.to_string(), Some(filename.to_string()));
            let mut parser = new_parser(lexer);
            match parser.parse_program() {
                Ok(program) => {
                    let start = Instant::now();
                    match eval_vm_profile(&program) {
                        Ok((value, profile)) => {
                            let elapsed = start.elapsed();
                            if !value.is_empty() {
                                println!("result: {value}");
                            }
                            println!("elapsed: {:?}", elapsed);
                            print_vm_profile(&profile);
                            true
                        }
                        Err(err) => {
                            println!("{}", err.render(&content, Some(filename)));
                            false
                        }
                    }
                }
                Err(err) => {
                    println!("{}", err.render(&content, Some(filename)));
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

fn print_vm_profile(profile: &VmProfile) {
    println!("total_instructions: {}", profile.total_instructions);
    println!("max_register_count: {}", profile.max_register_count);
    println!("max_frame_depth: {}", profile.max_frame_depth);
    println!("call_count: {}", profile.call_count);
    println!("import_count: {}", profile.import_count);
    println!("import_cache_hits: {}", profile.import_cache_hits);
    println!("heap_allocated: {}", profile.heap_allocated);
    println!("instruction_counts:");
    for (name, count) in profile.sorted_instruction_counts() {
        println!("  {name}: {count}");
    }
    if !profile.instruction_elapsed.is_empty() {
        println!("instruction_elapsed:");
        for (name, elapsed) in profile.sorted_instruction_elapsed() {
            println!("  {name}: {elapsed:?}");
        }
    }
    if !profile.builtin_counts.is_empty() {
        println!("builtin_counts:");
        for (name, count) in profile.sorted_builtin_counts() {
            println!("  {name}: {count}");
        }
    }
    if !profile.builtin_elapsed.is_empty() {
        println!("builtin_elapsed:");
        for (name, elapsed) in profile.sorted_builtin_elapsed() {
            println!("  {name}: {elapsed:?}");
        }
    }
    if !profile.allocation_counts.is_empty() {
        println!("allocation_counts:");
        for (name, count) in profile.sorted_allocation_counts() {
            println!("  {name}: {count}");
        }
    }
}

struct VmBenchmarkRow {
    file: String,
    compile_elapsed: Duration,
    execute_elapsed: Duration,
    profile_execute_elapsed: Duration,
    instructions: u64,
    max_registers: usize,
    max_frames: usize,
    heap_allocated: usize,
    calls: u64,
    imports: u64,
    result: String,
}

fn benchmark_vm_file(filename: &str) -> Result<VmBenchmarkRow, ()> {
    match fs::read_to_string(filename) {
        Ok(content) => {
            let mut parser = new_parser(new_lexer(content.to_owned(), Some(filename.to_owned())));
            let program = match parser.parse_program() {
                Ok(program) => program,
                Err(err) => {
                    println!("{}", err.render(&content, Some(filename)));
                    return Err(());
                }
            };

            match benchmark_vm(&program) {
                Ok(benchmark) => Ok(VmBenchmarkRow {
                    file: filename.to_string(),
                    compile_elapsed: benchmark.compile_elapsed,
                    execute_elapsed: benchmark.execute_elapsed,
                    profile_execute_elapsed: benchmark.profile_execute_elapsed,
                    instructions: benchmark.profile.total_instructions,
                    max_registers: benchmark.profile.max_register_count,
                    max_frames: benchmark.profile.max_frame_depth,
                    heap_allocated: benchmark.profile.heap_allocated,
                    calls: benchmark.profile.call_count,
                    imports: benchmark.profile.import_count,
                    result: benchmark_result_summary(&benchmark.value.to_string()),
                }),
                Err(err) => {
                    println!("vm error:");
                    println!("{}", err.render(&content, Some(filename)));
                    Err(())
                }
            }
        }
        Err(err) => {
            println!("Error reading file {} {}", filename, err);
            Err(())
        }
    }
}

fn bench_vm_examples() -> bool {
    let dir = "examples/benchmarks";
    let entries = match fs::read_dir(dir) {
        Ok(entries) => entries,
        Err(err) => {
            println!("Error reading benchmark dir {} {}", dir, err);
            return false;
        }
    };
    let mut files = entries
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|path| path.extension().and_then(|ext| ext.to_str()) == Some("monkey"))
        .collect::<Vec<_>>();
    files.sort();
    if files.is_empty() {
        println!("No benchmark files found in {dir}");
        return false;
    }
    let mut rows = Vec::new();
    let mut ok = true;
    for path in files {
        let filename = path.to_string_lossy().into_owned();
        match benchmark_vm_file(&filename) {
            Ok(row) => rows.push(row),
            Err(()) => ok = false,
        }
    }
    if !rows.is_empty() {
        print_vm_benchmark_report(&rows);
    }
    ok
}

fn print_vm_benchmark_report(rows: &[VmBenchmarkRow]) {
    println!(
        "{:<32} {:>10} {:>10} {:>10} {:>12} {:>8} {:>8} {:>10} {:>8} {:>8}  result",
        "file",
        "compile_ms",
        "exec_ms",
        "profile_ms",
        "instr",
        "regs",
        "frames",
        "heap",
        "calls",
        "imports"
    );
    println!("{}", "-".repeat(142));
    let mut total_compile_elapsed = Duration::ZERO;
    let mut total_execute_elapsed = Duration::ZERO;
    let mut total_profile_execute_elapsed = Duration::ZERO;
    let mut total_instructions = 0;
    let mut total_heap = 0;
    let mut total_calls = 0;
    let mut total_imports = 0;
    for row in rows {
        total_compile_elapsed += row.compile_elapsed;
        total_execute_elapsed += row.execute_elapsed;
        total_profile_execute_elapsed += row.profile_execute_elapsed;
        total_instructions += row.instructions;
        total_heap += row.heap_allocated;
        total_calls += row.calls;
        total_imports += row.imports;
        println!(
            "{:<32} {:>10.3} {:>10.3} {:>10.3} {:>12} {:>8} {:>8} {:>10} {:>8} {:>8}  {}",
            benchmark_file_label(&row.file),
            duration_ms(row.compile_elapsed),
            duration_ms(row.execute_elapsed),
            duration_ms(row.profile_execute_elapsed),
            row.instructions,
            row.max_registers,
            row.max_frames,
            row.heap_allocated,
            row.calls,
            row.imports,
            row.result
        );
    }
    println!("{}", "-".repeat(142));
    println!(
        "{:<32} {:>10.3} {:>10.3} {:>10.3} {:>12} {:>8} {:>8} {:>10} {:>8} {:>8}",
        format!("total ({} files)", rows.len()),
        duration_ms(total_compile_elapsed),
        duration_ms(total_execute_elapsed),
        duration_ms(total_profile_execute_elapsed),
        total_instructions,
        "",
        "",
        total_heap,
        total_calls,
        total_imports
    );
}

fn benchmark_file_label(path: &str) -> String {
    std::path::Path::new(path)
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or(path)
        .to_string()
}

fn benchmark_result_summary(result: &str) -> String {
    if result.is_empty() {
        return "-".to_string();
    }
    const MAX_LEN: usize = 32;
    let mut chars = result.chars();
    let summary = chars.by_ref().take(MAX_LEN).collect::<String>();
    if chars.next().is_some() {
        format!("{summary}...")
    } else {
        summary
    }
}

fn duration_ms(duration: Duration) -> f64 {
    duration.as_secs_f64() * 1000.0
}

fn run_repl() {
    println!("Hello! This is the Monkey programming language!");
    println!("Feel free to type in commands");

    let stdin = io::stdin();
    let stdout = io::stdout();
    repl::start(stdin.lock(), &mut stdout.lock());
}
