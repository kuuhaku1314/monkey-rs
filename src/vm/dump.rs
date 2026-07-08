use super::hir::{FunctionIr, HirInstruction};
use super::program::CompiledFunction;
use super::register::RegisterInstruction;
use std::fmt::Write;

pub(super) fn dump_hir(ir: &FunctionIr) -> String {
    let mut output = String::new();
    writeln!(&mut output, "HIR").expect("writing to string cannot fail");
    for (index, instruction) in ir.instructions.iter().enumerate() {
        writeln!(
            &mut output,
            "{index:04} {}",
            format_hir_instruction(instruction)
        )
        .expect("writing to string cannot fail");
    }
    output
}

pub(super) fn dump_bytecode(function: &CompiledFunction) -> String {
    let mut output = String::new();
    dump_function_bytecode(function, 0, &mut output);
    output
}

fn dump_function_bytecode(function: &CompiledFunction, depth: usize, output: &mut String) {
    let indent = "  ".repeat(depth);
    let name = function.name.as_deref().unwrap_or("<main>");
    writeln!(
        output,
        "{indent}Function {name} locals={} registers={} params={} upvalues={}",
        function.local_count,
        function.registers.register_count,
        function.parameters.len(),
        function.upvalues.len()
    )
    .expect("writing to string cannot fail");
    if !function.constants.is_empty() {
        writeln!(output, "{indent}  constants:").expect("writing to string cannot fail");
        for (index, constant) in function.constants.iter().enumerate() {
            writeln!(output, "{indent}    c{index} = {constant:?}")
                .expect("writing to string cannot fail");
        }
    }
    writeln!(output, "{indent}  bytecode:").expect("writing to string cannot fail");
    for (index, instruction) in function.registers.instructions.iter().enumerate() {
        writeln!(
            output,
            "{indent}    {index:04} {}",
            format_register_instruction(instruction)
        )
        .expect("writing to string cannot fail");
    }
    for instruction in &function.registers.instructions {
        if let RegisterInstruction::MakeFunction { function, .. } = instruction {
            writeln!(output).expect("writing to string cannot fail");
            dump_function_bytecode(function, depth + 1, output);
        }
    }
}

fn format_hir_instruction(instruction: &HirInstruction) -> String {
    match instruction {
        HirInstruction::MakeFunction(function) => {
            format!(
                "MakeFunction(name={})",
                function.name.as_deref().unwrap_or("<anonymous>")
            )
        }
        other => format!("{other:?}"),
    }
}

fn format_register_instruction(instruction: &RegisterInstruction) -> String {
    match instruction {
        RegisterInstruction::MakeFunction { dst, function } => {
            format!(
                "MakeFunction {{ dst: r{dst}, function: {} }}",
                function.name.as_deref().unwrap_or("<anonymous>")
            )
        }
        other => format!("{other:?}"),
    }
}
