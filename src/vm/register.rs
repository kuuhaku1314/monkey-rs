use super::hir::{FunctionIr, HirInstruction};
use super::program::{BuiltinOp, CompiledFunction};
use super::symbols::SymbolId;
use crate::lexer::Span;
use crate::token::Token;
use std::collections::HashMap;
use std::rc::Rc;

pub(super) type Register = usize;

#[derive(Clone, Debug)]
pub(super) enum RegisterConstant {
    Int(i64),
    Float(f64),
    Bool(bool),
    Null,
    Empty,
}

#[derive(Clone, Debug)]
pub(super) enum RegisterInstruction {
    Move {
        dst: Register,
        src: Register,
    },
    ConstantInt {
        dst: Register,
        value: i64,
    },
    ConstantFloat {
        dst: Register,
        value: f64,
    },
    ConstantBool {
        dst: Register,
        value: bool,
    },
    ConstantString {
        dst: Register,
        constant: usize,
    },
    ConstantNull {
        dst: Register,
    },
    ConstantEmpty {
        dst: Register,
    },
    MakeFunction {
        dst: Register,
        function: Rc<CompiledFunction>,
    },
    GetName {
        dst: Register,
        name: SymbolId,
        span: Span,
    },
    GetLocal {
        dst: Register,
        slot: usize,
        span: Span,
    },
    GetSlot {
        dst: Register,
        depth: usize,
        slot: usize,
        span: Span,
    },
    GetUpvalue {
        dst: Register,
        index: usize,
        span: Span,
    },
    DefineLocal {
        src: Register,
        name: SymbolId,
        slot: usize,
        span: Span,
    },
    DefineLocalConst {
        name: SymbolId,
        slot: usize,
        value: RegisterConstant,
        span: Span,
    },
    SetName {
        dst: Option<Register>,
        src: Register,
        name: SymbolId,
        span: Span,
    },
    SetLocal {
        dst: Option<Register>,
        src: Register,
        slot: usize,
        span: Span,
    },
    SetLocalConst {
        dst: Option<Register>,
        slot: usize,
        value: RegisterConstant,
        span: Span,
    },
    SetSlot {
        dst: Option<Register>,
        src: Register,
        depth: usize,
        slot: usize,
        span: Span,
    },
    SetUpvalue {
        dst: Option<Register>,
        src: Register,
        index: usize,
        span: Span,
    },
    UpdateNameInfix {
        dst: Option<Register>,
        name: SymbolId,
        right: Register,
        token: Token,
        span: Span,
    },
    UpdateLocalInfix {
        dst: Option<Register>,
        slot: usize,
        right: Register,
        token: Token,
        span: Span,
    },
    UpdateLocalInt {
        dst: Option<Register>,
        slot: usize,
        token: Token,
        operand: i64,
        span: Span,
    },
    UpdateLocalFromLocal {
        dst: Option<Register>,
        slot: usize,
        token: Token,
        operand_slot: usize,
        span: Span,
    },
    UpdateSlotInfix {
        dst: Option<Register>,
        depth: usize,
        slot: usize,
        right: Register,
        token: Token,
        span: Span,
    },
    UpdateUpvalueInfix {
        dst: Option<Register>,
        index: usize,
        right: Register,
        token: Token,
        span: Span,
    },
    UpdateUpvalueInt {
        dst: Option<Register>,
        index: usize,
        token: Token,
        operand: i64,
        span: Span,
    },
    UpdateUpvalueFromUpvalue {
        dst: Option<Register>,
        index: usize,
        token: Token,
        operand_index: usize,
        span: Span,
    },
    Pop {
        src: Option<Register>,
    },
    AssertBool {
        dst: Register,
        src: Register,
        span: Span,
    },
    Prefix {
        dst: Register,
        src: Register,
        token: Token,
        span: Span,
    },
    Infix {
        dst: Register,
        left: Register,
        right: Register,
        token: Token,
        span: Span,
    },
    InfixLocalInt {
        dst: Register,
        slot: usize,
        token: Token,
        operand: i64,
        span: Span,
    },
    InfixLocalLocal {
        dst: Register,
        left_slot: usize,
        token: Token,
        right_slot: usize,
        span: Span,
    },
    CompareLocalInt {
        dst: Register,
        slot: usize,
        token: Token,
        operand: i64,
        span: Span,
    },
    MakeSlice {
        dst: Register,
        elements: Vec<Register>,
    },
    MakeMap {
        dst: Register,
        entries: Vec<(Register, Register)>,
        span: Span,
    },
    Index {
        dst: Register,
        left: Register,
        index: Register,
        span: Span,
    },
    Member {
        dst: Register,
        left: Register,
        property: SymbolId,
        span: Span,
    },
    SetIndex {
        dst: Option<Register>,
        left: Register,
        index: Register,
        value: Register,
        span: Span,
    },
    SetMember {
        dst: Option<Register>,
        left: Register,
        value: Register,
        property: SymbolId,
        span: Span,
    },
    CallBuiltin {
        dst: Option<Register>,
        op: BuiltinOp,
        args: Vec<Register>,
        span: Span,
    },
    CallBuiltin0 {
        dst: Option<Register>,
        op: BuiltinOp,
        span: Span,
    },
    CallBuiltin1 {
        dst: Option<Register>,
        op: BuiltinOp,
        arg0: Register,
        span: Span,
    },
    CallBuiltin2 {
        dst: Option<Register>,
        op: BuiltinOp,
        arg0: Register,
        arg1: Register,
        span: Span,
    },
    CallBuiltin3 {
        dst: Option<Register>,
        op: BuiltinOp,
        arg0: Register,
        arg1: Register,
        arg2: Register,
        span: Span,
    },
    AppendLocalNoResult {
        list_slot: usize,
        value_slot: usize,
        span: Span,
    },
    DefineStruct {
        name: SymbolId,
        slot: usize,
        fields: Vec<String>,
        span: Span,
    },
    MakeStructNamed {
        dst: Register,
        struct_type: Register,
        fields: Vec<SymbolId>,
        values: Vec<Register>,
        span: Span,
    },
    MakeStructPositional {
        dst: Register,
        struct_type: Register,
        values: Vec<Register>,
        span: Span,
    },
    Import {
        alias: Option<SymbolId>,
        path: String,
        span: Span,
    },
    Export {
        name: SymbolId,
        span: Span,
    },
    Jump {
        target: usize,
    },
    ScopeCleanupJump {
        target: usize,
        depth: usize,
        span: Span,
    },
    JumpIfFalse {
        condition: Register,
        target: usize,
    },
    JumpIfLocalFalse {
        slot: usize,
        target: usize,
        span: Span,
    },
    JumpIfLocalIntFalse {
        slot: usize,
        token: Token,
        operand: i64,
        target: usize,
        span: Span,
    },
    JumpIfLocalLocalFalse {
        left_slot: usize,
        token: Token,
        right_slot: usize,
        target: usize,
        span: Span,
    },
    ForInStart {
        collection: Register,
        span: Span,
    },
    ForInNext {
        key: SymbolId,
        key_slot: usize,
        value: Option<(SymbolId, usize)>,
        local_count: usize,
        end_target: usize,
        span: Span,
    },
    ForInContinue {
        target: usize,
        span: Span,
    },
    ForInBreak {
        target: usize,
        span: Span,
    },
    EnterScope {
        local_count: usize,
    },
    ExitScope,
    Call {
        dst: Register,
        function: Register,
        args: Vec<Register>,
        span: Span,
        frame_name: Option<String>,
    },
    Return {
        src: Register,
    },
    Break {
        span: Span,
    },
    Continue {
        span: Span,
    },
}

impl RegisterInstruction {
    pub(super) fn kind(&self) -> &'static str {
        match self {
            Self::Move { .. } => "Move",
            Self::ConstantInt { .. } => "ConstantInt",
            Self::ConstantFloat { .. } => "ConstantFloat",
            Self::ConstantBool { .. } => "ConstantBool",
            Self::ConstantString { .. } => "ConstantString",
            Self::ConstantNull { .. } => "ConstantNull",
            Self::ConstantEmpty { .. } => "ConstantEmpty",
            Self::MakeFunction { .. } => "MakeFunction",
            Self::GetName { .. } => "GetName",
            Self::GetLocal { .. } => "GetLocal",
            Self::GetSlot { .. } => "GetSlot",
            Self::GetUpvalue { .. } => "GetUpvalue",
            Self::DefineLocal { .. } => "DefineLocal",
            Self::DefineLocalConst { .. } => "DefineLocalConst",
            Self::SetName { .. } => "SetName",
            Self::SetLocal { .. } => "SetLocal",
            Self::SetLocalConst { .. } => "SetLocalConst",
            Self::SetSlot { .. } => "SetSlot",
            Self::SetUpvalue { .. } => "SetUpvalue",
            Self::UpdateNameInfix { .. } => "UpdateNameInfix",
            Self::UpdateLocalInfix { .. } => "UpdateLocalInfix",
            Self::UpdateLocalInt { .. } => "UpdateLocalInt",
            Self::UpdateLocalFromLocal { .. } => "UpdateLocalFromLocal",
            Self::UpdateSlotInfix { .. } => "UpdateSlotInfix",
            Self::UpdateUpvalueInfix { .. } => "UpdateUpvalueInfix",
            Self::UpdateUpvalueInt { .. } => "UpdateUpvalueInt",
            Self::UpdateUpvalueFromUpvalue { .. } => "UpdateUpvalueFromUpvalue",
            Self::Pop { .. } => "Pop",
            Self::AssertBool { .. } => "AssertBool",
            Self::Prefix { .. } => "Prefix",
            Self::Infix { .. } => "Infix",
            Self::InfixLocalInt { .. } => "InfixLocalInt",
            Self::InfixLocalLocal { .. } => "InfixLocalLocal",
            Self::CompareLocalInt { .. } => "CompareLocalInt",
            Self::MakeSlice { .. } => "MakeSlice",
            Self::MakeMap { .. } => "MakeMap",
            Self::Index { .. } => "Index",
            Self::Member { .. } => "Member",
            Self::SetIndex { .. } => "SetIndex",
            Self::SetMember { .. } => "SetMember",
            Self::CallBuiltin { .. }
            | Self::CallBuiltin0 { .. }
            | Self::CallBuiltin1 { .. }
            | Self::CallBuiltin2 { .. }
            | Self::CallBuiltin3 { .. } => "CallBuiltin",
            Self::AppendLocalNoResult { .. } => "AppendLocalNoResult",
            Self::DefineStruct { .. } => "DefineStruct",
            Self::MakeStructNamed { .. } => "MakeStructNamed",
            Self::MakeStructPositional { .. } => "MakeStructPositional",
            Self::Import { .. } => "Import",
            Self::Export { .. } => "Export",
            Self::Jump { .. } => "Jump",
            Self::ScopeCleanupJump { .. } => "ScopeCleanupJump",
            Self::JumpIfFalse { .. } => "JumpIfFalse",
            Self::JumpIfLocalFalse { .. } => "JumpIfLocalFalse",
            Self::JumpIfLocalIntFalse { .. } => "JumpIfLocalIntFalse",
            Self::JumpIfLocalLocalFalse { .. } => "JumpIfLocalLocalFalse",
            Self::ForInStart { .. } => "ForInStart",
            Self::ForInNext { .. } => "ForInNext",
            Self::ForInContinue { .. } => "ForInContinue",
            Self::ForInBreak { .. } => "ForInBreak",
            Self::EnterScope { .. } => "EnterScope",
            Self::ExitScope => "ExitScope",
            Self::Call { .. } => "Call",
            Self::Return { .. } => "Return",
            Self::Break { .. } => "Break",
            Self::Continue { .. } => "Continue",
        }
    }
}

#[derive(Clone, Debug)]
pub(super) struct RegisterFunction {
    pub(super) instructions: Vec<RegisterInstruction>,
    pub(super) register_count: usize,
    pub(super) sync_env_on_exit: bool,
}

pub(super) fn compile_ir(ir: &FunctionIr) -> RegisterFunction {
    let mut emitter = RegisterEmitter {
        instructions: Vec::with_capacity(ir.instructions.len()),
        stack: Vec::new(),
        target_stacks: HashMap::new(),
        source_to_register: Vec::with_capacity(ir.instructions.len() + 1),
        next_register: 0,
        max_register: 0,
        free_registers: Vec::new(),
    };
    for (index, instruction) in ir.instructions.iter().enumerate() {
        emitter.reconcile_fallthrough(index);
        emitter.source_to_register.push(emitter.instructions.len());
        emitter.emit_instruction(index, instruction);
    }
    emitter.source_to_register.push(emitter.instructions.len());
    emitter.patch_targets();
    let instructions = optimize_register_instructions(emitter.instructions);
    let sync_env_on_exit = register_function_sync_env_on_exit(&instructions);
    RegisterFunction {
        instructions,
        register_count: emitter.max_register,
        sync_env_on_exit,
    }
}

fn register_function_sync_env_on_exit(instructions: &[RegisterInstruction]) -> bool {
    instructions
        .iter()
        .any(|instruction| matches!(instruction, RegisterInstruction::Export { .. }))
}

fn optimize_register_instructions(
    mut instructions: Vec<RegisterInstruction>,
) -> Vec<RegisterInstruction> {
    let mut remove = vec![false; instructions.len()];
    for index in 0..instructions.len().saturating_sub(1) {
        let Some((constant_register, constant_value)) =
            register_constant_from_instruction(&instructions[index])
        else {
            continue;
        };
        let replacement = match &instructions[index + 1] {
            RegisterInstruction::DefineLocal {
                src,
                name,
                slot,
                span,
            } if *src == constant_register => Some(RegisterInstruction::DefineLocalConst {
                name: *name,
                slot: *slot,
                value: constant_value.clone(),
                span: *span,
            }),
            RegisterInstruction::SetLocal {
                dst,
                src,
                slot,
                span,
            } if *src == constant_register => Some(RegisterInstruction::SetLocalConst {
                dst: *dst,
                slot: *slot,
                value: constant_value.clone(),
                span: *span,
            }),
            RegisterInstruction::UpdateUpvalueInfix {
                dst,
                index,
                right,
                token,
                span,
            } if *right == constant_register => match constant_value {
                RegisterConstant::Int(operand) => Some(RegisterInstruction::UpdateUpvalueInt {
                    dst: *dst,
                    index: *index,
                    token: token.clone(),
                    operand,
                    span: *span,
                }),
                _ => None,
            },
            _ => None,
        };
        if let Some(replacement) = replacement {
            remove[index] = true;
            instructions[index + 1] = replacement;
        }
    }
    for index in 0..instructions.len().saturating_sub(1) {
        let RegisterInstruction::GetUpvalue {
            dst: operand_register,
            index: operand_index,
            ..
        } = &instructions[index]
        else {
            continue;
        };
        let replacement = match &instructions[index + 1] {
            RegisterInstruction::UpdateUpvalueInfix {
                dst,
                index,
                right,
                token,
                span,
            } if right == operand_register => Some(RegisterInstruction::UpdateUpvalueFromUpvalue {
                dst: *dst,
                index: *index,
                token: token.clone(),
                operand_index: *operand_index,
                span: *span,
            }),
            _ => None,
        };
        if let Some(replacement) = replacement {
            remove[index] = true;
            instructions[index + 1] = replacement;
        }
    }
    for (index, instruction) in instructions.iter().enumerate() {
        match instruction {
            RegisterInstruction::Move { dst, src } if dst == src => {
                remove[index] = true;
            }
            RegisterInstruction::Jump { target } if *target == index + 1 => {
                remove[index] = true;
            }
            _ => {}
        }
    }
    if !remove.iter().any(|should_remove| *should_remove) {
        return instructions;
    }

    let mut old_to_new = vec![0; instructions.len() + 1];
    let mut next = 0;
    for index in 0..instructions.len() {
        old_to_new[index] = next;
        if !remove[index] {
            next += 1;
        }
    }
    old_to_new[instructions.len()] = next;

    let mut optimized = Vec::with_capacity(next);
    for (index, instruction) in instructions.into_iter().enumerate() {
        if !remove[index] {
            optimized.push(instruction);
        }
    }
    for instruction in &mut optimized {
        patch_register_target(instruction, &old_to_new);
    }
    optimized
}

fn register_constant_from_instruction(
    instruction: &RegisterInstruction,
) -> Option<(Register, RegisterConstant)> {
    match instruction {
        RegisterInstruction::ConstantInt { dst, value } => {
            Some((*dst, RegisterConstant::Int(*value)))
        }
        RegisterInstruction::ConstantFloat { dst, value } => {
            Some((*dst, RegisterConstant::Float(*value)))
        }
        RegisterInstruction::ConstantBool { dst, value } => {
            Some((*dst, RegisterConstant::Bool(*value)))
        }
        RegisterInstruction::ConstantNull { dst } => Some((*dst, RegisterConstant::Null)),
        RegisterInstruction::ConstantEmpty { dst } => Some((*dst, RegisterConstant::Empty)),
        _ => None,
    }
}

fn patch_register_target(instruction: &mut RegisterInstruction, old_to_new: &[usize]) {
    let map = |target: &mut usize| {
        *target = old_to_new
            .get(*target)
            .copied()
            .unwrap_or(old_to_new.len() - 1);
    };
    match instruction {
        RegisterInstruction::Jump { target }
        | RegisterInstruction::ScopeCleanupJump { target, .. }
        | RegisterInstruction::JumpIfFalse { target, .. }
        | RegisterInstruction::JumpIfLocalFalse { target, .. }
        | RegisterInstruction::JumpIfLocalIntFalse { target, .. }
        | RegisterInstruction::JumpIfLocalLocalFalse { target, .. }
        | RegisterInstruction::ForInContinue { target, .. }
        | RegisterInstruction::ForInBreak { target, .. } => map(target),
        RegisterInstruction::ForInNext { end_target, .. } => map(end_target),
        _ => {}
    }
}

struct RegisterEmitter {
    instructions: Vec<RegisterInstruction>,
    stack: Vec<Register>,
    target_stacks: HashMap<usize, Vec<Register>>,
    source_to_register: Vec<usize>,
    next_register: usize,
    max_register: usize,
    free_registers: Vec<Register>,
}

impl RegisterEmitter {
    fn alloc(&mut self) -> Register {
        if let Some(register) = self.free_registers.pop() {
            return register;
        }
        let register = self.next_register;
        self.next_register += 1;
        self.max_register = self.max_register.max(self.next_register);
        register
    }

    fn release(&mut self, register: Register) {
        if self.stack.contains(&register)
            || self
                .target_stacks
                .values()
                .any(|stack| stack.contains(&register))
            || self.free_registers.contains(&register)
        {
            return;
        }
        self.free_registers.push(register);
    }

    fn release_many(&mut self, registers: impl IntoIterator<Item = Register>) {
        for register in registers {
            self.release(register);
        }
    }

    fn push(&mut self, register: Register) {
        self.stack.push(register);
    }

    fn pop(&mut self) -> Register {
        self.stack
            .pop()
            .expect("compiled stack must have a register")
    }

    fn pop_many(&mut self, count: usize) -> Vec<Register> {
        let start = self.stack.len() - count;
        self.stack.split_off(start)
    }

    fn reconcile_fallthrough(&mut self, target: usize) {
        let Some(target_stack) = self.target_stacks.remove(&target) else {
            return;
        };
        if self.stack.is_empty() {
            self.stack = target_stack;
            return;
        }
        self.move_stack_to(&target_stack);
        self.stack = target_stack;
    }

    fn record_target_stack(&mut self, target: usize, stack: Vec<Register>) {
        self.target_stacks.entry(target).or_insert(stack);
    }

    fn reconcile_jump_target(&mut self, current: usize, target: usize) {
        if target <= current {
            return;
        }
        if let Some(target_stack) = self.target_stacks.get(&target).cloned() {
            self.move_stack_to(&target_stack);
            self.stack = target_stack;
        } else {
            self.record_target_stack(target, self.stack.clone());
        }
    }

    fn move_stack_to(&mut self, target_stack: &[Register]) {
        if self.stack.len() != target_stack.len() {
            return;
        }
        for (src, dst) in self.stack.iter().copied().zip(target_stack.iter().copied()) {
            if src != dst {
                self.instructions
                    .push(RegisterInstruction::Move { dst, src });
            }
        }
    }

    fn patch_targets(&mut self) {
        let source_to_register = self.source_to_register.clone();
        let end = self.instructions.len();
        let mapped = |target: usize| source_to_register.get(target).copied().unwrap_or(end);
        for instruction in &mut self.instructions {
            match instruction {
                RegisterInstruction::Jump { target }
                | RegisterInstruction::ScopeCleanupJump { target, .. }
                | RegisterInstruction::JumpIfFalse { target, .. }
                | RegisterInstruction::JumpIfLocalFalse { target, .. }
                | RegisterInstruction::JumpIfLocalIntFalse { target, .. }
                | RegisterInstruction::JumpIfLocalLocalFalse { target, .. }
                | RegisterInstruction::ForInContinue { target, .. }
                | RegisterInstruction::ForInBreak { target, .. } => {
                    *target = mapped(*target);
                }
                RegisterInstruction::ForInNext { end_target, .. } => {
                    *end_target = mapped(*end_target);
                }
                _ => {}
            }
        }
    }

    fn emit_instruction(&mut self, index: usize, instruction: &HirInstruction) {
        match instruction {
            HirInstruction::ConstantInt(value) => {
                let dst = self.alloc();
                self.instructions
                    .push(RegisterInstruction::ConstantInt { dst, value: *value });
                self.push(dst);
            }
            HirInstruction::ConstantFloat(value) => {
                let dst = self.alloc();
                self.instructions
                    .push(RegisterInstruction::ConstantFloat { dst, value: *value });
                self.push(dst);
            }
            HirInstruction::ConstantBool(value) => {
                let dst = self.alloc();
                self.instructions
                    .push(RegisterInstruction::ConstantBool { dst, value: *value });
                self.push(dst);
            }
            HirInstruction::ConstantString(constant) => {
                let dst = self.alloc();
                self.instructions.push(RegisterInstruction::ConstantString {
                    dst,
                    constant: *constant,
                });
                self.push(dst);
            }
            HirInstruction::ConstantNull => {
                let dst = self.alloc();
                self.instructions
                    .push(RegisterInstruction::ConstantNull { dst });
                self.push(dst);
            }
            HirInstruction::ConstantEmpty => {
                let dst = self.alloc();
                self.instructions
                    .push(RegisterInstruction::ConstantEmpty { dst });
                self.push(dst);
            }
            HirInstruction::MakeFunction(function) => {
                let dst = self.alloc();
                self.instructions.push(RegisterInstruction::MakeFunction {
                    dst,
                    function: Rc::clone(function),
                });
                self.push(dst);
            }
            HirInstruction::GetName(name, span) => {
                let dst = self.alloc();
                self.instructions.push(RegisterInstruction::GetName {
                    dst,
                    name: *name,
                    span: *span,
                });
                self.push(dst);
            }
            HirInstruction::GetLocal { slot, span } => {
                let dst = self.alloc();
                self.instructions.push(RegisterInstruction::GetLocal {
                    dst,
                    slot: *slot,
                    span: *span,
                });
                self.push(dst);
            }
            HirInstruction::GetSlot { depth, slot, span } => {
                let dst = self.alloc();
                self.instructions.push(RegisterInstruction::GetSlot {
                    dst,
                    depth: *depth,
                    slot: *slot,
                    span: *span,
                });
                self.push(dst);
            }
            HirInstruction::GetUpvalue { index, span } => {
                let dst = self.alloc();
                self.instructions.push(RegisterInstruction::GetUpvalue {
                    dst,
                    index: *index,
                    span: *span,
                });
                self.push(dst);
            }
            HirInstruction::DefineLocal { name, slot, span } => {
                let src = self.pop();
                self.instructions.push(RegisterInstruction::DefineLocal {
                    src,
                    name: *name,
                    slot: *slot,
                    span: *span,
                });
                self.release(src);
            }
            HirInstruction::SetName(name, span) => {
                let dst = self.alloc();
                self.lower_set_name(Some(dst), *name, *span)
            }
            HirInstruction::SetNameNoResult(name, span) => self.lower_set_name(None, *name, *span),
            HirInstruction::SetLocal { slot, span } => {
                let dst = self.alloc();
                self.lower_set_local(Some(dst), *slot, *span)
            }
            HirInstruction::SetLocalNoResult { slot, span } => {
                self.lower_set_local(None, *slot, *span)
            }
            HirInstruction::SetSlot { depth, slot, span } => {
                let dst = self.alloc();
                self.lower_set_slot(Some(dst), *depth, *slot, *span)
            }
            HirInstruction::SetSlotNoResult { depth, slot, span } => {
                self.lower_set_slot(None, *depth, *slot, *span)
            }
            HirInstruction::SetUpvalue { index, span } => {
                let dst = self.alloc();
                self.lower_set_upvalue(Some(dst), *index, *span)
            }
            HirInstruction::SetUpvalueNoResult { index, span } => {
                self.lower_set_upvalue(None, *index, *span)
            }
            HirInstruction::UpdateNameInfix(name, token, span) => {
                let dst = self.alloc();
                self.lower_update_name(Some(dst), *name, token.clone(), *span)
            }
            HirInstruction::UpdateNameInfixNoResult(name, token, span) => {
                self.lower_update_name(None, *name, token.clone(), *span)
            }
            HirInstruction::UpdateLocalInfix { slot, token, span } => {
                let dst = self.alloc();
                self.lower_update_local(Some(dst), *slot, token.clone(), *span)
            }
            HirInstruction::UpdateLocalInfixNoResult { slot, token, span } => {
                self.lower_update_local(None, *slot, token.clone(), *span)
            }
            HirInstruction::UpdateLocalInt {
                slot,
                token,
                operand,
                span,
            } => {
                let dst = self.alloc();
                self.lower_update_local_int(Some(dst), *slot, token.clone(), *operand, *span)
            }
            HirInstruction::UpdateLocalIntNoResult {
                slot,
                token,
                operand,
                span,
            } => self.lower_update_local_int(None, *slot, token.clone(), *operand, *span),
            HirInstruction::UpdateLocalFromLocal {
                slot,
                token,
                operand_slot,
                span,
            } => {
                let dst = self.alloc();
                self.lower_update_local_from_local(
                    Some(dst),
                    *slot,
                    token.clone(),
                    *operand_slot,
                    *span,
                )
            }
            HirInstruction::UpdateLocalFromLocalNoResult {
                slot,
                token,
                operand_slot,
                span,
            } => {
                self.lower_update_local_from_local(None, *slot, token.clone(), *operand_slot, *span)
            }
            HirInstruction::UpdateSlotInfix {
                depth,
                slot,
                token,
                span,
            } => {
                let dst = self.alloc();
                self.lower_update_slot(Some(dst), *depth, *slot, token.clone(), *span)
            }
            HirInstruction::UpdateSlotInfixNoResult {
                depth,
                slot,
                token,
                span,
            } => self.lower_update_slot(None, *depth, *slot, token.clone(), *span),
            HirInstruction::UpdateUpvalueInfix { index, token, span } => {
                let dst = self.alloc();
                self.lower_update_upvalue(Some(dst), *index, token.clone(), *span)
            }
            HirInstruction::UpdateUpvalueInfixNoResult { index, token, span } => {
                self.lower_update_upvalue(None, *index, token.clone(), *span)
            }
            HirInstruction::Pop => {
                let src = self.stack.pop();
                self.instructions.push(RegisterInstruction::Pop { src });
                if let Some(src) = src {
                    self.release(src);
                }
            }
            HirInstruction::AssertBool(span) => {
                let src = self.pop();
                let dst = self.alloc();
                self.instructions.push(RegisterInstruction::AssertBool {
                    dst,
                    src,
                    span: *span,
                });
                self.release(src);
                self.push(dst);
            }
            HirInstruction::Prefix(token, span) => {
                let src = self.pop();
                let dst = self.alloc();
                self.instructions.push(RegisterInstruction::Prefix {
                    dst,
                    src,
                    token: token.clone(),
                    span: *span,
                });
                self.release(src);
                self.push(dst);
            }
            HirInstruction::Infix(token, span) => {
                let right = self.pop();
                let left = self.pop();
                let dst = self.alloc();
                self.instructions.push(RegisterInstruction::Infix {
                    dst,
                    left,
                    right,
                    token: token.clone(),
                    span: *span,
                });
                self.release(left);
                self.release(right);
                self.push(dst);
            }
            HirInstruction::InfixLocalInt {
                slot,
                token,
                operand,
                span,
            } => {
                let dst = self.alloc();
                self.instructions.push(RegisterInstruction::InfixLocalInt {
                    dst,
                    slot: *slot,
                    token: token.clone(),
                    operand: *operand,
                    span: *span,
                });
                self.push(dst);
            }
            HirInstruction::InfixLocalLocal {
                left_slot,
                token,
                right_slot,
                span,
            } => {
                let dst = self.alloc();
                self.instructions
                    .push(RegisterInstruction::InfixLocalLocal {
                        dst,
                        left_slot: *left_slot,
                        token: token.clone(),
                        right_slot: *right_slot,
                        span: *span,
                    });
                self.push(dst);
            }
            HirInstruction::CompareLocalInt {
                slot,
                token,
                operand,
                span,
            } => {
                let dst = self.alloc();
                self.instructions
                    .push(RegisterInstruction::CompareLocalInt {
                        dst,
                        slot: *slot,
                        token: token.clone(),
                        operand: *operand,
                        span: *span,
                    });
                self.push(dst);
            }
            HirInstruction::MakeSlice(count) => {
                let elements = self.pop_many(*count);
                let dst = self.alloc();
                self.release_many(elements.iter().copied());
                self.instructions
                    .push(RegisterInstruction::MakeSlice { dst, elements });
                self.push(dst);
            }
            HirInstruction::MakeMap(count, span) => {
                let values = self.pop_many(*count * 2);
                let mut entries = Vec::with_capacity(*count);
                for pair in values.chunks_exact(2) {
                    entries.push((pair[0], pair[1]));
                }
                let dst = self.alloc();
                self.release_many(values.iter().copied());
                self.instructions.push(RegisterInstruction::MakeMap {
                    dst,
                    entries,
                    span: *span,
                });
                self.push(dst);
            }
            HirInstruction::Index(span) => {
                let index = self.pop();
                let left = self.pop();
                let dst = self.alloc();
                self.instructions.push(RegisterInstruction::Index {
                    dst,
                    left,
                    index,
                    span: *span,
                });
                self.release(left);
                self.release(index);
                self.push(dst);
            }
            HirInstruction::Member(property, span) => {
                let left = self.pop();
                let dst = self.alloc();
                self.instructions.push(RegisterInstruction::Member {
                    dst,
                    left,
                    property: *property,
                    span: *span,
                });
                self.release(left);
                self.push(dst);
            }
            HirInstruction::SetIndex(span) => {
                let dst = self.alloc();
                self.lower_set_index(Some(dst), *span)
            }
            HirInstruction::SetIndexNoResult(span) => self.lower_set_index(None, *span),
            HirInstruction::SetMember(property, span) => {
                let dst = self.alloc();
                self.lower_set_member(Some(dst), *property, *span)
            }
            HirInstruction::SetMemberNoResult(property, span) => {
                self.lower_set_member(None, *property, *span)
            }
            HirInstruction::CallBuiltin {
                op,
                arg_count,
                span,
            } => {
                let dst = self.alloc();
                self.lower_call_builtin(Some(dst), *op, *arg_count, *span)
            }
            HirInstruction::CallBuiltinNoResult {
                op,
                arg_count,
                span,
            } => self.lower_call_builtin(None, *op, *arg_count, *span),
            HirInstruction::AppendLocalNoResult {
                list_slot,
                value_slot,
                span,
            } => {
                self.instructions
                    .push(RegisterInstruction::AppendLocalNoResult {
                        list_slot: *list_slot,
                        value_slot: *value_slot,
                        span: *span,
                    });
            }
            HirInstruction::DefineStruct {
                name,
                slot,
                fields,
                span,
            } => {
                self.instructions.push(RegisterInstruction::DefineStruct {
                    name: *name,
                    slot: *slot,
                    fields: fields.clone(),
                    span: *span,
                });
            }
            HirInstruction::MakeStructNamed { fields, span } => {
                let values = self.pop_many(fields.len());
                let struct_type = self.pop();
                let dst = self.alloc();
                self.release(struct_type);
                self.release_many(values.iter().copied());
                self.instructions
                    .push(RegisterInstruction::MakeStructNamed {
                        dst,
                        struct_type,
                        fields: fields.clone(),
                        values,
                        span: *span,
                    });
                self.push(dst);
            }
            HirInstruction::MakeStructPositional { count, span } => {
                let values = self.pop_many(*count);
                let struct_type = self.pop();
                let dst = self.alloc();
                self.release(struct_type);
                self.release_many(values.iter().copied());
                self.instructions
                    .push(RegisterInstruction::MakeStructPositional {
                        dst,
                        struct_type,
                        values,
                        span: *span,
                    });
                self.push(dst);
            }
            HirInstruction::Import { alias, path, span } => {
                self.instructions.push(RegisterInstruction::Import {
                    alias: *alias,
                    path: path.clone(),
                    span: *span,
                });
            }
            HirInstruction::Export { name, span } => {
                self.instructions.push(RegisterInstruction::Export {
                    name: *name,
                    span: *span,
                })
            }
            HirInstruction::Jump(target) => {
                self.reconcile_jump_target(index, *target);
                self.instructions
                    .push(RegisterInstruction::Jump { target: *target });
                self.stack.clear();
            }
            HirInstruction::ScopeCleanupJump {
                target,
                depth,
                span,
            } => {
                self.reconcile_jump_target(index, *target);
                self.instructions
                    .push(RegisterInstruction::ScopeCleanupJump {
                        target: *target,
                        depth: *depth,
                        span: *span,
                    });
                self.stack.clear();
            }
            HirInstruction::JumpIfFalse(target) => {
                let condition = self.pop();
                self.record_target_stack(*target, self.stack.clone());
                self.instructions.push(RegisterInstruction::JumpIfFalse {
                    condition,
                    target: *target,
                });
                self.release(condition);
            }
            HirInstruction::JumpIfLocalFalse { slot, target, span } => {
                self.record_target_stack(*target, self.stack.clone());
                self.instructions
                    .push(RegisterInstruction::JumpIfLocalFalse {
                        slot: *slot,
                        target: *target,
                        span: *span,
                    });
            }
            HirInstruction::JumpIfLocalIntFalse {
                slot,
                token,
                operand,
                target,
                span,
            } => {
                self.record_target_stack(*target, self.stack.clone());
                self.instructions
                    .push(RegisterInstruction::JumpIfLocalIntFalse {
                        slot: *slot,
                        token: token.clone(),
                        operand: *operand,
                        target: *target,
                        span: *span,
                    });
            }
            HirInstruction::JumpIfLocalLocalFalse {
                left_slot,
                token,
                right_slot,
                target,
                span,
            } => {
                self.record_target_stack(*target, self.stack.clone());
                self.instructions
                    .push(RegisterInstruction::JumpIfLocalLocalFalse {
                        left_slot: *left_slot,
                        token: token.clone(),
                        right_slot: *right_slot,
                        target: *target,
                        span: *span,
                    });
            }
            HirInstruction::ForInStart { span } => {
                let collection = self.pop();
                self.instructions.push(RegisterInstruction::ForInStart {
                    collection,
                    span: *span,
                });
                self.release(collection);
            }
            HirInstruction::ForInNext {
                key,
                key_slot,
                value,
                local_count,
                end_target,
                span,
            } => {
                self.instructions.push(RegisterInstruction::ForInNext {
                    key: *key,
                    key_slot: *key_slot,
                    value: *value,
                    local_count: *local_count,
                    end_target: *end_target,
                    span: *span,
                });
            }
            HirInstruction::ForInContinue { target, span } => {
                self.instructions.push(RegisterInstruction::ForInContinue {
                    target: *target,
                    span: *span,
                });
            }
            HirInstruction::ForInBreak { target, span } => {
                self.instructions.push(RegisterInstruction::ForInBreak {
                    target: *target,
                    span: *span,
                });
            }
            HirInstruction::EnterScope(local_count) => {
                self.instructions.push(RegisterInstruction::EnterScope {
                    local_count: *local_count,
                })
            }
            HirInstruction::ExitScope => self.instructions.push(RegisterInstruction::ExitScope),
            HirInstruction::Call {
                arg_count,
                span,
                frame_name,
            } => {
                let args = self.pop_many(*arg_count);
                let function = self.pop();
                let dst = self.alloc();
                self.release(function);
                self.release_many(args.iter().copied());
                self.instructions.push(RegisterInstruction::Call {
                    dst,
                    function,
                    args,
                    span: *span,
                    frame_name: frame_name.clone(),
                });
                self.push(dst);
            }
            HirInstruction::Return => {
                let src = self.pop();
                self.instructions.push(RegisterInstruction::Return { src });
                self.release(src);
            }
            HirInstruction::Break(span) => self
                .instructions
                .push(RegisterInstruction::Break { span: *span }),
            HirInstruction::Continue(span) => {
                self.instructions
                    .push(RegisterInstruction::Continue { span: *span });
            }
        }
    }

    fn lower_set_name(&mut self, dst: Option<Register>, name: SymbolId, span: Span) {
        let src = self.pop();
        self.instructions.push(RegisterInstruction::SetName {
            dst,
            src,
            name,
            span,
        });
        self.release(src);
        if let Some(dst) = dst {
            self.push(dst);
        }
    }

    fn lower_set_local(&mut self, dst: Option<Register>, slot: usize, span: Span) {
        let src = self.pop();
        self.instructions.push(RegisterInstruction::SetLocal {
            dst,
            src,
            slot,
            span,
        });
        self.release(src);
        if let Some(dst) = dst {
            self.push(dst);
        }
    }

    fn lower_set_slot(&mut self, dst: Option<Register>, depth: usize, slot: usize, span: Span) {
        let src = self.pop();
        self.instructions.push(RegisterInstruction::SetSlot {
            dst,
            src,
            depth,
            slot,
            span,
        });
        self.release(src);
        if let Some(dst) = dst {
            self.push(dst);
        }
    }

    fn lower_set_upvalue(&mut self, dst: Option<Register>, index: usize, span: Span) {
        let src = self.pop();
        self.instructions.push(RegisterInstruction::SetUpvalue {
            dst,
            src,
            index,
            span,
        });
        self.release(src);
        if let Some(dst) = dst {
            self.push(dst);
        }
    }

    fn lower_update_name(
        &mut self,
        dst: Option<Register>,
        name: SymbolId,
        token: Token,
        span: Span,
    ) {
        let right = self.pop();
        self.instructions
            .push(RegisterInstruction::UpdateNameInfix {
                dst,
                name,
                right,
                token,
                span,
            });
        self.release(right);
        if let Some(dst) = dst {
            self.push(dst);
        }
    }

    fn lower_update_local(&mut self, dst: Option<Register>, slot: usize, token: Token, span: Span) {
        let right = self.pop();
        self.instructions
            .push(RegisterInstruction::UpdateLocalInfix {
                dst,
                slot,
                right,
                token,
                span,
            });
        self.release(right);
        if let Some(dst) = dst {
            self.push(dst);
        }
    }

    fn lower_update_local_int(
        &mut self,
        dst: Option<Register>,
        slot: usize,
        token: Token,
        operand: i64,
        span: Span,
    ) {
        self.instructions.push(RegisterInstruction::UpdateLocalInt {
            dst,
            slot,
            token,
            operand,
            span,
        });
        if let Some(dst) = dst {
            self.push(dst);
        }
    }

    fn lower_update_local_from_local(
        &mut self,
        dst: Option<Register>,
        slot: usize,
        token: Token,
        operand_slot: usize,
        span: Span,
    ) {
        self.instructions
            .push(RegisterInstruction::UpdateLocalFromLocal {
                dst,
                slot,
                token,
                operand_slot,
                span,
            });
        if let Some(dst) = dst {
            self.push(dst);
        }
    }

    fn lower_update_slot(
        &mut self,
        dst: Option<Register>,
        depth: usize,
        slot: usize,
        token: Token,
        span: Span,
    ) {
        let right = self.pop();
        self.instructions
            .push(RegisterInstruction::UpdateSlotInfix {
                dst,
                depth,
                slot,
                right,
                token,
                span,
            });
        self.release(right);
        if let Some(dst) = dst {
            self.push(dst);
        }
    }

    fn lower_update_upvalue(
        &mut self,
        dst: Option<Register>,
        index: usize,
        token: Token,
        span: Span,
    ) {
        let right = self.pop();
        self.instructions
            .push(RegisterInstruction::UpdateUpvalueInfix {
                dst,
                index,
                right,
                token,
                span,
            });
        if let Some(dst) = dst {
            self.push(dst);
        }
    }

    fn lower_set_index(&mut self, dst: Option<Register>, span: Span) {
        let value = self.pop();
        let index = self.pop();
        let left = self.pop();
        self.instructions.push(RegisterInstruction::SetIndex {
            dst,
            left,
            index,
            value,
            span,
        });
        self.release(left);
        self.release(index);
        self.release(value);
        if let Some(dst) = dst {
            self.push(dst);
        }
    }

    fn lower_set_member(&mut self, dst: Option<Register>, property: SymbolId, span: Span) {
        let value = self.pop();
        let left = self.pop();
        self.instructions.push(RegisterInstruction::SetMember {
            dst,
            left,
            value,
            property,
            span,
        });
        self.release(left);
        self.release(value);
        if let Some(dst) = dst {
            self.push(dst);
        }
    }

    fn lower_call_builtin(
        &mut self,
        dst: Option<Register>,
        op: BuiltinOp,
        arg_count: usize,
        span: Span,
    ) {
        let args = self.pop_many(arg_count);
        self.release_many(args.iter().copied());
        match args.as_slice() {
            [] => self
                .instructions
                .push(RegisterInstruction::CallBuiltin0 { dst, op, span }),
            [arg0] => self.instructions.push(RegisterInstruction::CallBuiltin1 {
                dst,
                op,
                arg0: *arg0,
                span,
            }),
            [arg0, arg1] => self.instructions.push(RegisterInstruction::CallBuiltin2 {
                dst,
                op,
                arg0: *arg0,
                arg1: *arg1,
                span,
            }),
            [arg0, arg1, arg2] => self.instructions.push(RegisterInstruction::CallBuiltin3 {
                dst,
                op,
                arg0: *arg0,
                arg1: *arg1,
                arg2: *arg2,
                span,
            }),
            _ => self.instructions.push(RegisterInstruction::CallBuiltin {
                dst,
                op,
                args,
                span,
            }),
        }
        if let Some(dst) = dst {
            self.push(dst);
        }
    }
}
