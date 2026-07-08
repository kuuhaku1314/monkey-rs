use super::program::{BuiltinOp, CompiledFunction};
use super::symbols::SymbolId;
use crate::lexer::Span;
use crate::token::Token;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub(super) struct FunctionIr {
    pub(super) instructions: Vec<HirInstruction>,
}

#[derive(Clone, Debug)]
pub(super) enum HirInstruction {
    ConstantInt(i64),
    ConstantFloat(f64),
    ConstantBool(bool),
    ConstantString(usize),
    ConstantNull,
    ConstantEmpty,
    MakeFunction(Rc<CompiledFunction>),
    GetName(SymbolId, Span),
    GetLocal {
        slot: usize,
        span: Span,
    },
    GetSlot {
        depth: usize,
        slot: usize,
        span: Span,
    },
    GetUpvalue {
        index: usize,
        span: Span,
    },
    DefineLocal {
        name: SymbolId,
        slot: usize,
        span: Span,
    },
    SetName(SymbolId, Span),
    SetNameNoResult(SymbolId, Span),
    SetLocal {
        slot: usize,
        span: Span,
    },
    SetLocalNoResult {
        slot: usize,
        span: Span,
    },
    SetSlot {
        depth: usize,
        slot: usize,
        span: Span,
    },
    SetSlotNoResult {
        depth: usize,
        slot: usize,
        span: Span,
    },
    SetUpvalue {
        index: usize,
        span: Span,
    },
    SetUpvalueNoResult {
        index: usize,
        span: Span,
    },
    UpdateNameInfix(SymbolId, Token, Span),
    UpdateNameInfixNoResult(SymbolId, Token, Span),
    UpdateLocalInfix {
        slot: usize,
        token: Token,
        span: Span,
    },
    UpdateLocalInfixNoResult {
        slot: usize,
        token: Token,
        span: Span,
    },
    UpdateLocalInt {
        slot: usize,
        token: Token,
        operand: i64,
        span: Span,
    },
    UpdateLocalIntNoResult {
        slot: usize,
        token: Token,
        operand: i64,
        span: Span,
    },
    UpdateLocalFromLocal {
        slot: usize,
        token: Token,
        operand_slot: usize,
        span: Span,
    },
    UpdateLocalFromLocalNoResult {
        slot: usize,
        token: Token,
        operand_slot: usize,
        span: Span,
    },
    UpdateSlotInfix {
        depth: usize,
        slot: usize,
        token: Token,
        span: Span,
    },
    UpdateSlotInfixNoResult {
        depth: usize,
        slot: usize,
        token: Token,
        span: Span,
    },
    UpdateUpvalueInfix {
        index: usize,
        token: Token,
        span: Span,
    },
    UpdateUpvalueInfixNoResult {
        index: usize,
        token: Token,
        span: Span,
    },
    Pop,
    AssertBool(Span),
    Prefix(Token, Span),
    Infix(Token, Span),
    InfixLocalInt {
        slot: usize,
        token: Token,
        operand: i64,
        span: Span,
    },
    InfixLocalLocal {
        left_slot: usize,
        token: Token,
        right_slot: usize,
        span: Span,
    },
    CompareLocalInt {
        slot: usize,
        token: Token,
        operand: i64,
        span: Span,
    },
    MakeSlice(usize),
    MakeMap(usize, Span),
    Index(Span),
    Member(SymbolId, Span),
    SetIndex(Span),
    SetIndexNoResult(Span),
    SetMember(SymbolId, Span),
    SetMemberNoResult(SymbolId, Span),
    CallBuiltin {
        op: BuiltinOp,
        arg_count: usize,
        span: Span,
    },
    CallBuiltinNoResult {
        op: BuiltinOp,
        arg_count: usize,
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
        fields: Vec<SymbolId>,
        span: Span,
    },
    MakeStructPositional {
        count: usize,
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
    Jump(usize),
    ScopeCleanupJump {
        target: usize,
        depth: usize,
        span: Span,
    },
    JumpIfFalse(usize),
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
    EnterScope(usize),
    ExitScope,
    Call {
        arg_count: usize,
        span: Span,
        frame_name: Option<String>,
    },
    Return,
    Break(Span),
    Continue(Span),
}
