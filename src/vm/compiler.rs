use super::hir::{FunctionIr, HirInstruction};
use super::program::{BuiltinOp, CompiledFunction};
use super::register;
use super::semantic::SemanticContext;
use super::symbols::{SymbolId, SymbolInterner};
use crate::ast::{
    AssignTarget, BlockStatement, Expression, ForInStatement, FunctionLiteral, IfExpression,
    InfixExpression, MapLiteral, PrefixExpression, Program, SliceLiteral, Statement, StructLiteral,
};
use crate::error::{Error, ErrorKind};
use crate::lexer::Span;
use crate::token::Token;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub(super) struct Compiler {
    interner: Rc<RefCell<SymbolInterner>>,
    hir: Vec<HirInstruction>,
    constants: Vec<String>,
    loop_stack: Vec<LoopContext>,
    semantic: SemanticContext,
}

struct LoopContext {
    breaks: Vec<LoopJump>,
    continues: Vec<LoopJump>,
}

struct LoopJump {
    position: usize,
    cleanup_depth: usize,
    span: Span,
}

impl Compiler {
    pub(super) fn compile_program(
        program: &Program,
        interner: Rc<RefCell<SymbolInterner>>,
    ) -> Result<Rc<CompiledFunction>, Error> {
        Self::compile_program_with_ir(program, interner).map(|(function, _)| function)
    }

    pub(super) fn compile_program_with_ir(
        program: &Program,
        interner: Rc<RefCell<SymbolInterner>>,
    ) -> Result<(Rc<CompiledFunction>, FunctionIr), Error> {
        let mut compiler = Self {
            interner,
            hir: Vec::new(),
            constants: Vec::new(),
            loop_stack: Vec::new(),
            semantic: SemanticContext::root(),
        };
        compiler.compile_statements(&program.statements, true)?;
        let local_count = compiler.semantic.local_count_at(0);
        let ir = FunctionIr {
            instructions: compiler.hir,
        };
        let registers = register::compile_ir(&ir);
        let function = CompiledFunction {
            name: None,
            parameters: Vec::new(),
            parameter_slots: Vec::new(),
            upvalues: Vec::new(),
            local_count,
            constants: compiler.constants,
            registers,
        };
        Ok((Rc::new(function), ir))
    }

    fn compile_function(&self, function: &FunctionLiteral) -> Result<Rc<CompiledFunction>, Error> {
        let function_scope_index = self.semantic.current_scope_index() + 1;
        let mut compiler = Self {
            interner: Rc::clone(&self.interner),
            hir: Vec::new(),
            constants: Vec::new(),
            loop_stack: Vec::new(),
            semantic: self.semantic.function_child(),
        };
        let mut parameter_slots = Vec::with_capacity(function.parameters.len());
        for parameter in &function.parameters {
            let symbol = compiler.intern(parameter.name.as_str());
            parameter_slots.push(compiler.declare_current(symbol, parameter.span)?);
        }
        compiler.compile_statements(&function.body.statements, true)?;
        let local_count = compiler.semantic.local_count_at(function_scope_index);
        let parameters = function
            .parameters
            .iter()
            .map(|parameter| compiler.intern(parameter.name.as_str()))
            .collect();
        let upvalues = compiler.semantic.upvalues().to_vec();
        let ir = FunctionIr {
            instructions: compiler.hir,
        };
        let registers = register::compile_ir(&ir);
        let compiled = CompiledFunction {
            name: function.name.as_ref().map(|name| name.name.to_string()),
            parameters,
            parameter_slots,
            upvalues,
            local_count,
            constants: compiler.constants,
            registers,
        };
        Ok(Rc::new(compiled))
    }

    fn intern(&self, name: &str) -> SymbolId {
        self.interner.borrow_mut().intern(name)
    }

    fn string_constant(&mut self, value: &str) -> usize {
        if let Some(index) = self.constants.iter().position(|constant| constant == value) {
            return index;
        }
        let index = self.constants.len();
        self.constants.push(value.to_string());
        index
    }

    fn declare_current(&mut self, name: SymbolId, span: Span) -> Result<usize, Error> {
        let name_text = self.interner.borrow().name(name).to_string();
        self.semantic.declare_current(name, name_text, span)
    }

    fn register_imported_builtin_module(&mut self, alias: SymbolId, path: &str) {
        let Some(exports) = stdlib_builtin_exports_for_path(path) else {
            return;
        };
        let mut members = HashMap::new();
        for export in exports {
            if let Some(op) = BuiltinOp::from_name(export) {
                members.insert(self.intern(export), op);
            }
        }
        if !members.is_empty() {
            self.semantic
                .register_imported_builtin_module(alias, members);
        }
    }

    fn register_bare_imported_builtins(&mut self, path: &str) {
        let Some(exports) = stdlib_builtin_exports_for_path(path) else {
            return;
        };
        let mut builtins = Vec::new();
        for export in exports {
            if let Some(op) = BuiltinOp::from_name(export) {
                builtins.push((self.intern(export), op));
            }
        }
        if !builtins.is_empty() {
            self.semantic.register_bare_imported_builtins(builtins);
        }
    }

    fn resolve_imported_builtin_member(
        &self,
        module: SymbolId,
        member: SymbolId,
    ) -> Option<BuiltinOp> {
        self.semantic
            .resolve_imported_builtin_member(module, member)
    }

    fn resolve_direct_builtin_call(&self, symbol: SymbolId, name: &str) -> Option<BuiltinOp> {
        self.semantic.resolve_direct_builtin_call(symbol, name)
    }

    fn resolve(&self, name: SymbolId) -> Option<(usize, usize)> {
        self.semantic.resolve(name)
    }

    fn upvalue_index(&mut self, depth: usize, slot: usize) -> usize {
        self.semantic.upvalue_index(depth, slot)
    }

    fn should_capture_depth(&self, depth: usize) -> bool {
        self.semantic.should_capture_depth(depth)
    }

    fn compile_statements(
        &mut self,
        statements: &[Statement],
        keep_last_value: bool,
    ) -> Result<(), Error> {
        let last_index = statements.len().saturating_sub(1);
        for (index, statement) in statements.iter().enumerate() {
            self.compile_statement(statement, keep_last_value && index == last_index)?;
        }
        Ok(())
    }

    fn compile_statement(&mut self, statement: &Statement, keep_value: bool) -> Result<(), Error> {
        match statement {
            Statement::Block(block) => self.compile_scoped_block(block, keep_value),
            Statement::Empty(_) => Ok(()),
            Statement::Expression(statement) => {
                if (statement.end_of_semicolon || !keep_value)
                    && self.compile_expression_no_result(&statement.expression)?
                {
                    return Ok(());
                }
                self.compile_expression(&statement.expression)?;
                if statement.end_of_semicolon || !keep_value {
                    self.emit(HirInstruction::Pop);
                }
                Ok(())
            }
            Statement::Let(statement) => {
                let symbol = self.intern(statement.name.name.as_str());
                let slot = self.declare_current(symbol, statement.name.span)?;
                self.compile_expression(&statement.value)?;
                self.emit(HirInstruction::DefineLocal {
                    name: symbol,
                    slot,
                    span: statement.name.span,
                });
                Ok(())
            }
            Statement::Return(statement) => {
                self.compile_expression(&statement.value)?;
                self.emit(HirInstruction::Return);
                Ok(())
            }
            Statement::While(statement) => self.compile_while(statement, keep_value),
            Statement::ForIn(statement) => self.compile_for_in(statement, keep_value),
            Statement::Assign(statement) => {
                let produces_value = self.compile_assignment(
                    &statement.target,
                    &statement.value,
                    statement.span,
                    keep_value,
                )?;
                if produces_value && !keep_value {
                    self.emit(HirInstruction::Pop);
                }
                Ok(())
            }
            Statement::Break(statement) => {
                if self.loop_stack.is_empty() {
                    return Err(Error::with_kind_span(
                        ErrorKind::Runtime,
                        "break outside loop".to_string(),
                        statement.span,
                    ));
                }
                let pos = self.emit(HirInstruction::Break(statement.span));
                let cleanup_depth = self.semantic.cleanup_depth();
                self.loop_stack.last_mut().unwrap().breaks.push(LoopJump {
                    position: pos,
                    cleanup_depth,
                    span: statement.span,
                });
                Ok(())
            }
            Statement::Continue(statement) => {
                if self.loop_stack.is_empty() {
                    return Err(Error::with_kind_span(
                        ErrorKind::Runtime,
                        "continue outside loop".to_string(),
                        statement.span,
                    ));
                }
                let pos = self.emit(HirInstruction::Continue(statement.span));
                let cleanup_depth = self.semantic.cleanup_depth();
                self.loop_stack
                    .last_mut()
                    .unwrap()
                    .continues
                    .push(LoopJump {
                        position: pos,
                        cleanup_depth,
                        span: statement.span,
                    });
                Ok(())
            }
            Statement::Import(statement) => {
                let alias = statement
                    .alias
                    .as_ref()
                    .map(|alias| self.intern(alias.name.as_str()));
                if let Some(alias) = alias {
                    self.register_imported_builtin_module(alias, statement.path.value.as_str());
                } else {
                    self.register_bare_imported_builtins(statement.path.value.as_str());
                }
                self.emit(HirInstruction::Import {
                    alias,
                    path: statement.path.value.to_string(),
                    span: statement.span,
                });
                Ok(())
            }
            Statement::Export(statement) => {
                self.emit(HirInstruction::Export {
                    name: self.intern(statement.name.name.as_str()),
                    span: statement.name.span,
                });
                Ok(())
            }
            Statement::Struct(statement) => {
                let symbol = self.intern(statement.name.name.as_str());
                let slot = self.declare_current(symbol, statement.name.span)?;
                let mut fields = Vec::new();
                for field in &statement.fields {
                    if fields.contains(&field.name) {
                        return Err(Error::with_kind_span(
                            ErrorKind::Name,
                            format!(
                                "duplicate field '{}' in struct {}",
                                field.name, statement.name.name
                            ),
                            field.span,
                        ));
                    }
                    fields.push(field.name.to_string());
                }
                self.emit(HirInstruction::DefineStruct {
                    name: symbol,
                    slot,
                    fields,
                    span: statement.name.span,
                });
                Ok(())
            }
        }
    }

    fn compile_scoped_block(
        &mut self,
        block: &BlockStatement,
        keep_value: bool,
    ) -> Result<(), Error> {
        if !block_defines_names(block) {
            self.compile_statements(&block.statements, keep_value)?;
            return Ok(());
        }
        let enter_pos = self.emit(HirInstruction::EnterScope(0));
        self.semantic.enter_scope();
        self.compile_statements(&block.statements, keep_value)?;
        let local_count = self.semantic.exit_scope();
        self.patch_enter_scope(enter_pos, local_count);
        self.emit(HirInstruction::ExitScope);
        Ok(())
    }

    fn compile_while(
        &mut self,
        statement: &crate::ast::WhileStatement,
        keep_value: bool,
    ) -> Result<(), Error> {
        let loop_start = self.hir.len();
        let jump_false = self.compile_jump_if_false(&statement.condition)?;
        self.loop_stack.push(LoopContext {
            breaks: Vec::new(),
            continues: Vec::new(),
        });
        self.compile_scoped_block(&statement.consequence, false)?;
        self.emit(HirInstruction::Jump(loop_start));
        let loop_end = self.hir.len();
        self.patch_jump(jump_false, loop_end);
        let context = self.loop_stack.pop().unwrap();
        for jump in context.breaks {
            self.patch_loop_control(jump, loop_end);
        }
        for jump in context.continues {
            self.patch_loop_control(jump, loop_start);
        }
        if keep_value {
            self.emit(HirInstruction::ConstantEmpty);
        }
        Ok(())
    }

    fn compile_jump_if_false(&mut self, condition: &Expression) -> Result<usize, Error> {
        if let Expression::Identifier(identifier) = condition {
            let symbol = self.intern(identifier.name.as_str());
            if let Some((0, slot)) = self.resolve(symbol) {
                return Ok(self.emit(HirInstruction::JumpIfLocalFalse {
                    slot,
                    target: usize::MAX,
                    span: identifier.span,
                }));
            }
        }
        if let Expression::Infix(expression) = condition {
            match expression.token {
                Token::EQ | Token::NotEq | Token::LT | Token::Lte | Token::GT | Token::Gte => {
                    if let Expression::Identifier(identifier) = &expression.left {
                        let symbol = self.intern(identifier.name.as_str());
                        if let Expression::Integer(value) = &expression.right {
                            if let Some((0, slot)) = self.resolve(symbol) {
                                return Ok(self.emit(HirInstruction::JumpIfLocalIntFalse {
                                    slot,
                                    token: expression.token.clone(),
                                    operand: value.value,
                                    target: usize::MAX,
                                    span: expression.span,
                                }));
                            }
                        }
                        if let Expression::Identifier(right) = &expression.right {
                            let right_symbol = self.intern(right.name.as_str());
                            if let (Some((0, left_slot)), Some((0, right_slot))) =
                                (self.resolve(symbol), self.resolve(right_symbol))
                            {
                                return Ok(self.emit(HirInstruction::JumpIfLocalLocalFalse {
                                    left_slot,
                                    token: expression.token.clone(),
                                    right_slot,
                                    target: usize::MAX,
                                    span: expression.span,
                                }));
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        self.compile_expression(condition)?;
        Ok(self.emit(HirInstruction::JumpIfFalse(usize::MAX)))
    }

    fn compile_for_in(
        &mut self,
        statement: &ForInStatement,
        keep_value: bool,
    ) -> Result<(), Error> {
        self.compile_expression(&statement.collection)?;
        self.emit(HirInstruction::ForInStart {
            span: statement.span,
        });
        let loop_start = self.hir.len();
        self.semantic.push_for_scope();
        let key = self.intern(statement.key.name.as_str());
        let key_slot = self.declare_current(key, statement.key.span)?;
        let value = if let Some(value) = &statement.value {
            let value_symbol = self.intern(value.name.as_str());
            Some((
                value_symbol,
                self.declare_current(value_symbol, value.span)?,
            ))
        } else {
            None
        };
        let next_pos = self.emit(HirInstruction::ForInNext {
            key,
            key_slot,
            value,
            local_count: 0,
            end_target: usize::MAX,
            span: statement.span,
        });
        self.loop_stack.push(LoopContext {
            breaks: Vec::new(),
            continues: Vec::new(),
        });
        self.compile_statements(&statement.body.statements, false)?;
        self.emit(HirInstruction::ForInContinue {
            target: loop_start,
            span: statement.span,
        });
        let loop_end = self.hir.len();
        let context = self.loop_stack.pop().unwrap();
        for jump in context.breaks {
            self.hir[jump.position] = HirInstruction::ForInBreak {
                target: loop_end,
                span: statement.span,
            };
        }
        for jump in context.continues {
            self.hir[jump.position] = HirInstruction::ForInContinue {
                target: loop_start,
                span: statement.span,
            };
        }
        let local_count = self.semantic.pop_for_scope();
        self.patch_for_in_next(next_pos, local_count, loop_end);
        if keep_value {
            self.emit(HirInstruction::ConstantEmpty);
        }
        Ok(())
    }

    fn compile_assignment(
        &mut self,
        target: &AssignTarget,
        value: &Expression,
        span: Span,
        keep_value: bool,
    ) -> Result<bool, Error> {
        match target {
            AssignTarget::Identifier(identifier) => {
                let symbol = self.intern(identifier.name.as_str());
                if let Some((token, right)) =
                    self.self_update_expression(identifier.name.as_str(), value)
                {
                    if let Some((depth, slot)) = self.resolve(symbol) {
                        if depth == 0 {
                            if let Expression::Integer(value) = right {
                                if keep_value {
                                    self.emit(HirInstruction::UpdateLocalInt {
                                        slot,
                                        token: token.clone(),
                                        operand: value.value,
                                        span,
                                    });
                                } else {
                                    self.emit(HirInstruction::UpdateLocalIntNoResult {
                                        slot,
                                        token: token.clone(),
                                        operand: value.value,
                                        span,
                                    });
                                    return Ok(false);
                                }
                            } else if let Expression::Identifier(identifier) = right {
                                let operand = self.intern(identifier.name.as_str());
                                if let Some((0, operand_slot)) = self.resolve(operand) {
                                    if keep_value {
                                        self.emit(HirInstruction::UpdateLocalFromLocal {
                                            slot,
                                            token: token.clone(),
                                            operand_slot,
                                            span,
                                        });
                                    } else {
                                        self.emit(HirInstruction::UpdateLocalFromLocalNoResult {
                                            slot,
                                            token: token.clone(),
                                            operand_slot,
                                            span,
                                        });
                                        return Ok(false);
                                    }
                                } else {
                                    self.compile_expression(right)?;
                                    if keep_value {
                                        self.emit(HirInstruction::UpdateLocalInfix {
                                            slot,
                                            token: token.clone(),
                                            span,
                                        });
                                    } else {
                                        self.emit(HirInstruction::UpdateLocalInfixNoResult {
                                            slot,
                                            token: token.clone(),
                                            span,
                                        });
                                        return Ok(false);
                                    }
                                }
                            } else {
                                self.compile_expression(right)?;
                                if keep_value {
                                    self.emit(HirInstruction::UpdateLocalInfix {
                                        slot,
                                        token: token.clone(),
                                        span,
                                    });
                                } else {
                                    self.emit(HirInstruction::UpdateLocalInfixNoResult {
                                        slot,
                                        token: token.clone(),
                                        span,
                                    });
                                    return Ok(false);
                                }
                            }
                        } else {
                            self.compile_expression(right)?;
                            if self.should_capture_depth(depth) {
                                let index = self.upvalue_index(depth, slot);
                                if keep_value {
                                    self.emit(HirInstruction::UpdateUpvalueInfix {
                                        index,
                                        token: token.clone(),
                                        span,
                                    });
                                } else {
                                    self.emit(HirInstruction::UpdateUpvalueInfixNoResult {
                                        index,
                                        token: token.clone(),
                                        span,
                                    });
                                    return Ok(false);
                                }
                            } else {
                                if keep_value {
                                    self.emit(HirInstruction::UpdateSlotInfix {
                                        depth,
                                        slot,
                                        token: token.clone(),
                                        span,
                                    });
                                } else {
                                    self.emit(HirInstruction::UpdateSlotInfixNoResult {
                                        depth,
                                        slot,
                                        token: token.clone(),
                                        span,
                                    });
                                    return Ok(false);
                                }
                            }
                        }
                    } else {
                        self.compile_expression(right)?;
                        if keep_value {
                            self.emit(HirInstruction::UpdateNameInfix(symbol, token.clone(), span));
                        } else {
                            self.emit(HirInstruction::UpdateNameInfixNoResult(
                                symbol,
                                token.clone(),
                                span,
                            ));
                            return Ok(false);
                        }
                    }
                    return Ok(true);
                }
                self.compile_expression(value)?;
                if let Some((depth, slot)) = self.resolve(symbol) {
                    if depth == 0 {
                        if keep_value {
                            self.emit(HirInstruction::SetLocal {
                                slot,
                                span: identifier.span,
                            });
                        } else {
                            self.emit(HirInstruction::SetLocalNoResult {
                                slot,
                                span: identifier.span,
                            });
                            return Ok(false);
                        }
                    } else if self.should_capture_depth(depth) {
                        let index = self.upvalue_index(depth, slot);
                        if keep_value {
                            self.emit(HirInstruction::SetUpvalue {
                                index,
                                span: identifier.span,
                            });
                        } else {
                            self.emit(HirInstruction::SetUpvalueNoResult {
                                index,
                                span: identifier.span,
                            });
                            return Ok(false);
                        }
                    } else {
                        if keep_value {
                            self.emit(HirInstruction::SetSlot {
                                depth,
                                slot,
                                span: identifier.span,
                            });
                        } else {
                            self.emit(HirInstruction::SetSlotNoResult {
                                depth,
                                slot,
                                span: identifier.span,
                            });
                            return Ok(false);
                        }
                    }
                } else {
                    if keep_value {
                        self.emit(HirInstruction::SetName(symbol, identifier.span));
                    } else {
                        self.emit(HirInstruction::SetNameNoResult(symbol, identifier.span));
                        return Ok(false);
                    }
                }
            }
            AssignTarget::Index { left, index } => {
                self.compile_expression(left)?;
                self.compile_expression(index)?;
                self.compile_expression(value)?;
                if keep_value {
                    self.emit(HirInstruction::SetIndex(span));
                } else {
                    self.emit(HirInstruction::SetIndexNoResult(span));
                    return Ok(false);
                }
            }
            AssignTarget::Member { left, property } => {
                self.compile_expression(left)?;
                self.compile_expression(value)?;
                let property = self.intern(property.name.as_str());
                if keep_value {
                    self.emit(HirInstruction::SetMember(property, span));
                } else {
                    self.emit(HirInstruction::SetMemberNoResult(property, span));
                    return Ok(false);
                }
            }
        }
        Ok(true)
    }

    fn compile_expression_no_result(&mut self, expression: &Expression) -> Result<bool, Error> {
        let Expression::Call(call) = expression else {
            return Ok(false);
        };
        if let Expression::Member(member) = &call.function {
            if let Expression::Identifier(module) = &member.left {
                let module_symbol = self.intern(module.name.as_str());
                let member_symbol = self.intern(member.property.name.as_str());
                if let Some(op) = self.resolve_imported_builtin_member(module_symbol, member_symbol)
                {
                    for argument in &call.arguments {
                        self.compile_expression(argument)?;
                    }
                    self.emit(HirInstruction::CallBuiltinNoResult {
                        op,
                        arg_count: call.arguments.len(),
                        span: call.span,
                    });
                    return Ok(true);
                }
            }
        }
        if let Expression::Identifier(function) = &call.function {
            let symbol = self.intern(function.name.as_str());
            if let Some(op) = self.resolve_direct_builtin_call(symbol, function.name.as_str()) {
                if matches!(op, BuiltinOp::Append) && call.arguments.len() == 2 {
                    if let (Expression::Identifier(list), Expression::Identifier(value)) =
                        (&call.arguments[0], &call.arguments[1])
                    {
                        let list_symbol = self.intern(list.name.as_str());
                        let value_symbol = self.intern(value.name.as_str());
                        if let (Some((0, list_slot)), Some((0, value_slot))) =
                            (self.resolve(list_symbol), self.resolve(value_symbol))
                        {
                            self.emit(HirInstruction::AppendLocalNoResult {
                                list_slot,
                                value_slot,
                                span: call.span,
                            });
                            return Ok(true);
                        }
                    }
                }
                for argument in &call.arguments {
                    self.compile_expression(argument)?;
                }
                self.emit(HirInstruction::CallBuiltinNoResult {
                    op,
                    arg_count: call.arguments.len(),
                    span: call.span,
                });
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn self_update_expression<'a>(
        &self,
        target: &str,
        value: &'a Expression,
    ) -> Option<(&'a Token, &'a Expression)> {
        let Expression::Infix(infix) = value else {
            return None;
        };
        match infix.token {
            Token::Plus | Token::Minus | Token::Asterisk | Token::Slash => {}
            _ => return None,
        }
        let Expression::Identifier(identifier) = &infix.left else {
            return None;
        };
        if identifier.name != target {
            return None;
        }
        if !is_vm_pure_expression(&infix.right) {
            return None;
        }
        Some((&infix.token, &infix.right))
    }

    fn compile_expression(&mut self, expression: &Expression) -> Result<(), Error> {
        match expression {
            Expression::Bool(value) => {
                self.emit(HirInstruction::ConstantBool(value.value));
            }
            Expression::Null(_) => {
                self.emit(HirInstruction::ConstantNull);
            }
            Expression::Float(value) => {
                self.emit(HirInstruction::ConstantFloat(value.value));
            }
            Expression::Integer(value) => {
                self.emit(HirInstruction::ConstantInt(value.value));
            }
            Expression::String(value) => {
                let index = self.string_constant(value.value.as_str());
                self.emit(HirInstruction::ConstantString(index));
            }
            Expression::Identifier(identifier) => {
                let symbol = self.intern(identifier.name.as_str());
                if let Some((depth, slot)) = self.resolve(symbol) {
                    if depth == 0 {
                        self.emit(HirInstruction::GetLocal {
                            slot,
                            span: identifier.span,
                        });
                    } else if self.should_capture_depth(depth) {
                        let index = self.upvalue_index(depth, slot);
                        self.emit(HirInstruction::GetUpvalue {
                            index,
                            span: identifier.span,
                        });
                    } else {
                        self.emit(HirInstruction::GetSlot {
                            depth,
                            slot,
                            span: identifier.span,
                        });
                    }
                } else {
                    self.emit(HirInstruction::GetName(symbol, identifier.span));
                }
            }
            Expression::Prefix(expression) => self.compile_prefix(expression)?,
            Expression::Infix(expression) => self.compile_infix(expression)?,
            Expression::If(expression) => self.compile_if(expression)?,
            Expression::Slice(slice) => self.compile_slice(slice)?,
            Expression::Map(map) => self.compile_map(map)?,
            Expression::Index(index) => {
                self.compile_expression(&index.left)?;
                self.compile_expression(&index.index)?;
                self.emit(HirInstruction::Index(index.span));
            }
            Expression::Member(member) => {
                self.compile_expression(&member.left)?;
                let property = self.intern(member.property.name.as_str());
                self.emit(HirInstruction::Member(property, member.span));
            }
            Expression::Call(call) => {
                if let Expression::Member(member) = &call.function {
                    if let Expression::Identifier(module) = &member.left {
                        let module_symbol = self.intern(module.name.as_str());
                        let member_symbol = self.intern(member.property.name.as_str());
                        if let Some(op) =
                            self.resolve_imported_builtin_member(module_symbol, member_symbol)
                        {
                            for argument in &call.arguments {
                                self.compile_expression(argument)?;
                            }
                            self.emit(HirInstruction::CallBuiltin {
                                op,
                                arg_count: call.arguments.len(),
                                span: call.span,
                            });
                            return Ok(());
                        }
                    }
                }
                if let Expression::Identifier(function) = &call.function {
                    let symbol = self.intern(function.name.as_str());
                    if let Some(op) =
                        self.resolve_direct_builtin_call(symbol, function.name.as_str())
                    {
                        for argument in &call.arguments {
                            self.compile_expression(argument)?;
                        }
                        self.emit(HirInstruction::CallBuiltin {
                            op,
                            arg_count: call.arguments.len(),
                            span: call.span,
                        });
                        return Ok(());
                    }
                }
                self.compile_expression(&call.function)?;
                for argument in &call.arguments {
                    self.compile_expression(argument)?;
                }
                self.emit(HirInstruction::Call {
                    arg_count: call.arguments.len(),
                    span: call.span,
                    frame_name: call_frame_name(&call.function),
                });
            }
            Expression::Function(function) => {
                if let Some(name) = &function.name {
                    let symbol = self.intern(name.name.as_str());
                    let slot = self.declare_current(symbol, name.span)?;
                    let compiled = self.compile_function(function)?;
                    self.emit(HirInstruction::MakeFunction(compiled));
                    self.emit(HirInstruction::DefineLocal {
                        name: symbol,
                        slot,
                        span: name.span,
                    });
                    self.emit(HirInstruction::GetLocal {
                        slot,
                        span: name.span,
                    });
                } else {
                    let compiled = self.compile_function(function)?;
                    self.emit(HirInstruction::MakeFunction(compiled));
                }
            }
            Expression::Grouped(expression) => self.compile_expression(&expression.expression)?,
            Expression::StructLiteral(literal) => self.compile_struct_literal(literal)?,
        };
        Ok(())
    }

    fn compile_struct_literal(&mut self, literal: &StructLiteral) -> Result<(), Error> {
        self.compile_expression(&literal.name)?;
        if !literal.values.is_empty() {
            for value in &literal.values {
                self.compile_expression(value)?;
            }
            self.emit(HirInstruction::MakeStructPositional {
                count: literal.values.len(),
                span: literal.span,
            });
            return Ok(());
        }
        let mut fields = Vec::new();
        for (field, value) in &literal.fields {
            fields.push(self.intern(field.name.as_str()));
            self.compile_expression(value)?;
        }
        self.emit(HirInstruction::MakeStructNamed {
            fields,
            span: literal.span,
        });
        Ok(())
    }

    fn compile_prefix(&mut self, expression: &PrefixExpression) -> Result<(), Error> {
        self.compile_expression(&expression.right)?;
        self.emit(HirInstruction::Prefix(
            expression.token.clone(),
            expression.span,
        ));
        Ok(())
    }

    fn compile_infix(&mut self, expression: &InfixExpression) -> Result<(), Error> {
        match expression.token {
            Token::Assign => {
                let Some(target) = expression_to_assign_target(&expression.left) else {
                    self.compile_expression(&expression.left)?;
                    self.compile_expression(&expression.right)?;
                    self.emit(HirInstruction::Infix(
                        expression.token.clone(),
                        expression.span,
                    ));
                    return Ok(());
                };
                self.compile_assignment(&target, &expression.right, expression.span, true)?;
            }
            Token::And => {
                self.compile_expression(&expression.left)?;
                let jump_false = self.emit(HirInstruction::JumpIfFalse(usize::MAX));
                self.compile_expression(&expression.right)?;
                self.emit(HirInstruction::AssertBool(expression.right.span()));
                let jump_end = self.emit(HirInstruction::Jump(usize::MAX));
                let false_pos = self.hir.len();
                self.emit(HirInstruction::ConstantBool(false));
                let end = self.hir.len();
                self.patch_jump(jump_false, false_pos);
                self.patch_jump(jump_end, end);
            }
            Token::Or => {
                self.compile_expression(&expression.left)?;
                let jump_false = self.emit(HirInstruction::JumpIfFalse(usize::MAX));
                self.emit(HirInstruction::ConstantBool(true));
                let jump_end = self.emit(HirInstruction::Jump(usize::MAX));
                let false_pos = self.hir.len();
                self.compile_expression(&expression.right)?;
                self.emit(HirInstruction::AssertBool(expression.right.span()));
                let end = self.hir.len();
                self.patch_jump(jump_false, false_pos);
                self.patch_jump(jump_end, end);
            }
            _ => {
                if self.compile_local_int_compare(expression) {
                    return Ok(());
                }
                if self.compile_local_infix(expression) {
                    return Ok(());
                }
                self.compile_expression(&expression.left)?;
                self.compile_expression(&expression.right)?;
                self.emit(HirInstruction::Infix(
                    expression.token.clone(),
                    expression.span,
                ));
            }
        }
        Ok(())
    }

    fn compile_local_infix(&mut self, expression: &InfixExpression) -> bool {
        match expression.token {
            Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Slash
            | Token::EQ
            | Token::NotEq
            | Token::LT
            | Token::Lte
            | Token::GT
            | Token::Gte => {}
            _ => return false,
        }
        let Expression::Identifier(left) = &expression.left else {
            return false;
        };
        let left_symbol = self.intern(left.name.as_str());
        let Some((0, left_slot)) = self.resolve(left_symbol) else {
            return false;
        };
        if let Expression::Integer(right) = &expression.right {
            self.emit(HirInstruction::InfixLocalInt {
                slot: left_slot,
                token: expression.token.clone(),
                operand: right.value,
                span: expression.span,
            });
            return true;
        }
        if let Expression::Identifier(right) = &expression.right {
            let right_symbol = self.intern(right.name.as_str());
            if let Some((0, right_slot)) = self.resolve(right_symbol) {
                self.emit(HirInstruction::InfixLocalLocal {
                    left_slot,
                    token: expression.token.clone(),
                    right_slot,
                    span: expression.span,
                });
                return true;
            }
        }
        false
    }

    fn compile_local_int_compare(&mut self, expression: &InfixExpression) -> bool {
        match expression.token {
            Token::EQ | Token::NotEq | Token::LT | Token::Lte | Token::GT | Token::Gte => {}
            _ => return false,
        }
        let Expression::Identifier(identifier) = &expression.left else {
            return false;
        };
        let Expression::Integer(value) = &expression.right else {
            return false;
        };
        let symbol = self.intern(identifier.name.as_str());
        let Some((0, slot)) = self.resolve(symbol) else {
            return false;
        };
        self.emit(HirInstruction::CompareLocalInt {
            slot,
            token: expression.token.clone(),
            operand: value.value,
            span: expression.span,
        });
        true
    }

    fn compile_if(&mut self, expression: &IfExpression) -> Result<(), Error> {
        self.compile_expression(&expression.condition)?;
        let jump_false = self.emit(HirInstruction::JumpIfFalse(usize::MAX));
        self.compile_scoped_block(&expression.consequence, true)?;
        let jump_end = self.emit(HirInstruction::Jump(usize::MAX));
        let alternative_start = self.hir.len();
        if let Some(alternative) = &expression.alternative {
            self.compile_scoped_block(alternative, true)?;
        } else if let Some(optional) = &expression.optional {
            self.compile_expression(optional)?;
        } else {
            self.emit(HirInstruction::ConstantEmpty);
        }
        let end = self.hir.len();
        self.patch_jump(jump_false, alternative_start);
        self.patch_jump(jump_end, end);
        Ok(())
    }

    fn compile_slice(&mut self, slice: &SliceLiteral) -> Result<(), Error> {
        for element in &slice.elements {
            self.compile_expression(element)?;
        }
        self.emit(HirInstruction::MakeSlice(slice.elements.len()));
        Ok(())
    }

    fn compile_map(&mut self, map: &MapLiteral) -> Result<(), Error> {
        for (key, value) in &map.kv_pair {
            self.compile_expression(key)?;
            self.compile_expression(value)?;
        }
        self.emit(HirInstruction::MakeMap(map.kv_pair.len(), map.span));
        Ok(())
    }

    fn emit(&mut self, instruction: HirInstruction) -> usize {
        self.hir.push(instruction);
        self.hir.len() - 1
    }

    fn patch_jump(&mut self, position: usize, target: usize) {
        match &mut self.hir[position] {
            HirInstruction::Jump(value) | HirInstruction::JumpIfFalse(value) => *value = target,
            HirInstruction::JumpIfLocalFalse {
                target: jump_target,
                ..
            } => *jump_target = target,
            HirInstruction::JumpIfLocalIntFalse {
                target: jump_target,
                ..
            } => *jump_target = target,
            HirInstruction::JumpIfLocalLocalFalse {
                target: jump_target,
                ..
            } => *jump_target = target,
            _ => unreachable!(),
        }
    }

    fn patch_enter_scope(&mut self, position: usize, local_count: usize) {
        match &mut self.hir[position] {
            HirInstruction::EnterScope(value) => *value = local_count,
            _ => unreachable!(),
        }
    }

    fn patch_for_in_next(&mut self, position: usize, local_count: usize, end_target: usize) {
        match &mut self.hir[position] {
            HirInstruction::ForInNext {
                local_count: count,
                end_target: target,
                ..
            } => {
                *count = local_count;
                *target = end_target;
            }
            _ => unreachable!(),
        }
    }

    fn patch_loop_control(&mut self, jump: LoopJump, target: usize) {
        self.hir[jump.position] = if jump.cleanup_depth == 0 {
            HirInstruction::Jump(target)
        } else {
            HirInstruction::ScopeCleanupJump {
                target,
                depth: jump.cleanup_depth,
                span: jump.span,
            }
        };
    }
}

pub(super) fn call_frame_name(function: &Expression) -> Option<String> {
    match function {
        Expression::Identifier(identifier) => Some(identifier.name.to_string()),
        Expression::Member(member) => Some(member.property.name.to_string()),
        _ => None,
    }
}

fn stdlib_builtin_exports_for_path(path: &str) -> Option<&'static [&'static str]> {
    let normalized = path.replace('\\', "/");
    let stdlib_path = normalized
        .strip_prefix("stdlib/")
        .or_else(|| normalized.split_once("/stdlib/").map(|(_, suffix)| suffix))?;
    match stdlib_path {
        "encoding.monkey" => Some(&[
            "url_encode",
            "url_decode",
            "base64_encode",
            "base64_decode",
            "sha256",
        ]),
        "env.monkey" => Some(&["env_get", "env_set", "cwd", "set_cwd", "args", "exit"]),
        "fs.monkey" => Some(&[
            "read_file",
            "write_file",
            "append_file",
            "file_exists",
            "read_dir",
            "mkdir",
            "remove_file",
            "remove_dir",
            "copy_file",
            "rename",
            "metadata",
        ]),
        "http.monkey" => Some(&["http_get", "http_post", "http_request"]),
        "json.monkey" => Some(&["json_parse", "json_stringify"]),
        "list.monkey" => Some(&["sort"]),
        "math.monkey" => Some(&["abs", "floor", "ceil", "round", "sqrt", "pow", "min", "max"]),
        "path.monkey" => Some(&[
            "path_join",
            "path_dirname",
            "path_basename",
            "path_ext",
            "path_exists",
            "path_is_file",
            "path_is_dir",
        ]),
        "prelude.monkey" => Some(&["sort"]),
        "process.monkey" => Some(&["exec"]),
        "random.monkey" => Some(&["random_int", "random_float"]),
        "terminal.monkey" => Some(&[
            "clear",
            "home",
            "hide_cursor",
            "show_cursor",
            "enable_raw_mode",
            "disable_raw_mode",
            "read_key",
            "read_key_timeout",
            "read_key_latest_timeout",
            "move",
            "clear_line",
            "size",
            "enter_alt_screen",
            "leave_alt_screen",
            "fg",
            "bg",
            "bold",
            "reset_style",
            "paint",
            "paint_runs",
        ]),
        "time.monkey" => Some(&["time_ms", "now_ms", "sleep_ms"]),
        _ => None,
    }
}

pub(super) fn block_defines_names(block: &BlockStatement) -> bool {
    block.statements.iter().any(statement_defines_names)
}

fn statement_defines_names(statement: &Statement) -> bool {
    match statement {
        Statement::Let(_) | Statement::Struct(_) | Statement::Import(_) | Statement::Export(_) => {
            true
        }
        Statement::Expression(statement) => expression_defines_names(&statement.expression),
        Statement::Return(statement) => expression_defines_names(&statement.value),
        Statement::Assign(statement) => {
            assign_target_defines_names(&statement.target)
                || expression_defines_names(&statement.value)
        }
        Statement::While(statement) => expression_defines_names(&statement.condition),
        Statement::ForIn(statement) => expression_defines_names(&statement.collection),
        Statement::Block(block) => block_defines_names(block),
        Statement::Break(_) | Statement::Continue(_) | Statement::Empty(_) => false,
    }
}

fn assign_target_defines_names(target: &AssignTarget) -> bool {
    match target {
        AssignTarget::Identifier(_) => false,
        AssignTarget::Index { left, index } => {
            expression_defines_names(left) || expression_defines_names(index)
        }
        AssignTarget::Member { left, .. } => expression_defines_names(left),
    }
}

fn expression_to_assign_target(expression: &Expression) -> Option<AssignTarget> {
    match expression {
        Expression::Identifier(identifier) => Some(AssignTarget::Identifier(identifier.clone())),
        Expression::Grouped(expression) => expression_to_assign_target(&expression.expression),
        Expression::Index(index) => Some(AssignTarget::Index {
            left: index.left.clone(),
            index: index.index.clone(),
        }),
        Expression::Member(member) => Some(AssignTarget::Member {
            left: member.left.clone(),
            property: member.property.clone(),
        }),
        _ => None,
    }
}

fn expression_defines_names(expression: &Expression) -> bool {
    match expression {
        Expression::Function(function) => function.name.is_some(),
        Expression::Grouped(expression) => expression_defines_names(&expression.expression),
        Expression::Prefix(expression) => expression_defines_names(&expression.right),
        Expression::Infix(expression) => {
            expression_defines_names(&expression.left)
                || expression_defines_names(&expression.right)
        }
        Expression::If(expression) => {
            expression_defines_names(&expression.condition)
                || block_defines_names(&expression.consequence)
                || expression
                    .alternative
                    .as_ref()
                    .is_some_and(block_defines_names)
                || expression
                    .optional
                    .as_ref()
                    .is_some_and(|expression| expression_defines_names(expression))
        }
        Expression::Slice(slice) => slice.elements.iter().any(expression_defines_names),
        Expression::Map(map) => map
            .kv_pair
            .iter()
            .any(|(key, value)| expression_defines_names(key) || expression_defines_names(value)),
        Expression::Index(index) => {
            expression_defines_names(&index.left) || expression_defines_names(&index.index)
        }
        Expression::Member(member) => expression_defines_names(&member.left),
        Expression::Call(call) => {
            expression_defines_names(&call.function)
                || call.arguments.iter().any(expression_defines_names)
        }
        Expression::StructLiteral(literal) => {
            expression_defines_names(&literal.name)
                || literal.values.iter().any(expression_defines_names)
                || literal
                    .fields
                    .iter()
                    .any(|(_, value)| expression_defines_names(value))
        }
        Expression::Bool(_)
        | Expression::Null(_)
        | Expression::Float(_)
        | Expression::Integer(_)
        | Expression::String(_)
        | Expression::Identifier(_) => false,
    }
}

pub(super) fn is_vm_pure_expression(expression: &Expression) -> bool {
    match expression {
        Expression::Bool(_)
        | Expression::Null(_)
        | Expression::Float(_)
        | Expression::Integer(_)
        | Expression::String(_)
        | Expression::Identifier(_) => true,
        Expression::Grouped(expression) => is_vm_pure_expression(&expression.expression),
        Expression::Prefix(expression) => is_vm_pure_expression(&expression.right),
        Expression::Infix(expression) => {
            is_vm_pure_expression(&expression.left) && is_vm_pure_expression(&expression.right)
        }
        Expression::Slice(slice) => slice.elements.iter().all(is_vm_pure_expression),
        Expression::Map(map) => map
            .kv_pair
            .iter()
            .all(|(key, value)| is_vm_pure_expression(key) && is_vm_pure_expression(value)),
        Expression::StructLiteral(literal) => {
            is_vm_pure_expression(&literal.name)
                && literal.values.iter().all(is_vm_pure_expression)
                && literal
                    .fields
                    .iter()
                    .all(|(_, value)| is_vm_pure_expression(value))
        }
        Expression::Index(index) => {
            is_vm_pure_expression(&index.left) && is_vm_pure_expression(&index.index)
        }
        Expression::Member(member) => is_vm_pure_expression(&member.left),
        Expression::If(_) | Expression::Call(_) | Expression::Function(_) => false,
    }
}
