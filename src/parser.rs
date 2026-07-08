use crate::ast::{
    AssignStatement, AssignTarget, BlockStatement, BoolLiteral, BreakStatement, CallExpression,
    ContinueStatement, EmptyStatement, ExportStatement, Expression, ExpressionStatement,
    FloatLiteral, ForInStatement, FunctionLiteral, Identifier, IfExpression, ImportStatement,
    IndexExpression, InfixExpression, IntegerLiteral, LetStatement, MapLiteral, MemberExpression,
    NullLiteral, PrefixExpression, Program, ReturnStatement, SliceLiteral, Statement,
    StringLiteral, StructLiteral, StructStatement, WhileStatement,
};
use crate::error::{make_error_with_source_span, Error, ErrorKind};
use crate::lexer::{Lexer, Position, Span};
use crate::token::{Precedence, Token};
use std::collections::HashMap;
use std::mem::discriminant;
use std::mem::Discriminant;

#[derive(Clone)]
pub struct Parser {
    lexer: Lexer,
    path: Option<String>,
    cur_token: Token,
    peek_token: Token,
    cur_token_span: Span,
    peek_token_span: Span,
    expression_context: Vec<ExpressionContext>,
    prefix_parse_fns: HashMap<Discriminant<Token>, PrefixParseFn>,
    infix_parse_fns: HashMap<Discriminant<Token>, InfixParseFn>,
}

pub fn new_parser(lexer: Lexer) -> Parser {
    let mut parser = Parser {
        path: lexer.path(),
        lexer,
        cur_token: Token::Eof,
        peek_token: Token::Eof,
        cur_token_span: Span::point(Position(0, 0)),
        peek_token_span: Span::point(Position(0, 0)),
        expression_context: vec![ExpressionContext::Normal],
        prefix_parse_fns: HashMap::new(),
        infix_parse_fns: HashMap::new(),
    };
    // For expressions, the current token is the current prefix/infix token when calling the function
    // After the function returns, the current token is the last token in the complete expression
    let prefix_parsers = [
        (
            Token::Ident(String::default()),
            Parser::parse_identifier as PrefixParseFn,
        ),
        (
            Token::IntLiteral(i64::default()),
            Parser::parse_integer_literal,
        ),
        (
            Token::FloatLiteral(f64::default()),
            Parser::parse_float_literal,
        ),
        (Token::True, Parser::parse_bool_literal),
        (Token::False, Parser::parse_bool_literal),
        (Token::Null, Parser::parse_null_literal),
        (
            Token::StringLiteral(String::default()),
            Parser::parse_string_literal,
        ),
        (Token::Bang, Parser::parse_prefix_expression),
        (Token::Minus, Parser::parse_prefix_expression),
        (Token::Lparen, Parser::parse_grouped_expression),
        (Token::IF, Parser::parse_if_expression),
        (Token::Function, Parser::parse_function_expression),
        (Token::Lbrace, Parser::parse_map_literal),
        (Token::Lbracket, Parser::parse_slice_literal),
    ];
    for (token, prefix_fn) in prefix_parsers {
        parser.register_prefix(token, prefix_fn)
    }
    // infix
    let infix_parsers = [
        (Token::EQ, Parser::parse_infix_expression as InfixParseFn),
        (Token::NotEq, Parser::parse_infix_expression),
        (Token::And, Parser::parse_infix_expression),
        (Token::Or, Parser::parse_infix_expression),
        (Token::LT, Parser::parse_infix_expression),
        (Token::Lte, Parser::parse_infix_expression),
        (Token::GT, Parser::parse_infix_expression),
        (Token::Gte, Parser::parse_infix_expression),
        (Token::Plus, Parser::parse_infix_expression),
        (Token::Minus, Parser::parse_infix_expression),
        (Token::Slash, Parser::parse_infix_expression),
        (Token::Asterisk, Parser::parse_infix_expression),
        (Token::Assign, Parser::parse_infix_expression),
        (Token::Lparen, Parser::parse_call_expression),
        (Token::Lbracket, Parser::parse_index_expression),
        (Token::Dot, Parser::parse_member_expression),
        (Token::Lbrace, Parser::parse_struct_literal),
    ];
    for (token, infix_fn) in infix_parsers {
        parser.register_infix(token, infix_fn)
    }
    // prefetch
    parser.next_token();
    parser.next_token();
    parser
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ExpressionContext {
    Normal,
    BeforeBlock,
}

#[derive(Clone, Copy)]
enum StructLiteralStyle {
    Named,
    Positional,
}

impl Parser {
    fn register_prefix(&mut self, token: Token, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(discriminant(&token), func);
    }

    fn register_infix(&mut self, token: Token, func: InfixParseFn) {
        self.infix_parse_fns.insert(discriminant(&token), func);
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.to_owned();
        self.cur_token_span = self.peek_token_span;
        let token = self.lexer.next_spanned_token();
        self.peek_token_span = token.span;
        self.peek_token = token.token;
    }

    fn expect_peek(&mut self, ok: bool) -> bool {
        if ok {
            self.next_token();
        }
        ok
    }

    fn cur_token_precedence(&self) -> Precedence {
        self.cur_token.precedence()
    }

    fn peek_token_precedence(&self) -> Precedence {
        self.peek_token.precedence()
    }

    pub fn parse_program(&mut self) -> Result<Program, Error> {
        let mut program = Program {
            statements: vec![],
            path: self.path.to_owned(),
        };
        while self.cur_token != Token::Eof {
            if self.cur_token == Token::Semicolon {
                program.statements.push(Statement::Empty(EmptyStatement {
                    span: self.cur_token_span,
                }));
                self.next_token();
                continue;
            }
            program.statements.push(self.parse_statement()?);
            self.next_token();
        }
        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<Statement, Error> {
        self._parse_statement(false)
    }

    fn parse_statement_in_block(&mut self) -> Result<Statement, Error> {
        self._parse_statement(true)
    }

    fn _parse_statement(&mut self, in_block: bool) -> Result<Statement, Error> {
        match &self.cur_token {
            Token::Let => Ok(Statement::Let(self.parse_let_statement()?)),
            Token::Return => Ok(Statement::Return(self.parse_return_statement()?)),
            Token::Import => Ok(Statement::Import(self.parse_import_statement()?)),
            Token::Export => Ok(Statement::Export(self.parse_export_statement()?)),
            Token::Struct => Ok(Statement::Struct(self.parse_struct_statement()?)),
            Token::Break => Ok(Statement::Break(self.parse_break_statement()?)),
            Token::Continue => Ok(Statement::Continue(self.parse_continue_statement()?)),
            Token::Lbrace => self.parse_lbrace_statement(in_block),
            Token::While => Ok(Statement::While(self.parse_while_statement()?)),
            Token::For => Ok(Statement::ForIn(self.parse_for_in_statement()?)),
            _ => self.parse_expression_statement_as_statement(in_block),
        }
    }

    fn parse_lbrace_statement(&mut self, in_block: bool) -> Result<Statement, Error> {
        let mut expression_parser = self.clone();
        if let Ok(statement) = expression_parser.parse_expression_statement_as_statement(in_block) {
            *self = expression_parser;
            return Ok(statement);
        }
        Ok(Statement::Block(self.parse_block_statement()?))
    }

    fn parse_expression_statement_as_statement(
        &mut self,
        in_block: bool,
    ) -> Result<Statement, Error> {
        let stmt = self.parse_expression_statement()?;
        // block statement last statement can not semicolon
        // but cannot determine if this is the last statement here
        // so need to hand this over to the block statement for inspection
        if !in_block
            && !self.peek_token.is_eof()
            && self.must_end_of_semicolon(&stmt.expression)
            && !stmt.end_of_semicolon
        {
            return Err(self.make_syntax_error(Token::Semicolon));
        }
        Ok(Self::expression_statement_to_statement(stmt))
    }

    fn expression_statement_to_statement(stmt: ExpressionStatement) -> Statement {
        let ExpressionStatement {
            span,
            expression,
            end_of_semicolon,
        } = stmt;
        match expression {
            Expression::Infix(infix) if infix.token.is_assign() => {
                let InfixExpression {
                    span: infix_span,
                    token,
                    left,
                    right,
                } = *infix;
                match Self::expression_to_assign_target_or_expression(left) {
                    Ok(target) => Statement::Assign(AssignStatement {
                        span,
                        target,
                        value: right,
                    }),
                    Err(left) => Statement::Expression(ExpressionStatement {
                        span,
                        expression: Expression::Infix(Box::new(InfixExpression {
                            span: infix_span,
                            token,
                            left: *left,
                            right,
                        })),
                        end_of_semicolon,
                    }),
                }
            }
            expression => Statement::Expression(ExpressionStatement {
                span,
                expression,
                end_of_semicolon,
            }),
        }
    }

    fn expression_to_assign_target_or_expression(
        expression: Expression,
    ) -> Result<AssignTarget, Box<Expression>> {
        match expression {
            Expression::Identifier(identifier) => Ok(AssignTarget::Identifier(identifier)),
            Expression::Index(index) => Ok(AssignTarget::Index {
                left: index.left,
                index: index.index,
            }),
            Expression::Member(member) => Ok(AssignTarget::Member {
                left: member.left,
                property: member.property,
            }),
            Expression::Grouped(grouped) => {
                let crate::ast::GroupedExpression { span, expression } = *grouped;
                Self::expression_to_assign_target_or_expression(expression).map_err(|expression| {
                    Box::new(Expression::Grouped(Box::new(
                        crate::ast::GroupedExpression {
                            span,
                            expression: *expression,
                        },
                    )))
                })
            }
            expression => Err(Box::new(expression)),
        }
    }

    fn must_end_of_semicolon(&self, expression: &Expression) -> bool {
        !matches!(expression, Expression::Function(_) | Expression::If(_))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, Error> {
        self.with_expression_context(ExpressionContext::Normal, |parser| {
            parser.parse_block_statement_inner()
        })
    }

    fn parse_block_statement_inner(&mut self) -> Result<BlockStatement, Error> {
        debug_assert!(self.cur_token.is_lbrace());
        let mut block = BlockStatement {
            span: self.cur_token_span,
            statements: vec![],
        };
        self.next_token();
        while !self.cur_token.is_rbrace() && !self.cur_token.is_eof() {
            if self.cur_token.is_semicolon() {
                block.statements.push(Statement::Empty(EmptyStatement {
                    span: self.cur_token_span,
                }));
                self.next_token();
                continue;
            }
            let stmt = self.parse_statement_in_block()?;
            // is not last express statement; must be to check end of semicolon
            if !self.peek_token.is_rbrace() && !self.peek_token.is_eof() {
                if let Statement::Expression(ref stmt) = stmt {
                    if self.must_end_of_semicolon(&stmt.expression) && !stmt.end_of_semicolon {
                        return Err(self.make_syntax_error(Token::Semicolon));
                    }
                }
            }
            block.statements.push(stmt);
            self.next_token();
        }
        if self.cur_token.is_eof() {
            return Err(self.make_error("unclosed block".to_string(), self.cur_token_span));
        }
        block.span = Span::new(block.span.start, self.cur_token_span.end);
        Ok(block)
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, Error> {
        debug_assert!(self.cur_token.is_let());
        let start = self.cur_token_span.start;
        if !self.expect_peek(self.peek_token.is_ident()) {
            return Err(self.make_syntax_error(Token::Ident(String::default())));
        }
        let name = Identifier {
            span: self.cur_token_span,
            name: self.cur_token.literal(),
        };
        if !self.expect_peek(self.peek_token.is_assign()) {
            return Err(self.make_syntax_error(Token::Assign));
        }
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(self.peek_token.is_semicolon()) {
            return Err(self.make_syntax_error(Token::Semicolon));
        }
        Ok(LetStatement {
            span: Span::new(start, self.cur_token_span.end),
            name,
            value,
        })
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, Error> {
        debug_assert!(self.cur_token.is_return());
        let start = self.cur_token_span.start;
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(self.peek_token.is_semicolon()) {
            return Err(self.make_syntax_error(Token::Semicolon));
        }
        Ok(ReturnStatement {
            span: Span::new(start, self.cur_token_span.end),
            value,
        })
    }

    fn parse_import_statement(&mut self) -> Result<ImportStatement, Error> {
        debug_assert!(self.cur_token.is_import());
        let start = self.cur_token_span.start;
        let mut alias = None;
        if self.peek_token.is_ident() {
            self.next_token();
            alias = Some(Identifier {
                span: self.cur_token_span,
                name: self.cur_token.literal(),
            });
            if !self.expect_peek(self.peek_token.is_from()) {
                return Err(self.make_syntax_error(Token::From));
            }
        }
        if !self.expect_peek(self.peek_token.is_string_literal()) {
            return Err(self.make_syntax_error(Token::StringLiteral(String::default())));
        }
        let path = match self.cur_token.to_owned() {
            Token::StringLiteral(value) => StringLiteral {
                span: self.cur_token_span,
                value,
            },
            _ => unreachable!(),
        };
        if !self.expect_peek(self.peek_token.is_semicolon()) {
            return Err(self.make_syntax_error(Token::Semicolon));
        }
        Ok(ImportStatement {
            span: Span::new(start, self.cur_token_span.end),
            alias,
            path,
        })
    }

    fn parse_export_statement(&mut self) -> Result<ExportStatement, Error> {
        debug_assert!(self.cur_token.is_export());
        let start = self.cur_token_span.start;
        if !self.expect_peek(self.peek_token.is_ident()) {
            return Err(self.make_syntax_error(Token::Ident(String::default())));
        }
        let name = Identifier {
            span: self.cur_token_span,
            name: self.cur_token.literal(),
        };
        if !self.expect_peek(self.peek_token.is_semicolon()) {
            return Err(self.make_syntax_error(Token::Semicolon));
        }
        Ok(ExportStatement {
            span: Span::new(start, self.cur_token_span.end),
            name,
        })
    }

    fn parse_struct_statement(&mut self) -> Result<StructStatement, Error> {
        debug_assert!(self.cur_token.is_struct());
        let start = self.cur_token_span.start;
        if !self.expect_peek(self.peek_token.is_ident()) {
            return Err(self.make_syntax_error(Token::Ident(String::default())));
        }
        let name = Identifier {
            span: self.cur_token_span,
            name: self.cur_token.literal(),
        };
        if !self.expect_peek(self.peek_token.is_lbrace()) {
            return Err(self.make_syntax_error(Token::Lbrace));
        }
        let mut fields = Vec::new();
        while !self.peek_token.is_rbrace() && !self.peek_token.is_eof() {
            self.next_token();
            if self.cur_token.is_comma() {
                return Err(self.make_syntax_error(Token::Ident(String::default())));
            }
            if !self.cur_token.is_ident() {
                return Err(self.make_syntax_error(Token::Ident(String::default())));
            }
            fields.push(Identifier {
                span: self.cur_token_span,
                name: self.cur_token.literal(),
            });
            if !self.expect_peek(self.peek_token.is_semicolon()) {
                return Err(self.make_syntax_error(Token::Semicolon));
            }
        }
        if !self.expect_peek(self.peek_token.is_rbrace()) {
            return Err(self.make_syntax_error(Token::Rbrace));
        }
        Ok(StructStatement {
            span: Span::new(start, self.cur_token_span.end),
            name,
            fields,
        })
    }

    fn parse_break_statement(&mut self) -> Result<BreakStatement, Error> {
        debug_assert!(self.cur_token.is_break());
        let start = self.cur_token_span.start;
        if !self.expect_peek(self.peek_token.is_semicolon()) {
            return Err(self.make_syntax_error(Token::Semicolon));
        }
        Ok(BreakStatement {
            span: Span::new(start, self.cur_token_span.end),
        })
    }

    fn parse_continue_statement(&mut self) -> Result<ContinueStatement, Error> {
        debug_assert!(self.cur_token.is_continue());
        let start = self.cur_token_span.start;
        if !self.expect_peek(self.peek_token.is_semicolon()) {
            return Err(self.make_syntax_error(Token::Semicolon));
        }
        Ok(ContinueStatement {
            span: Span::new(start, self.cur_token_span.end),
        })
    }

    fn parse_while_statement(&mut self) -> Result<WhileStatement, Error> {
        debug_assert!(self.cur_token.is_while());
        let start = self.cur_token_span.start;
        self.next_token();
        let condition = self.parse_expression_before_block(Precedence::Lowest)?;
        if !self.expect_peek(self.peek_token.is_lbrace()) {
            return Err(self.make_syntax_error(Token::Lbrace));
        }
        let consequence = self.parse_block_statement()?;
        Ok(WhileStatement {
            span: Span::new(start, consequence.span.end),
            condition,
            consequence,
        })
    }

    fn parse_for_in_statement(&mut self) -> Result<ForInStatement, Error> {
        debug_assert!(self.cur_token.is_for());
        let start = self.cur_token_span.start;
        self.next_token();
        let key = match self.parse_expression(Precedence::Lowest)? {
            Expression::Identifier(ident) => ident,
            _ => return Err(self.make_error("should be variable".to_string(), self.cur_token_span)),
        };
        let mut value = None;
        if self.expect_peek(self.peek_token.is_comma()) {
            self.next_token();
            value = Some(match self.parse_expression(Precedence::Lowest)? {
                Expression::Identifier(ident) => ident,
                _ => {
                    return Err(
                        self.make_error("should be variable".to_string(), self.cur_token_span)
                    )
                }
            })
        }
        if !self.expect_peek(self.peek_token.is_in()) {
            return Err(self.make_syntax_error(Token::IN));
        }
        self.next_token();
        let collection = self.parse_expression_before_block(Precedence::Lowest)?;
        if !self.expect_peek(self.peek_token.is_lbrace()) {
            return Err(self.make_syntax_error(Token::Lbrace));
        }
        let body = self.parse_block_statement()?;
        Ok(ForInStatement {
            span: Span::new(start, body.span.end),
            collection,
            key,
            value,
            body,
        })
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, Error> {
        let expression = self.parse_expression(Precedence::Lowest)?;
        let mut span = expression.span();
        let end_of_semicolon = self.expect_peek(self.peek_token.is_semicolon());
        if end_of_semicolon {
            span = Span::new(span.start, self.cur_token_span.end);
        }
        let stmt = ExpressionStatement {
            span,
            expression,
            end_of_semicolon,
        };
        Ok(stmt)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, Error> {
        if self.cur_token.is_semicolon() || self.cur_token.is_rbrace() || self.cur_token.is_eof() {
            return Err(self.make_error(
                format!("expected expression, got {}", self.cur_token.identifier()),
                self.cur_token_span,
            ));
        }
        let mut left_exp = self
            .prefix_parse_fns
            .get(&discriminant(&self.cur_token))
            .ok_or_else(|| {
                self.make_error(
                    format!("unsupported expression {}", self.cur_token.literal()),
                    self.cur_token_span,
                )
            })?(self)?;
        // expression end token

        while !self.peek_token.is_semicolon() &&
            !self.peek_token.is_eof() &&
            // if peek token is } or ; or eof etc..., not meeting this condition, so no problem
            (
                precedence < self.peek_token_precedence() ||
                    // if is assign, from right to left
                    (precedence == self.peek_token_precedence() && self.peek_token_precedence() == Precedence::Assign)
            )
        {
            if self.peek_token.is_lbrace() && !can_start_struct_literal(&left_exp) {
                return Ok(left_exp);
            }
            let Some(func) = self
                .infix_parse_fns
                .get(&discriminant(&self.peek_token))
                .cloned()
            else {
                return Ok(left_exp);
            };
            if self.peek_token.is_lbrace() && !self.struct_literal_allowed() {
                return Ok(left_exp);
            }
            self.next_token();
            left_exp = func(self, left_exp)?;
        }
        Ok(left_exp)
    }

    fn parse_expression_before_block(
        &mut self,
        precedence: Precedence,
    ) -> Result<Expression, Error> {
        self.with_expression_context(ExpressionContext::BeforeBlock, |parser| {
            parser.parse_expression(precedence)
        })
    }

    fn parse_nested_expression(&mut self, precedence: Precedence) -> Result<Expression, Error> {
        self.with_expression_context(ExpressionContext::Normal, |parser| {
            parser.parse_expression(precedence)
        })
    }

    fn with_expression_context<T>(
        &mut self,
        context: ExpressionContext,
        f: impl FnOnce(&mut Self) -> Result<T, Error>,
    ) -> Result<T, Error> {
        self.expression_context.push(context);
        let result = f(self);
        self.expression_context.pop();
        result
    }

    fn struct_literal_allowed(&self) -> bool {
        self.expression_context
            .last()
            .copied()
            .unwrap_or(ExpressionContext::Normal)
            == ExpressionContext::Normal
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>, Error> {
        let mut identifiers = vec![];
        if self.peek_token.is_rparen() {
            self.next_token();
            return Ok(identifiers);
        }
        self.next_token();
        if !self.cur_token.is_ident() {
            return Err(self.make_syntax_error(Token::Ident(String::default())));
        }
        let ident = Identifier {
            span: self.cur_token_span,
            name: self.cur_token.literal(),
        };
        identifiers.push(ident);
        while self.peek_token.is_comma() {
            self.next_token();
            self.next_token();
            if !self.cur_token.is_ident() {
                return Err(self.make_syntax_error(Token::Ident(String::default())));
            }
            let ident = Identifier {
                span: self.cur_token_span,
                name: self.cur_token.literal(),
            };
            identifiers.push(ident);
        }
        if !self.expect_peek(self.peek_token.is_rparen()) {
            return Err(self.make_syntax_error(Token::Rparen));
        }
        Ok(identifiers)
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, Error> {
        debug_assert!(self.cur_token.is_lparen());
        let mut args = vec![];
        if self.peek_token.is_rparen() {
            self.next_token();
            return Ok(args);
        }
        self.next_token();
        args.push(self.parse_nested_expression(Precedence::Lowest)?);
        while self.peek_token.is_comma() {
            self.next_token();
            self.next_token();
            args.push(self.parse_nested_expression(Precedence::Lowest)?);
        }
        if !self.expect_peek(self.peek_token.is_rparen()) {
            return Err(self.make_syntax_error(Token::Rparen));
        }
        Ok(args)
    }

    fn parse_identifier(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_ident());
        Ok(Expression::Identifier(Identifier {
            span: p.cur_token_span,
            name: match p.cur_token {
                Token::Ident(ref v) => v.to_owned(),
                _ => unreachable!(),
            },
        }))
    }

    fn parse_integer_literal(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_int_literal());
        Ok(Expression::Integer(IntegerLiteral {
            span: p.cur_token_span,
            value: match p.cur_token {
                Token::IntLiteral(v) => v,
                _ => unreachable!(),
            },
        }))
    }

    fn parse_float_literal(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_float_literal());
        Ok(Expression::Float(FloatLiteral {
            span: p.cur_token_span,
            value: match p.cur_token {
                Token::FloatLiteral(v) => v,
                _ => unreachable!(),
            },
        }))
    }

    fn parse_bool_literal(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_true() || p.cur_token.is_false());
        Ok(Expression::Bool(BoolLiteral {
            span: p.cur_token_span,
            value: p.cur_token.is_true(),
        }))
    }

    fn parse_null_literal(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_null());
        Ok(Expression::Null(NullLiteral {
            span: p.cur_token_span,
        }))
    }

    fn parse_string_literal(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_string_literal());
        if let Token::StringLiteral(value) = p.cur_token.to_owned() {
            return Ok(Expression::String(StringLiteral {
                span: p.cur_token_span,
                value,
            }));
        }
        unreachable!()
    }

    fn parse_prefix_expression(p: &mut Parser) -> Result<Expression, Error> {
        let token = p.cur_token.to_owned();
        let start = p.cur_token_span.start;
        p.next_token();
        let right = p.parse_expression(Precedence::Prefix)?;
        Ok(Expression::Prefix(Box::new(PrefixExpression {
            span: Span::new(start, right.span().end),
            token,
            right,
        })))
    }

    fn parse_infix_expression(p: &mut Parser, left: Expression) -> Result<Expression, Error> {
        let token = p.cur_token.to_owned();
        let precedence = p.cur_token_precedence();
        let start = left.span().start;
        p.next_token();
        let right = p.parse_expression(precedence)?;
        Ok(Expression::Infix(Box::new(InfixExpression {
            span: Span::new(start, right.span().end),
            token,
            left,
            right,
        })))
    }

    fn parse_grouped_expression(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_lparen());
        let start = p.cur_token_span.start;
        p.next_token();
        let exp = p.parse_nested_expression(Precedence::Lowest)?;
        if !p.expect_peek(p.peek_token.is_rparen()) {
            Err(p.make_syntax_error(Token::Rparen))
        } else {
            Ok(Expression::Grouped(Box::new(
                crate::ast::GroupedExpression {
                    span: Span::new(start, p.cur_token_span.end),
                    expression: exp,
                },
            )))
        }
    }

    fn parse_if_expression(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_if());
        let start = p.cur_token_span.start;
        p.next_token();
        let condition = p.parse_expression_before_block(Precedence::Lowest)?;
        if !p.expect_peek(p.peek_token.is_lbrace()) {
            return Err(p.make_syntax_error(Token::Lbrace));
        }
        let consequence = p.parse_block_statement()?;
        let mut alternative = None;
        let mut optional = None;
        if p.expect_peek(p.peek_token.is_else()) {
            if p.expect_peek(p.peek_token.is_lbrace()) {
                alternative = Some(p.parse_block_statement()?);
            } else if p.expect_peek(p.peek_token.is_if()) {
                optional = Some(Box::new(Self::parse_if_expression(p)?));
            } else {
                return Err(p.make_syntax_error(Token::Lbrace));
            }
        }
        let end = optional
            .as_ref()
            .map(|expression| expression.span().end)
            .or_else(|| alternative.as_ref().map(|block| block.span.end))
            .unwrap_or(consequence.span.end);
        Ok(Expression::If(Box::new(IfExpression {
            span: Span::new(start, end),
            condition,
            consequence,
            alternative,
            optional,
        })))
    }

    fn parse_function_expression(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_function());
        let mut name = None;
        let start = p.cur_token_span.start;
        if p.peek_token.is_ident() {
            name = Some(Identifier {
                span: p.peek_token_span,
                name: p.peek_token.literal(),
            });
            p.next_token();
        }
        if !p.expect_peek(p.peek_token.is_lparen()) {
            return Err(p.make_syntax_error(Token::Lparen));
        }
        let parameters = p.parse_function_parameters()?;
        if !p.expect_peek(p.peek_token.is_lbrace()) {
            return Err(p.make_syntax_error(Token::Lbrace));
        }
        let body = p.parse_block_statement()?;
        Ok(Expression::Function(FunctionLiteral {
            span: Span::new(start, body.span.end),
            name,
            parameters,
            body,
        }))
    }

    fn parse_call_expression(p: &mut Parser, function: Expression) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_lparen());
        let start = function.span().start;
        let arguments = p.parse_call_arguments()?;
        Ok(Expression::Call(Box::new(CallExpression {
            span: Span::new(start, p.cur_token_span.end),
            function,
            arguments,
        })))
    }

    fn parse_slice_literal(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_lbracket());
        let mut elements = Vec::new();
        let start = p.cur_token_span.start;
        let mut is_first_loop = true;
        while !p.peek_token.is_rbracket() && !p.peek_token.is_eof() {
            if !is_first_loop && !p.expect_peek(p.peek_token.is_comma()) {
                return Err(p.make_syntax_error(Token::Comma));
            }
            p.next_token(); // skip [ or ,
            elements.push(p.parse_nested_expression(Precedence::Lowest)?);
            is_first_loop = false;
        }
        if !p.expect_peek(p.peek_token.is_rbracket()) {
            return Err(p.make_syntax_error(Token::Rbracket));
        }
        Ok(Expression::Slice(SliceLiteral {
            span: Span::new(start, p.cur_token_span.end),
            elements,
        }))
    }

    fn parse_map_literal(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_lbrace());
        let mut kv_pair = Vec::new();
        let start = p.cur_token_span.start;
        let mut is_first_loop = true;
        while !p.peek_token.is_rbrace() && !p.peek_token.is_eof() {
            if !is_first_loop && !p.expect_peek(p.peek_token.is_comma()) {
                return Err(p.make_syntax_error(Token::Comma));
            }
            p.next_token(); // skip { or ,
            let k = p.parse_map_key()?;
            if !p.expect_peek(p.peek_token.is_colon()) {
                return Err(p.make_syntax_error(Token::Colon));
            }
            p.next_token(); // skip :
            kv_pair.push((k, p.parse_nested_expression(Precedence::Lowest)?));
            is_first_loop = false;
        }
        if !p.expect_peek(p.peek_token.is_rbrace()) {
            return Err(p.make_syntax_error(Token::Rbrace));
        }
        Ok(Expression::Map(MapLiteral {
            span: Span::new(start, p.cur_token_span.end),
            kv_pair,
        }))
    }

    fn parse_index_expression(p: &mut Parser, left: Expression) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_lbracket());
        let start = left.span().start;
        p.next_token();
        let index = p.parse_nested_expression(Precedence::Lowest)?;
        if !p.expect_peek(p.peek_token.is_rbracket()) {
            return Err(p.make_syntax_error(Token::Rbracket));
        }
        Ok(Expression::Index(Box::new(IndexExpression {
            span: Span::new(start, p.cur_token_span.end),
            left,
            index,
        })))
    }

    fn parse_map_key(&mut self) -> Result<Expression, Error> {
        if let Token::Ident(name) = self.cur_token.to_owned() {
            if self.peek_token.is_colon() {
                return Ok(Expression::String(StringLiteral {
                    span: self.cur_token_span,
                    value: name,
                }));
            }
        }
        self.parse_nested_expression(Precedence::Lowest)
    }

    fn parse_member_expression(p: &mut Parser, left: Expression) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_dot());
        let start = left.span().start;
        if !p.expect_peek(p.peek_token.is_ident()) {
            return Err(p.make_syntax_error(Token::Ident(String::default())));
        }
        let property = Identifier {
            span: p.cur_token_span,
            name: p.cur_token.literal(),
        };
        Ok(Expression::Member(Box::new(MemberExpression {
            span: Span::new(start, property.span.end),
            left,
            property,
        })))
    }

    fn parse_struct_literal(p: &mut Parser, left: Expression) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_lbrace());
        let name = match left {
            Expression::Identifier(_) | Expression::Member(_) => left,
            _ => return Err(p.make_syntax_error(Token::Ident(String::default()))),
        };
        let start = name.span().start;
        let mut fields = Vec::new();
        let mut values = Vec::new();
        let mut literal_style = None;
        let mut is_first_loop = true;
        while !p.peek_token.is_rbrace() && !p.peek_token.is_eof() {
            if !is_first_loop && !p.expect_peek(p.peek_token.is_comma()) {
                return Err(p.make_syntax_error(Token::Comma));
            }
            p.next_token();

            let is_named_field = p.cur_token.is_ident() && p.peek_token.is_colon();
            match (literal_style, is_named_field) {
                (None, true) => literal_style = Some(StructLiteralStyle::Named),
                (None, false) => literal_style = Some(StructLiteralStyle::Positional),
                (Some(StructLiteralStyle::Named), false) => {
                    return Err(p.make_syntax_error(Token::Ident(String::default())));
                }
                (Some(StructLiteralStyle::Positional), true) => {
                    return Err(p.make_syntax_error(Token::Comma));
                }
                _ => {}
            }

            if is_named_field {
                let field = Identifier {
                    span: p.cur_token_span,
                    name: p.cur_token.literal(),
                };
                p.next_token(); // skip field name
                p.next_token(); // skip :
                let value = p.parse_nested_expression(Precedence::Lowest)?;
                fields.push((field, value));
            } else {
                values.push(p.parse_nested_expression(Precedence::Lowest)?);
            }
            is_first_loop = false;
        }
        if !p.expect_peek(p.peek_token.is_rbrace()) {
            return Err(p.make_syntax_error(Token::Rbrace));
        }
        Ok(Expression::StructLiteral(StructLiteral {
            span: Span::new(start, p.cur_token_span.end),
            name: Box::new(name),
            fields,
            values,
        }))
    }

    fn make_syntax_error(&self, expect_token: Token) -> Error {
        self.make_error(
            format!(
                "syntax error expected next token to be {}, got {} instead",
                expect_token.identifier(),
                self.peek_token.identifier()
            ),
            self.peek_token_span,
        )
    }

    fn make_error(&self, msg: String, span: Span) -> Error {
        make_error_with_source_span(ErrorKind::Syntax, msg, self.path.to_owned(), span)
    }
}

fn can_start_struct_literal(expression: &Expression) -> bool {
    matches!(
        expression,
        Expression::Identifier(_) | Expression::Member(_)
    )
}

type PrefixParseFn = fn(&mut Parser) -> Result<Expression, Error>;
type InfixParseFn = fn(&mut Parser, Expression) -> Result<Expression, Error>;
