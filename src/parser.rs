use crate::ast::{
    BlockStatement, BoolLiteral, CallExpression, EmptyStatement, Expression, ExpressionStatement,
    FloatLiteral, ForInStatement, FunctionLiteral, Identifier, IfExpression, IndexAssignStatement,
    IndexExpression, InfixExpression, IntegerLiteral, LetStatement, MapLiteral, PrefixExpression,
    Program, ReturnStatement, SliceLiteral, Statement, StringLiteral, WhileStatement,
};
use crate::error::{make_error, Error};
use crate::lexer::{Lexer, Position};
use crate::token::Token;
use std::collections::HashMap;
use std::mem::discriminant;
use std::mem::Discriminant;

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    path: Option<String>,
    cur_token_position: Position,
    peek_token_position: Position,
    prefix_parse_fns: HashMap<Discriminant<Token>, PrefixParseFn>,
    infix_parse_fns: HashMap<Discriminant<Token>, InfixParseFn>,
    precedences_map: HashMap<Discriminant<Token>, OperationPriority>,
}

pub fn new_parser(lexer: Lexer) -> Parser {
    let path = lexer.file_path().to_owned();
    let mut parser = Parser {
        lexer,
        cur_token: Token::Eof,
        peek_token: Token::Eof,
        path,
        cur_token_position: Position(0, 0),
        peek_token_position: Position(0, 0),
        prefix_parse_fns: HashMap::new(),
        infix_parse_fns: HashMap::new(),
        precedences_map: token_precedences_map(),
    };
    // For expressions, the current token is the current prefix/infix token when calling the function
    // After the function returns, the current token is the last token in the complete expression

    // prefix
    parser.register_prefix(
        discriminant(&Token::Ident(String::default())),
        Parser::parse_identifier,
    );
    parser.register_prefix(
        discriminant(&Token::IntLiteral(i64::default())),
        Parser::parse_integer_literal,
    );
    parser.register_prefix(
        discriminant(&Token::FloatLiteral(f64::default())),
        Parser::parse_float_literal,
    );
    parser.register_prefix(discriminant(&Token::True), Parser::parse_bool_literal);
    parser.register_prefix(discriminant(&Token::False), Parser::parse_bool_literal);
    parser.register_prefix(
        discriminant(&Token::StringLiteral(String::default())),
        Parser::parse_string_literal,
    );
    parser.register_prefix(discriminant(&Token::Bang), Parser::parse_prefix_expression);
    parser.register_prefix(discriminant(&Token::Minus), Parser::parse_prefix_expression);
    parser.register_prefix(
        discriminant(&Token::Lparen),
        Parser::parse_grouped_expression,
    );
    parser.register_prefix(discriminant(&Token::IF), Parser::parse_if_expression);
    parser.register_prefix(
        discriminant(&Token::Function),
        Parser::parse_function_expression,
    );
    parser.register_prefix(discriminant(&Token::Lbrace), Parser::parse_map_literal);
    parser.register_prefix(discriminant(&Token::Lbracket), Parser::parse_slice_literal);
    // infix
    parser.register_infix(discriminant(&Token::EQ), Parser::parse_infix_expression);
    parser.register_infix(discriminant(&Token::NotEq), Parser::parse_infix_expression);
    parser.register_infix(discriminant(&Token::LT), Parser::parse_infix_expression);
    parser.register_infix(discriminant(&Token::Lte), Parser::parse_infix_expression);
    parser.register_infix(discriminant(&Token::GT), Parser::parse_infix_expression);
    parser.register_infix(discriminant(&Token::Gte), Parser::parse_infix_expression);
    parser.register_infix(discriminant(&Token::Plus), Parser::parse_infix_expression);
    parser.register_infix(discriminant(&Token::Minus), Parser::parse_infix_expression);
    parser.register_infix(discriminant(&Token::Slash), Parser::parse_infix_expression);
    parser.register_infix(
        discriminant(&Token::Asterisk),
        Parser::parse_infix_expression,
    );
    parser.register_infix(discriminant(&Token::Assign), Parser::parse_infix_expression);
    parser.register_infix(discriminant(&Token::Lparen), Parser::parse_call_expression);
    parser.register_infix(
        discriminant(&Token::Lbracket),
        Parser::parse_index_expression,
    );
    // prefetch
    parser.next_token();
    parser.next_token();
    parser
}

fn token_precedences_map() -> HashMap<Discriminant<Token>, OperationPriority> {
    let mut map = HashMap::new();
    map.insert(discriminant(&Token::EQ), OperationPriority::Equals);
    map.insert(discriminant(&Token::NotEq), OperationPriority::Equals);
    map.insert(discriminant(&Token::LT), OperationPriority::Compare);
    map.insert(discriminant(&Token::Lte), OperationPriority::Compare);
    map.insert(discriminant(&Token::GT), OperationPriority::Compare);
    map.insert(discriminant(&Token::Gte), OperationPriority::Compare);
    map.insert(discriminant(&Token::Plus), OperationPriority::Sum);
    map.insert(discriminant(&Token::Minus), OperationPriority::Sum);
    map.insert(discriminant(&Token::Slash), OperationPriority::Product);
    map.insert(discriminant(&Token::Asterisk), OperationPriority::Product);
    map.insert(discriminant(&Token::Lparen), OperationPriority::Call);
    map.insert(discriminant(&Token::Assign), OperationPriority::Assign);
    map.insert(discriminant(&Token::Lbracket), OperationPriority::Index);
    map
}

#[derive(Clone, Debug, PartialEq)]
pub enum OperationPriority {
    Lowest,
    // =
    Assign,
    // ==
    Equals,
    /// >, <, <=, >=
    Compare,
    /// +
    Sum,
    /// *，/
    Product,
    /// -x, !
    Prefix,
    /// // myFunction(X)
    Call,
    /// slice[1]
    Index,
}

impl Parser {
    fn register_prefix(&mut self, token: Discriminant<Token>, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token, func);
    }

    fn register_infix(&mut self, token: Discriminant<Token>, func: InfixParseFn) {
        self.infix_parse_fns.insert(token, func);
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.to_owned();
        self.cur_token_position = self.peek_token_position.to_owned();
        self.peek_token = self.lexer.next_token();
        let mut position = self.lexer.line_char_at();
        // char index is token end + 1, token start = token length - 1
        position.1 -= self.peek_token.literal().len();
        self.peek_token_position = position.into();
    }

    fn expect_peek(&mut self, ok: bool) -> bool {
        if ok {
            self.next_token();
        }
        ok
    }

    fn cur_token_precedence(&self) -> OperationPriority {
        self.precedences_map
            .get(&discriminant(&self.cur_token))
            .cloned()
            .unwrap_or(OperationPriority::Lowest)
    }

    fn peek_token_precedence(&self) -> OperationPriority {
        self.precedences_map
            .get(&discriminant(&self.peek_token))
            .cloned()
            .unwrap_or(OperationPriority::Lowest)
    }

    pub fn parse_program(&mut self) -> Result<Program, Error> {
        let mut program = Program { statements: vec![] };
        while self.cur_token != Token::Eof {
            if self.cur_token == Token::Semicolon {
                program.statements.push(Statement::Empty(EmptyStatement {
                    position: self.cur_token_position.to_owned(),
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
            Token::Lbrace => Ok(Statement::Block(self.parse_block_statement()?)),
            Token::While => Ok(Statement::While(self.parse_while_statement()?)),
            Token::For => Ok(Statement::ForIn(self.parse_for_in_statement()?)),
            _ => {
                if self.cur_token.is_ident() && self.peek_token.is_lbracket() {
                    return Ok(Statement::IndexAssign(self.parse_index_assign_statement()?));
                }
                let stmt = self.parse_expression_statement()?;
                // block statement last statement can not semicolon
                // but cannot determine if this is the last statement here
                // so need to hand this over to the block statement for inspection
                if !in_block
                    && self.must_end_of_semicolon(&stmt.expression)
                    && !stmt.end_of_semicolon
                {
                    return Err(self.make_syntax_error(Token::Semicolon));
                }
                Ok(Statement::Expression(stmt))
            }
        }
    }

    fn must_end_of_semicolon(&self, expression: &Expression) -> bool {
        !matches!(expression, Expression::Function(_) | Expression::If(_))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, Error> {
        debug_assert!(self.cur_token.is_lbrace());
        let mut block = BlockStatement {
            position: self.cur_token_position.to_owned(),
            statements: vec![],
        };
        self.next_token();
        while !self.cur_token.is_rbrace() && !self.cur_token.is_eof() {
            if self.cur_token.is_semicolon() {
                block.statements.push(Statement::Empty(EmptyStatement {
                    position: self.cur_token_position.to_owned(),
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
            return Err(make_error(format!(
                "unclosed block at {}",
                self.cur_token_position_msg()
            )));
        }
        Ok(block)
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, Error> {
        debug_assert!(self.cur_token.is_let());
        let position = self.cur_token_position.to_owned();
        if !self.expect_peek(self.peek_token.is_ident()) {
            return Err(self.make_syntax_error(Token::Ident(String::default())));
        }
        let name = Identifier {
            position: self.cur_token_position.to_owned(),
            token: self.cur_token.to_owned(),
            name: self.cur_token.literal(),
        };
        if !self.expect_peek(self.peek_token.is_assign()) {
            return Err(self.make_syntax_error(Token::Assign));
        }
        self.next_token();
        let stmt = LetStatement {
            position,
            name,
            value: self.parse_expression(OperationPriority::Lowest)?,
        };
        if !self.expect_peek(self.peek_token.is_semicolon()) {
            return Err(self.make_syntax_error(Token::Semicolon));
        }
        Ok(stmt)
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, Error> {
        debug_assert!(self.cur_token.is_return());
        let position = self.cur_token_position.to_owned();
        self.next_token();
        let stmt = ReturnStatement {
            position,
            value: self.parse_expression(OperationPriority::Lowest)?,
        };
        if !self.expect_peek(self.peek_token.is_semicolon()) {
            return Err(self.make_syntax_error(Token::Semicolon));
        }
        Ok(stmt)
    }

    fn parse_while_statement(&mut self) -> Result<WhileStatement, Error> {
        debug_assert!(self.cur_token.is_while());
        let position = self.cur_token_position.to_owned();
        self.next_token();
        let condition = self.parse_expression(OperationPriority::Lowest)?;
        if !self.expect_peek(self.peek_token.is_lbrace()) {
            return Err(self.make_syntax_error(Token::Lbrace));
        }
        let consequence = self.parse_block_statement()?;
        Ok(WhileStatement {
            position,
            condition,
            consequence,
        })
    }

    fn parse_for_in_statement(&mut self) -> Result<ForInStatement, Error> {
        debug_assert!(self.cur_token.is_for());
        let position = self.cur_token_position.to_owned();
        self.next_token();
        let key = match self.parse_expression(OperationPriority::Lowest)? {
            Expression::Identifier(ident) => ident,
            _ => {
                return Err(make_error(format!(
                    "should be variable at {}",
                    self.cur_token_position_msg()
                )))
            }
        };
        let mut value = None;
        if self.expect_peek(self.peek_token.is_comma()) {
            self.next_token();
            value = Some(match self.parse_expression(OperationPriority::Lowest)? {
                Expression::Identifier(ident) => ident,
                _ => {
                    return Err(make_error(format!(
                        "should be variable at {}",
                        self.cur_token_position_msg()
                    )))
                }
            })
        }
        if !self.expect_peek(self.peek_token.is_in()) {
            return Err(self.make_syntax_error(Token::IN));
        }
        self.next_token();
        let collection = self.parse_expression(OperationPriority::Lowest)?;
        if !self.expect_peek(self.peek_token.is_lbrace()) {
            return Err(self.make_syntax_error(Token::Lbrace));
        }
        Ok(ForInStatement {
            position,
            collection,
            key,
            value,
            body: self.parse_block_statement()?,
        })
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, Error> {
        let stmt = ExpressionStatement {
            position: self.cur_token_position.to_owned(),
            expression: self.parse_expression(OperationPriority::Lowest)?,
            end_of_semicolon: self.expect_peek(self.peek_token.is_semicolon()),
        };
        Ok(stmt)
    }

    fn parse_index_assign_statement(&mut self) -> Result<IndexAssignStatement, Error> {
        debug_assert!(self.cur_token.is_ident());
        let position = self.cur_token_position.to_owned();
        // priority should be >= '['
        let left = self.parse_expression(OperationPriority::Index)?;
        self.next_token();
        debug_assert!(self.cur_token.is_lbracket());
        self.next_token();
        let index = self.parse_expression(OperationPriority::Lowest)?;
        if !self.expect_peek(self.peek_token.is_rbracket()) {
            return Err(self.make_syntax_error(Token::Rbracket));
        }
        if !self.expect_peek(self.peek_token.is_assign()) {
            return Err(self.make_syntax_error(Token::Assign));
        }
        self.next_token();
        let right = self.parse_expression(OperationPriority::Lowest)?;
        Ok(IndexAssignStatement {
            position,
            left,
            index,
            right,
        })
    }

    fn parse_expression(&mut self, precedence: OperationPriority) -> Result<Expression, Error> {
        let mut left_exp = self
            .prefix_parse_fns
            .get(&discriminant(&self.cur_token))
            .ok_or_else(|| {
                make_error(format!(
                    "unsupported expression {} at {}",
                    self.cur_token.literal(),
                    self.cur_token_position_msg()
                ))
            })?(self)?;
        // expression end token

        while !self.peek_token.is_semicolon() &&
            !self.peek_token.is_eof() &&
            // if peek token is } or ; or eof etc..., not meeting this condition, so no problem
            (
                (precedence.to_owned() as u8) < self.peek_token_precedence() as u8 ||
                // if is assign, from right to left
                (precedence == self.peek_token_precedence() && self.peek_token_precedence() == OperationPriority::Assign)
            )
        {
            let func = self
                .infix_parse_fns
                .get(&discriminant(&self.peek_token))
                .cloned();
            if func.is_none() {
                return Ok(left_exp);
            }
            self.next_token();
            left_exp = func.unwrap()(self, left_exp)?;
        }
        Ok(left_exp)
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
            position: self.cur_token_position.to_owned(),
            token: self.cur_token.to_owned(),
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
                position: self.cur_token_position.to_owned(),
                token: self.cur_token.to_owned(),
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
        args.push(self.parse_expression(OperationPriority::Lowest)?);
        while self.peek_token.is_comma() {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(OperationPriority::Lowest)?);
        }
        if !self.expect_peek(self.peek_token.is_rparen()) {
            return Err(self.make_syntax_error(Token::Rparen));
        }
        Ok(args)
    }

    pub fn cur_token_position_msg(&self) -> String {
        match self.path {
            None => {
                format!(
                    "{}:{}",
                    self.cur_token_position.0, self.cur_token_position.1
                )
            }
            Some(ref path) => {
                format!(
                    "{}{}{}:{}",
                    path,
                    std::path::MAIN_SEPARATOR,
                    self.cur_token_position.0,
                    self.cur_token_position.1
                )
            }
        }
    }

    pub fn peek_token_position_msg(&self) -> String {
        match self.path {
            None => {
                format!(
                    "{}:{}",
                    self.peek_token_position.0, self.peek_token_position.1
                )
            }
            Some(ref path) => {
                format!(
                    "{}/{}:{}",
                    path, self.peek_token_position.0, self.peek_token_position.1
                )
            }
        }
    }

    fn parse_identifier(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_ident());
        Ok(Expression::Identifier(Identifier {
            position: p.cur_token_position.to_owned(),
            token: p.cur_token.to_owned(),
            name: match p.cur_token {
                Token::Ident(ref v) => v.to_owned(),
                _ => unreachable!(),
            },
        }))
    }

    fn parse_integer_literal(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_int_literal());
        Ok(Expression::Integer(IntegerLiteral {
            position: p.cur_token_position.to_owned(),
            token: p.cur_token.to_owned(),
            value: match p.cur_token {
                Token::IntLiteral(v) => v,
                _ => unreachable!(),
            },
        }))
    }

    fn parse_float_literal(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_float_literal());
        Ok(Expression::Float(FloatLiteral {
            position: p.cur_token_position.to_owned(),
            token: p.cur_token.to_owned(),
            value: match p.cur_token {
                Token::FloatLiteral(v) => v,
                _ => unreachable!(),
            },
        }))
    }

    fn parse_bool_literal(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_true() || p.cur_token.is_false());
        Ok(Expression::Bool(BoolLiteral {
            position: p.cur_token_position.to_owned(),
            token: p.cur_token.to_owned(),
            value: p.cur_token.is_true(),
        }))
    }

    fn parse_string_literal(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_string_literal());
        if let Token::StringLiteral(value) = p.cur_token.to_owned() {
            return Ok(Expression::String(StringLiteral {
                position: p.cur_token_position.to_owned(),
                token: p.cur_token.to_owned(),
                value,
            }));
        }
        unreachable!()
    }

    fn parse_prefix_expression(p: &mut Parser) -> Result<Expression, Error> {
        let token = p.cur_token.to_owned();
        let position = p.cur_token_position.to_owned();
        p.next_token();
        Ok(Expression::Prefix(Box::new(PrefixExpression {
            position,
            token,
            right: p.parse_expression(OperationPriority::Prefix)?,
        })))
    }

    fn parse_infix_expression(p: &mut Parser, left: Expression) -> Result<Expression, Error> {
        let token = p.cur_token.to_owned();
        let precedence = p.cur_token_precedence();
        let position = p.cur_token_position.to_owned();
        p.next_token();
        Ok(Expression::Infix(Box::new(InfixExpression {
            position,
            token,
            left,
            right: p.parse_expression(precedence)?,
        })))
    }

    fn parse_grouped_expression(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_lparen());
        p.next_token();
        let exp = p.parse_expression(OperationPriority::Lowest)?;
        if !p.expect_peek(p.peek_token.is_rparen()) {
            Err(p.make_syntax_error(Token::Rparen))
        } else {
            Ok(exp)
        }
    }

    fn parse_if_expression(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_if());
        let position = p.cur_token_position.to_owned();
        p.next_token();
        let condition = p.parse_expression(OperationPriority::Lowest)?;
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
        Ok(Expression::If(Box::new(IfExpression {
            position,
            condition,
            consequence,
            alternative,
            optional,
        })))
    }

    fn parse_function_expression(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_function());
        let mut name = None;
        let position = p.cur_token_position.to_owned();
        if p.peek_token.is_ident() {
            name = Some(Identifier {
                position: p.peek_token_position.to_owned(),
                token: p.peek_token.to_owned(),
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
            position,
            name,
            parameters,
            body,
        }))
    }

    fn parse_call_expression(p: &mut Parser, function: Expression) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_lparen());
        Ok(Expression::Call(Box::new(CallExpression {
            position: p.cur_token_position.to_owned(),
            function,
            arguments: p.parse_call_arguments()?,
        })))
    }

    fn parse_slice_literal(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_lbracket());
        let mut elements = Vec::new();
        let position = p.cur_token_position.to_owned();
        let mut is_first_loop = true;
        while !p.peek_token.is_rbracket() && !p.peek_token.is_eof() {
            if !is_first_loop && !p.expect_peek(p.peek_token.is_comma()) {
                return Err(p.make_syntax_error(Token::Comma));
            }
            p.next_token(); // skip [ or ,
            elements.push(p.parse_expression(OperationPriority::Lowest)?);
            is_first_loop = false;
        }
        if !p.expect_peek(p.peek_token.is_rbracket()) {
            return Err(p.make_syntax_error(Token::Rbracket));
        }
        Ok(Expression::Slice(SliceLiteral { position, elements }))
    }

    fn parse_map_literal(p: &mut Parser) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_lbrace());
        let mut kv_pair = Vec::new();
        let position = p.cur_token_position.to_owned();
        let mut is_first_loop = true;
        while !p.peek_token.is_rbrace() && !p.peek_token.is_eof() {
            if !is_first_loop && !p.expect_peek(p.peek_token.is_comma()) {
                return Err(p.make_syntax_error(Token::Comma));
            }
            p.next_token(); // skip { or ,
            let k = p.parse_expression(OperationPriority::Lowest)?;
            if !p.expect_peek(p.peek_token.is_colon()) {
                return Err(p.make_syntax_error(Token::Colon));
            }
            p.next_token(); // skip :
            kv_pair.push((k, p.parse_expression(OperationPriority::Lowest)?));
            is_first_loop = false;
        }
        if !p.expect_peek(p.peek_token.is_rbrace()) {
            return Err(p.make_syntax_error(Token::Rbrace));
        }
        Ok(Expression::Map(MapLiteral { position, kv_pair }))
    }

    fn parse_index_expression(p: &mut Parser, left: Expression) -> Result<Expression, Error> {
        debug_assert!(p.cur_token.is_lbracket());
        let position = p.cur_token_position.to_owned();
        p.next_token();
        let index = p.parse_expression(OperationPriority::Index)?;
        if !p.expect_peek(p.peek_token.is_rbracket()) {
            return Err(p.make_syntax_error(Token::Rbracket));
        }
        Ok(Expression::Index(Box::new(IndexExpression {
            position,
            left,
            index,
        })))
    }

    fn make_syntax_error(&self, expect_token: Token) -> Error {
        make_error(format!(
            "syntax error expected next token to be {}, got {} instead at {}",
            expect_token.identifier(),
            self.peek_token.identifier(),
            self.peek_token_position_msg()
        ))
    }
}

type PrefixParseFn = fn(&mut Parser) -> Result<Expression, Error>;
type InfixParseFn = fn(&mut Parser, Expression) -> Result<Expression, Error>;

#[cfg(test)]
mod tests {
    use crate::lexer::new_lexer;
    use crate::parser::new_parser;

    #[test]
    fn test_parser() {
        let input = String::from("let apple_number = 3;\n\n// test note! \n  let fruit\n =\t  3  ; let name = \"\\n\\\"小王\" ;// ");
        let mut p = new_parser(new_lexer(input, None));
        println!("{}", p.parse_program().unwrap().string());
        let input = String::from("add((((a + b) + ((c * d) / f)) + g)) == if a {1};");
        let mut p = new_parser(new_lexer(input, None));
        println!("{}", p.parse_program().unwrap().string());
        let input = String::from("let a = 1; a= 2;");
        let mut p = new_parser(new_lexer(input, None));
        println!("{}", p.parse_program().unwrap().string());
    }
}
