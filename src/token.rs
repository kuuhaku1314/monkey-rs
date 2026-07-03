use crate::error::Error;
use std::collections::HashMap;
use std::sync::LazyLock;
use strum::EnumIs;

#[derive(Clone, PartialEq, Debug, EnumIs)]
pub enum Token {
    /// invalid token(token,error_message)
    Illegal(String, Error),
    Eof,
    /// foo
    Ident(String),
    /// 1
    IntLiteral(i64),
    /// 1.1
    FloatLiteral(f64),
    /// "apple"
    StringLiteral(String),
    /// =
    Assign,
    /// +
    Plus,
    /// -
    Minus,
    /// !
    Bang,
    /// *
    Asterisk,
    /// /
    Slash,
    /// <
    LT,
    /// >
    GT,
    /// ==
    EQ,
    /// !=
    NotEq,
    /// &&
    And,
    /// ||
    Or,
    /// >=
    Gte,
    /// <=
    Lte,
    /// ,
    Comma,
    /// .
    Dot,
    /// ;
    Semicolon,
    /// (
    Lparen,
    /// )
    Rparen,
    /// {
    Lbrace,
    /// }
    Rbrace,
    /// [
    Lbracket,
    /// ]
    Rbracket,
    /// :
    Colon,
    /// fn
    Function,
    /// let
    Let,
    /// true
    True,
    /// false
    False,
    /// null
    Null,
    /// if
    IF,
    /// else
    Else,
    /// return
    Return,
    /// import
    Import,
    /// from
    From,
    /// export
    Export,
    /// struct
    Struct,
    /// while
    While,
    /// for
    For,
    /// in
    IN,
    /// break
    Break,
    /// continue
    Continue,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest = 0,
    Assign = 1,
    LogicOr = 2,
    LogicAnd = 3,
    Equals = 4,
    Compare = 5,
    Sum = 6,
    Product = 7,
    Prefix = 8,
    Call = 9,
    Index = 10,
}

impl Precedence {
    pub fn next(self) -> Self {
        match self {
            Precedence::Lowest => Precedence::Assign,
            Precedence::Assign => Precedence::LogicOr,
            Precedence::LogicOr => Precedence::LogicAnd,
            Precedence::LogicAnd => Precedence::Equals,
            Precedence::Equals => Precedence::Compare,
            Precedence::Compare => Precedence::Sum,
            Precedence::Sum => Precedence::Product,
            Precedence::Product => Precedence::Prefix,
            Precedence::Prefix => Precedence::Call,
            Precedence::Call => Precedence::Index,
            Precedence::Index => Precedence::Index,
        }
    }
}

impl Token {
    pub fn precedence(&self) -> Precedence {
        match self {
            Token::Assign => Precedence::Assign,
            Token::Or => Precedence::LogicOr,
            Token::And => Precedence::LogicAnd,
            Token::EQ | Token::NotEq => Precedence::Equals,
            Token::LT | Token::Lte | Token::GT | Token::Gte => Precedence::Compare,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            Token::Lparen | Token::Lbrace => Precedence::Call,
            Token::Lbracket | Token::Dot => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }

    pub fn identifier(&self) -> &'static str {
        match self {
            Token::Illegal(_, _) => "illegal",
            Token::Eof => "eof",
            Token::Ident(_) => "ident",
            Token::IntLiteral(_) => "int-literal",
            Token::FloatLiteral(_) => "float-literal",
            Token::StringLiteral(_) => "string-literal",
            Token::Assign => "=",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Bang => "!",
            Token::Asterisk => "*",
            Token::Slash => "/",
            Token::LT => "<",
            Token::GT => ">",
            Token::EQ => "==",
            Token::NotEq => "!=",
            Token::And => "&&",
            Token::Or => "||",
            Token::Gte => ">=",
            Token::Lte => "<=",
            Token::Comma => ",",
            Token::Dot => ".",
            Token::Semicolon => ";",
            Token::Lparen => "(",
            Token::Rparen => ")",
            Token::Lbrace => "{",
            Token::Rbrace => "}",
            Token::Lbracket => "[",
            Token::Rbracket => "]",
            Token::Colon => ":",
            Token::Function => "fn",
            Token::Let => "let",
            Token::True => "true",
            Token::False => "false",
            Token::Null => "null",
            Token::IF => "if",
            Token::Else => "else",
            Token::Return => "return",
            Token::Import => "import",
            Token::From => "from",
            Token::Export => "export",
            Token::Struct => "struct",
            Token::While => "while",
            Token::For => "for",
            Token::IN => "in",
            Token::Break => "break",
            Token::Continue => "continue",
        }
    }

    pub fn literal(&self) -> String {
        String::from(match self {
            // valuable
            Token::Illegal(v, _) => return v.to_owned(),
            Token::Eof => "\0",
            Token::Ident(v) => return v.to_owned(),
            Token::IntLiteral(v) => return v.to_string(),
            Token::FloatLiteral(v) => return v.to_string(),
            Token::StringLiteral(v) => return format!("\"{v}\""),
            // symbol
            Token::Assign => "=",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Bang => "!",
            Token::Asterisk => "*",
            Token::Slash => "/",
            Token::LT => "<",
            Token::GT => ">",
            Token::EQ => "==",
            Token::NotEq => "!=",
            Token::And => "&&",
            Token::Or => "||",
            Token::Gte => ">=",
            Token::Lte => "<=",
            Token::Comma => ",",
            Token::Dot => ".",
            Token::Semicolon => ";",
            Token::Lparen => "(",
            Token::Rparen => ")",
            Token::Lbrace => "{",
            Token::Rbrace => "}",
            Token::Lbracket => "[",
            Token::Rbracket => "]",
            Token::Colon => ":",
            // keyword
            Token::Function => "fn",
            Token::Let => "let",
            Token::True => "true",
            Token::False => "false",
            Token::Null => "null",
            Token::IF => "if",
            Token::Else => "else",
            Token::Return => "return",
            Token::Import => "import",
            Token::From => "from",
            Token::Export => "export",
            Token::Struct => "struct",
            Token::While => "while",
            Token::For => "for",
            Token::IN => "in",
            Token::Break => "break",
            Token::Continue => "continue",
        })
    }
}

pub static KEY_WORDS_TABLE: LazyLock<HashMap<&'static str, Token>> = LazyLock::new(key_words_table);

fn key_words_table() -> HashMap<&'static str, Token> {
    let mut set = HashMap::new();
    let key_word_tokens = vec![
        Token::Function,
        Token::Let,
        Token::True,
        Token::False,
        Token::Null,
        Token::IF,
        Token::Else,
        Token::Return,
        Token::Import,
        Token::From,
        Token::Export,
        Token::Struct,
        Token::Let,
        Token::While,
        Token::For,
        Token::IN,
        Token::Break,
        Token::Continue,
    ];
    for x in key_word_tokens {
        set.insert(x.identifier(), x);
    }
    set
}
