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
    /// >=
    Gte,
    /// <=
    Lte,
    /// ,
    Comma,
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
    /// if
    IF,
    /// else
    Else,
    /// return
    Return,
    /// struct
    Struct,
    /// while
    While,
    /// for
    For,
    /// in
    IN,
}

impl Token {
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
            Token::Gte => ">=",
            Token::Lte => "<=",
            Token::Comma => ",",
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
            Token::IF => "if",
            Token::Else => "else",
            Token::Return => "return",
            Token::Struct => "struct",
            Token::While => "while",
            Token::For => "for",
            Token::IN => "in",
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
            Token::Gte => ">=",
            Token::Lte => "<=",
            Token::Comma => ",",
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
            Token::IF => "if",
            Token::Else => "else",
            Token::Return => "return",
            Token::Struct => "struct",
            Token::While => "while",
            Token::For => "for",
            Token::IN => "in",
        })
    }
}

pub static KEY_WORDS_TABLE: LazyLock<HashMap<&'static str, Token>> =
    LazyLock::new(key_words_table);

fn key_words_table() -> HashMap<&'static str, Token> {
    let mut set = HashMap::new();
    let key_word_tokens = vec![
        Token::Function,
        Token::Let,
        Token::True,
        Token::False,
        Token::IF,
        Token::Else,
        Token::Return,
        Token::Struct,
        Token::Let,
        Token::While,
        Token::For,
        Token::IN,
    ];
    for x in key_word_tokens {
        set.insert(x.identifier(), x);
    }
    set
}
