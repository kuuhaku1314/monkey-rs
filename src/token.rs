use crate::error::Error;
use std::collections::HashMap;
use std::sync::LazyLock;
use strum::EnumIs;

#[derive(Clone, PartialEq, Debug, EnumIs)]
pub enum Token {
    /// invalid token(token,error_message)
    ILLEGAL(String, Error),
    EOF,
    /// foo
    IDENT(String),
    /// 1
    IntLiteral(i64),
    /// 1.1
    FloatLiteral(f64),
    /// "apple"
    StringLiteral(String),
    /// =
    ASSIGN,
    /// +
    PLUS,
    /// -
    MINUS,
    /// !
    BANG,
    /// *
    ASTERISK,
    /// /
    SLASH,
    /// <
    LT,
    /// >
    GT,
    /// ==
    EQ,
    /// !=
    NotEq,
    /// >=
    GTE,
    /// <=
    LTE,
    /// ,
    COMMA,
    /// ;
    SEMICOLON,
    /// (
    LPAREN,
    /// )
    RPAREN,
    /// {
    LBRACE,
    /// }
    RBRACE,
    /// [
    LBRACKET,
    /// ]
    RBRACKET,
    /// :
    COLON,
    /// fn
    FUNCTION,
    /// let
    LET,
    /// true
    TRUE,
    /// false
    FALSE,
    /// if
    IF,
    /// else
    ELSE,
    /// return
    RETURN,
    /// struct
    STRUCT,
    /// while
    WHILE,
    /// for
    FOR,
    /// in
    IN,
}

impl Token {
    pub fn identifier(&self) -> &'static str {
        match self {
            Token::ILLEGAL(_, _) => "illegal",
            Token::EOF => "eof",
            Token::IDENT(_) => "ident",
            Token::IntLiteral(_) => "int-literal",
            Token::FloatLiteral(_) => "float-literal",
            Token::StringLiteral(_) => "string-literal",
            Token::ASSIGN => "=",
            Token::PLUS => "+",
            Token::MINUS => "-",
            Token::BANG => "!",
            Token::ASTERISK => "*",
            Token::SLASH => "/",
            Token::LT => "<",
            Token::GT => ">",
            Token::EQ => "==",
            Token::NotEq => "!=",
            Token::GTE => ">=",
            Token::LTE => "<=",
            Token::COMMA => ",",
            Token::SEMICOLON => ";",
            Token::LPAREN => "(",
            Token::RPAREN => ")",
            Token::LBRACE => "{",
            Token::RBRACE => "}",
            Token::LBRACKET => "[",
            Token::RBRACKET => "]",
            Token::COLON => ":",
            Token::FUNCTION => "fn",
            Token::LET => "let",
            Token::TRUE => "true",
            Token::FALSE => "false",
            Token::IF => "if",
            Token::ELSE => "else",
            Token::RETURN => "return",
            Token::STRUCT => "struct",
            Token::WHILE => "while",
            Token::FOR => "for",
            Token::IN => "in",
        }
    }

    pub fn literal(&self) -> String {
        String::from(match self {
            // valuable
            Token::ILLEGAL(v, _) => return v.to_owned(),
            Token::EOF => "\0",
            Token::IDENT(v) => return v.to_owned(),
            Token::IntLiteral(v) => return v.to_string(),
            Token::FloatLiteral(v) => return v.to_string(),
            Token::StringLiteral(v) => return format!("\"{v}\""),
            // symbol
            Token::ASSIGN => "=",
            Token::PLUS => "+",
            Token::MINUS => "-",
            Token::BANG => "!",
            Token::ASTERISK => "*",
            Token::SLASH => "/",
            Token::LT => "<",
            Token::GT => ">",
            Token::EQ => "==",
            Token::NotEq => "!=",
            Token::GTE => ">=",
            Token::LTE => "<=",
            Token::COMMA => ",",
            Token::SEMICOLON => ";",
            Token::LPAREN => "(",
            Token::RPAREN => ")",
            Token::LBRACE => "{",
            Token::RBRACE => "}",
            Token::LBRACKET => "[",
            Token::RBRACKET => "]",
            Token::COLON => ":",
            // keyword
            Token::FUNCTION => "fn",
            Token::LET => "let",
            Token::TRUE => "true",
            Token::FALSE => "false",
            Token::IF => "if",
            Token::ELSE => "else",
            Token::RETURN => "return",
            Token::STRUCT => "struct",
            Token::WHILE => "while",
            Token::FOR => "for",
            Token::IN => "in",
        })
    }
}

pub static KEY_WORDS_TABLE: LazyLock<HashMap<&'static str, Token>> =
    LazyLock::new(|| key_words_table());

fn key_words_table() -> HashMap<&'static str, Token> {
    let mut set = HashMap::new();
    let key_word_tokens = vec![
        Token::FUNCTION,
        Token::LET,
        Token::TRUE,
        Token::FALSE,
        Token::IF,
        Token::ELSE,
        Token::RETURN,
        Token::STRUCT,
        Token::LET,
        Token::WHILE,
        Token::FOR,
        Token::IN,
    ];
    for x in key_word_tokens {
        set.insert(x.identifier(), x);
    }
    set
}
