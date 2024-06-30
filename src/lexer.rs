use crate::error::Error;
use crate::token::Token::StringLiteral;
use crate::token::{Token, KEY_WORDS_TABLE};

pub struct Lexer {
    path: Option<String>,
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
    cur_line: usize,
    cur_ch_position: usize,
}

#[derive(Debug, Clone)]
pub struct Position(pub usize, pub usize);

impl From<(usize, usize)> for Position {
    fn from(tuple: (usize, usize)) -> Self {
        Position(tuple.0, tuple.1)
    }
}

pub fn new_lexer(input: String, path: Option<String>) -> Lexer {
    let unicode = input.chars().collect::<Vec<char>>();
    let mut ch = '\0';
    if unicode.len() > 0 {
        ch = unicode[0];
    }
    Lexer {
        path,
        input: unicode,
        position: 0,
        read_position: 1,
        ch,
        cur_line: 1,
        cur_ch_position: 1,
    }
}

impl Lexer {
    pub fn line_char_at(&self) -> (usize, usize) {
        (self.cur_line, self.cur_ch_position)
    }

    pub fn file_path(&self) -> Option<String> {
        self.path.to_owned()
    }

    pub fn cur_position_msg(&self) -> String {
        match self.path {
            None => {
                format!("{}:{}", self.cur_line, self.cur_ch_position)
            }
            Some(ref path) => {
                format!(
                    "{}{}{}:{}",
                    path,
                    std::path::MAIN_SEPARATOR,
                    self.cur_line,
                    self.cur_ch_position
                )
            }
        }
    }

    pub fn next_token(&mut self) -> Token {
        let token = loop {
            break match self.ch {
                '=' => {
                    // note or div
                    match self.look_ahead() {
                        Some('=') => {
                            self.read_char();
                            Token::EQ
                        }
                        _ => Token::ASSIGN,
                    }
                }
                ';' => Token::SEMICOLON,
                '(' => Token::LPAREN,
                ')' => Token::RPAREN,
                ',' => Token::COMMA,
                '+' => Token::PLUS,
                '-' => Token::MINUS,
                '!' => {
                    // note or div
                    match self.look_ahead() {
                        Some('=') => {
                            self.read_char();
                            Token::NotEq
                        }
                        _ => Token::BANG,
                    }
                }
                '*' => Token::ASTERISK,
                '<' => {
                    // note or div
                    match self.look_ahead() {
                        Some('=') => {
                            self.read_char();
                            Token::LTE
                        }
                        _ => Token::LT,
                    }
                }
                '>' => {
                    // note or div
                    match self.look_ahead() {
                        Some('=') => {
                            self.read_char();
                            Token::GTE
                        }
                        _ => Token::GT,
                    }
                }
                '{' => Token::LBRACE,
                '}' => Token::RBRACE,
                '[' => Token::LBRACKET,
                ']' => Token::RBRACKET,
                ':' => Token::COLON,
                '\0' => {
                    // stream end or invalid token
                    if self.is_stream_eof() {
                        Token::EOF
                    } else {
                        Token::ILLEGAL(
                            '\0'.to_string(),
                            Error {
                                msg: format!("invalid eof char at {}", self.cur_position_msg()),
                            },
                        )
                    }
                }
                '/' => {
                    // note or div
                    match self.look_ahead() {
                        Some('/') => {
                            _ = self.read_note();
                            continue;
                        }
                        _ => Token::SLASH,
                    }
                }
                '"' => self.read_string(), // string
                ' ' | '\t' | '\n' => {
                    // blank
                    self.skip_blanks();
                    continue;
                }
                other => {
                    // number or identity or invalid token
                    // should be return because already read next char
                    return if is_letter(other) {
                        self.read_identifier()
                    } else if is_digit(other) {
                        self.read_number()
                    } else {
                        let token = Token::ILLEGAL(
                            other.to_string(),
                            Error {
                                msg: format!(
                                    "invalid char {} at {}",
                                    other,
                                    self.cur_position_msg()
                                ),
                            },
                        );
                        // skip invalid char
                        self.read_char();
                        token
                    };
                }
            };
        };
        self.read_char();
        token
    }

    fn look_ahead(&mut self) -> Option<char> {
        if self.read_position >= self.input.len() {
            None
        } else {
            Some(self.input[self.read_position])
        }
    }

    fn is_stream_eof(&self) -> bool {
        return self.read_position >= self.position && self.ch == '\0';
    }

    fn next_line(&mut self) {
        self.cur_line += 1;
        self.cur_ch_position = 0;
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
        self.cur_ch_position += 1;
    }

    fn read_note(&mut self) -> String {
        debug_assert_eq!(self.ch, '/');
        self.read_char();
        debug_assert_eq!(self.ch, '/');
        self.read_char();
        let mut str = String::new();
        loop {
            let ch = self.ch;
            if ch != '\n' && !self.is_stream_eof() {
                self.read_char();
                str.push(ch);
                continue;
            }
            break;
        }
        str
    }

    fn read_identifier(&mut self) -> Token {
        let mut str = String::new();
        while is_letter(self.ch) || is_digit(self.ch) {
            str.push(self.ch);
            self.read_char();
        }
        debug_assert!(!str.is_empty());
        KEY_WORDS_TABLE
            .get(str.as_str())
            .cloned()
            .unwrap_or(Token::IDENT(str))
    }

    fn read_number(&mut self) -> Token {
        let mut has_dot = false;
        let mut str = String::new();
        while (has_dot && is_digit(self.ch)) || (!has_dot && (is_digit(self.ch) || self.ch == '.'))
        {
            if self.ch == '.' {
                has_dot = true
            }
            str.push(self.ch);
            self.read_char();
        }
        debug_assert!(!str.is_empty());
        if has_dot {
            match str.parse::<f64>() {
                Ok(num) => Token::FloatLiteral(num),
                Err(_) => Token::ILLEGAL(
                    str.to_owned(),
                    Error {
                        msg: format!("invalid float {} at {}", str, self.cur_position_msg()),
                    },
                ),
            }
        } else {
            match str.parse::<i64>() {
                Ok(num) => Token::IntLiteral(num),
                Err(_) => Token::ILLEGAL(
                    str.to_owned(),
                    Error {
                        msg: format!("invalid float {} at {}", str, self.cur_position_msg()),
                    },
                ),
            }
        }
    }

    fn read_string(&mut self) -> Token {
        debug_assert_eq!(self.ch, '"');
        // read left "
        self.read_char();
        let mut str = String::new();
        loop {
            let ch = self.ch;
            if ch != '"' && !self.is_stream_eof() {
                if ch != '\\' {
                    str.push(ch);
                    self.read_char();
                    continue;
                }
                // handle escape char
                match self.look_ahead() {
                    None => {
                        return Token::ILLEGAL(
                            ch.to_string(),
                            Error {
                                msg: format!(
                                    "unclosed char literal {} at {}",
                                    str,
                                    self.cur_position_msg()
                                ),
                            },
                        )
                    }
                    Some(ch) => {
                        match ch {
                            'n' => str.push('\n'),
                            't' => str.push('\t'),
                            'r' => str.push('\r'),
                            '\\' => str.push('\\'),
                            '"' => str.push('"'),
                            '0' => str.push('\0'),
                            _ => {
                                return Token::ILLEGAL(
                                    format!("\\{}", ch),
                                    Error {
                                        msg: format!(
                                            "unsupported escape character \\{} at {}",
                                            ch,
                                            self.cur_position_msg()
                                        ),
                                    },
                                )
                            }
                        };
                        // read escape char
                        self.read_char();
                        self.read_char();
                        continue;
                    }
                }
            }
            break;
        }
        if self.is_stream_eof() {
            return Token::ILLEGAL(
                format!("\"{}", str),
                Error {
                    msg: format!(
                        "not closed string {} at {}, should be \"",
                        str,
                        self.cur_position_msg()
                    ),
                },
            );
        }
        StringLiteral(str)
    }

    fn skip_blanks(&mut self) {
        loop {
            let ch = self.ch;
            if ch == ' ' || ch == '\t' {
                self.read_char();
                continue;
            }
            if ch == '\n' {
                self.next_line();
                self.read_char();
                continue;
            }
            break;
        }
    }
}

fn is_letter(ch: char) -> bool {
    ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'
}

fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}

#[cfg(test)]
mod tests {
    use crate::lexer::new_lexer;
    use crate::token::Token;

    #[test]
    fn test_next_token() {
        let input = String::from("let apple_number = 3;\n\n// test note! \n  let fruit[2]\n =\t  {3.1 ,0}  ; let name = \"\\n\\\"小王\" ;// ");
        let mut lexer = new_lexer(input, None);
        assert_eq!(lexer.next_token(), Token::LET);
        assert_eq!(
            lexer.next_token(),
            Token::IDENT(String::from("apple_number"))
        );
        assert_eq!(lexer.next_token(), Token::ASSIGN);
        assert_eq!(lexer.next_token(), Token::IntLiteral(3));
        assert_eq!(lexer.next_token(), Token::SEMICOLON);
        assert_eq!(lexer.next_token(), Token::LET);
        assert_eq!(lexer.next_token(), Token::IDENT(String::from("fruit")));
        assert_eq!(lexer.next_token(), Token::LBRACKET);
        assert_eq!(lexer.next_token(), Token::IntLiteral(2));
        assert_eq!(lexer.next_token(), Token::RBRACKET);
        assert_eq!(lexer.next_token(), Token::ASSIGN);
        assert_eq!(lexer.next_token(), Token::LBRACE);
        assert_eq!(lexer.next_token(), Token::FloatLiteral(3.1));
        assert_eq!(lexer.next_token(), Token::COMMA);
        assert_eq!(lexer.next_token(), Token::IntLiteral(0));
        assert_eq!(lexer.next_token(), Token::RBRACE);
        assert_eq!(lexer.next_token(), Token::SEMICOLON);
        assert_eq!(lexer.next_token(), Token::LET);
        assert_eq!(lexer.next_token(), Token::IDENT(String::from("name")));
        assert_eq!(lexer.next_token(), Token::ASSIGN);
        assert_eq!(
            lexer.next_token(),
            Token::StringLiteral(String::from("\n\"小王"))
        );
        assert_eq!(lexer.next_token(), Token::SEMICOLON);
        assert_eq!(lexer.next_token(), Token::EOF);
    }
}
