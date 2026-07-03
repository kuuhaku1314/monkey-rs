use crate::error::Error;
use crate::token::Token::StringLiteral;
use crate::token::{Token, KEY_WORDS_TABLE};

#[derive(Clone)]
pub struct Lexer {
    input: Vec<char>,
    path: Option<String>,
    position: usize,
    read_position: usize,
    ch: char,
    cur_line: usize,
    cur_ch_position: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position(pub usize, pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Span {
        Span { start, end }
    }

    pub fn point(position: Position) -> Span {
        Span {
            start: position,
            end: position,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

impl From<(usize, usize)> for Position {
    fn from(tuple: (usize, usize)) -> Self {
        Position(tuple.0, tuple.1)
    }
}

pub fn new_lexer(input: String, path: Option<String>) -> Lexer {
    let unicode = input.chars().collect::<Vec<char>>();
    let mut ch = '\0';
    if !unicode.is_empty() {
        ch = unicode[0];
    }
    Lexer {
        input: unicode,
        path,
        position: 0,
        read_position: 1,
        ch,
        cur_line: 1,
        cur_ch_position: 1,
    }
}

impl Lexer {
    pub fn path(&self) -> Option<String> {
        self.path.to_owned()
    }

    pub fn line_char_at(&self) -> (usize, usize) {
        (self.cur_line, self.cur_ch_position)
    }

    pub fn next_spanned_token(&mut self) -> SpannedToken {
        loop {
            self.skip_blanks();
            if self.ch == '/' && self.look_ahead() == Some('/') {
                _ = self.read_note();
                continue;
            }
            break;
        }

        let start = self.current_position();
        let token = match self.ch {
            '=' => match self.look_ahead() {
                Some('=') => {
                    self.read_char();
                    Token::EQ
                }
                _ => Token::Assign,
            },
            ';' => Token::Semicolon,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            ',' => Token::Comma,
            '.' => Token::Dot,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' => {
                // note or div
                match self.look_ahead() {
                    Some('=') => {
                        self.read_char();
                        Token::NotEq
                    }
                    _ => Token::Bang,
                }
            }
            '*' => Token::Asterisk,
            '&' => match self.look_ahead() {
                Some('&') => {
                    self.read_char();
                    Token::And
                }
                _ => Token::Illegal(
                    '&'.to_string(),
                    Error::with_span(
                        "invalid char &, did you mean &&".to_string(),
                        Span::point(self.line_char_at().into()),
                    )
                    .with_source_path(self.path()),
                ),
            },
            '|' => match self.look_ahead() {
                Some('|') => {
                    self.read_char();
                    Token::Or
                }
                _ => Token::Illegal(
                    '|'.to_string(),
                    Error::with_span(
                        "invalid char |, did you mean ||".to_string(),
                        Span::point(self.line_char_at().into()),
                    )
                    .with_source_path(self.path()),
                ),
            },
            '<' => {
                // note or div
                match self.look_ahead() {
                    Some('=') => {
                        self.read_char();
                        Token::Lte
                    }
                    _ => Token::LT,
                }
            }
            '>' => {
                // note or div
                match self.look_ahead() {
                    Some('=') => {
                        self.read_char();
                        Token::Gte
                    }
                    _ => Token::GT,
                }
            }
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            '[' => Token::Lbracket,
            ']' => Token::Rbracket,
            ':' => Token::Colon,
            '\0' => {
                if self.is_stream_eof() {
                    return SpannedToken {
                        token: Token::Eof,
                        span: Span::point(start),
                    };
                } else {
                    Token::Illegal(
                        '\0'.to_string(),
                        Error::with_span(
                            "invalid eof char".to_string(),
                            Span::point(self.line_char_at().into()),
                        )
                        .with_source_path(self.path()),
                    )
                }
            }
            '/' => Token::Slash,
            '"' => self.read_string(), // string
            other => {
                let token = if is_letter(other) {
                    self.read_identifier()
                } else if is_digit(other) {
                    self.read_number()
                } else {
                    let token = Token::Illegal(
                        other.to_string(),
                        Error::with_span(
                            format!("invalid char {}", other),
                            Span::point(self.line_char_at().into()),
                        )
                        .with_source_path(self.path()),
                    );
                    // skip invalid char
                    self.read_char();
                    token
                };
                return SpannedToken {
                    token,
                    span: Span::new(start, self.current_position()),
                };
            }
        };
        self.read_char();
        SpannedToken {
            token,
            span: Span::new(start, self.current_position()),
        }
    }

    fn current_position(&self) -> Position {
        self.line_char_at().into()
    }

    fn look_ahead(&mut self) -> Option<char> {
        if self.read_position >= self.input.len() {
            None
        } else {
            Some(self.input[self.read_position])
        }
    }

    fn is_stream_eof(&self) -> bool {
        self.read_position >= self.position && self.ch == '\0'
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
            .unwrap_or(Token::Ident(str))
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
                Err(_) => Token::Illegal(
                    str.to_owned(),
                    Error::with_span(
                        format!("invalid float {}", str),
                        Span::point(self.line_char_at().into()),
                    )
                    .with_source_path(self.path()),
                ),
            }
        } else {
            match str.parse::<i64>() {
                Ok(num) => Token::IntLiteral(num),
                Err(_) => Token::Illegal(
                    str.to_owned(),
                    Error::with_span(
                        format!("invalid integer {}", str),
                        Span::point(self.line_char_at().into()),
                    )
                    .with_source_path(self.path()),
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
                        return Token::Illegal(
                            ch.to_string(),
                            Error::with_span(
                                format!("unclosed char literal {}", str),
                                Span::point(self.line_char_at().into()),
                            )
                            .with_source_path(self.path()),
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
                                return Token::Illegal(
                                    format!("\\{}", ch),
                                    Error::with_span(
                                        format!("unsupported escape character \\{}", ch),
                                        Span::point(self.line_char_at().into()),
                                    )
                                    .with_source_path(self.path()),
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
            return Token::Illegal(
                format!("\"{}", str),
                Error::with_span(
                    format!("not closed string {}, should be \"", str),
                    Span::point(self.line_char_at().into()),
                )
                .with_source_path(self.path()),
            );
        }
        StringLiteral(str)
    }

    fn skip_blanks(&mut self) {
        loop {
            let ch = self.ch;
            if ch == ' ' || ch == '\t' || ch == '\r' {
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
    ch.is_ascii_lowercase() || ch.is_ascii_uppercase() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use crate::lexer::new_lexer;
    use crate::token::Token;

    #[test]
    fn test_next_spanned_token() {
        let input = String::from("let apple_number = 3;\n\n// test note! \n  let fruit[2]\n =\t  {3.1 ,0}  ; let name = \"\\n\\\"小王\" ;// ");
        let mut lexer = new_lexer(input, None);
        assert_eq!(next_token_value(&mut lexer), Token::Let);
        assert_eq!(
            next_token_value(&mut lexer),
            Token::Ident(String::from("apple_number"))
        );
        assert_eq!(next_token_value(&mut lexer), Token::Assign);
        assert_eq!(next_token_value(&mut lexer), Token::IntLiteral(3));
        assert_eq!(next_token_value(&mut lexer), Token::Semicolon);
        assert_eq!(next_token_value(&mut lexer), Token::Let);
        assert_eq!(
            next_token_value(&mut lexer),
            Token::Ident(String::from("fruit"))
        );
        assert_eq!(next_token_value(&mut lexer), Token::Lbracket);
        assert_eq!(next_token_value(&mut lexer), Token::IntLiteral(2));
        assert_eq!(next_token_value(&mut lexer), Token::Rbracket);
        assert_eq!(next_token_value(&mut lexer), Token::Assign);
        assert_eq!(next_token_value(&mut lexer), Token::Lbrace);
        assert_eq!(next_token_value(&mut lexer), Token::FloatLiteral(3.1));
        assert_eq!(next_token_value(&mut lexer), Token::Comma);
        assert_eq!(next_token_value(&mut lexer), Token::IntLiteral(0));
        assert_eq!(next_token_value(&mut lexer), Token::Rbrace);
        assert_eq!(next_token_value(&mut lexer), Token::Semicolon);
        assert_eq!(next_token_value(&mut lexer), Token::Let);
        assert_eq!(
            next_token_value(&mut lexer),
            Token::Ident(String::from("name"))
        );
        assert_eq!(next_token_value(&mut lexer), Token::Assign);
        assert_eq!(
            next_token_value(&mut lexer),
            Token::StringLiteral(String::from("\n\"小王"))
        );
        assert_eq!(next_token_value(&mut lexer), Token::Semicolon);
        assert_eq!(next_token_value(&mut lexer), Token::Eof);
    }

    fn next_token_value(lexer: &mut crate::lexer::Lexer) -> Token {
        lexer.next_spanned_token().token
    }
}
