use crate::lexer::Span;
use std::fmt::{Debug, Display, Formatter};
use std::fs;

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub kind: ErrorKind,
    pub msg: String,
    pub span: Option<Span>,
    pub source_path: Option<String>,
    pub stack: Vec<CallFrame>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    Runtime,
    Syntax,
    Type,
    Name,
    Index,
    Import,
    Io,
    Assert,
    Panic,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallFrame {
    pub name: String,
    pub span: Span,
    pub source_path: Option<String>,
}

impl Error {
    pub fn new(msg: String) -> Error {
        Error::with_kind(ErrorKind::Runtime, msg)
    }

    pub fn with_kind(kind: ErrorKind, msg: String) -> Error {
        Error {
            kind,
            msg,
            span: None,
            source_path: None,
            stack: Vec::new(),
        }
    }

    pub fn with_span(msg: String, span: Span) -> Error {
        Error::with_kind_span(ErrorKind::Runtime, msg, span)
    }

    pub fn with_kind_span(kind: ErrorKind, msg: String, span: Span) -> Error {
        Error {
            kind,
            msg,
            span: Some(span),
            source_path: None,
            stack: Vec::new(),
        }
    }

    pub fn with_kind_source_span(
        kind: ErrorKind,
        msg: String,
        source_path: Option<String>,
        span: Span,
    ) -> Error {
        Error {
            kind,
            msg,
            span: Some(span),
            source_path,
            stack: Vec::new(),
        }
    }

    pub fn with_source_path(mut self, source_path: Option<String>) -> Error {
        if self.source_path.is_none() {
            self.source_path = source_path;
        }
        self
    }

    pub fn with_stack(mut self, stack: &[CallFrame]) -> Error {
        if self.stack.is_empty() {
            self.stack = stack.to_vec();
        }
        self
    }

    pub fn render(&self, source: &str, path: Option<&str>) -> String {
        let Some(span) = self.span else {
            return self.to_string();
        };
        let display_path = self.source_path.as_deref().or(path);
        let location = match display_path {
            Some(path) => format!("{}:{}:{}", path, span.start.0, span.start.1),
            None => format!("{}:{}", span.start.0, span.start.1),
        };
        let source_text = match (self.source_path.as_deref(), path) {
            (Some(error_path), Some(current_path)) if error_path != current_path => {
                fs::read_to_string(error_path).unwrap_or_default()
            }
            (Some(error_path), None) => fs::read_to_string(error_path).unwrap_or_default(),
            _ => source.to_string(),
        };
        let line = source_text
            .lines()
            .nth(span.start.0.saturating_sub(1))
            .unwrap_or("");
        let start_column = char_column(line, span.start.1).max(1);
        let end_column = if span.end.0 == span.start.0 {
            char_column(line, span.end.1).max(start_column + 1)
        } else {
            line.chars().count() + 1
        };
        let marker_width = end_column.saturating_sub(start_column).max(1);
        let padding = " ".repeat(start_column.saturating_sub(1));
        let marker = "^".repeat(marker_width);
        let line_number = span.start.0.to_string();
        let gutter = " ".repeat(line_number.len());
        let mut rendered = format!(
            "Error: {}\n{}--> {}\n{} |\n{} | {}\n{} | {}{}",
            self.msg, gutter, location, gutter, line_number, line, gutter, padding, marker
        );
        if !self.stack.is_empty() {
            rendered.push_str("\nstack:");
            for frame in self.stack.iter().rev() {
                let location = match frame.source_path.as_deref() {
                    Some(path) => format!("{}:{}:{}", path, frame.span.start.0, frame.span.start.1),
                    None => format!("{}:{}", frame.span.start.0, frame.span.start.1),
                };
                rendered.push_str(format!("\n  at {} {}", frame.name, location).as_str());
            }
        }
        rendered
    }
}

fn char_column(line: &str, byte_column: usize) -> usize {
    let byte_index = byte_column.saturating_sub(1);
    line.char_indices()
        .take_while(|(index, _)| *index < byte_index)
        .count()
        + 1
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.span {
            Some(span) => write!(
                f,
                "Error: {} at {}:{}-{}:{}",
                self.msg, span.start.0, span.start.1, span.end.0, span.end.1
            ),
            None => write!(f, "Error: {}", self.msg),
        }
    }
}

impl std::error::Error for Error {}

pub fn make_error_with_source_span(
    kind: ErrorKind,
    msg: String,
    source_path: Option<String>,
    span: Span,
) -> Error {
    Error::with_kind_source_span(kind, msg, source_path, span)
}

#[cfg(test)]
mod tests {
    use super::{Error, ErrorKind};
    use crate::lexer::{Position, Span};

    #[test]
    fn render_aligns_marker_with_source_for_multi_digit_line_numbers() {
        let source = (1..=27)
            .map(|line| {
                if line == 27 {
                    "    print(result.value)".to_string()
                } else {
                    "".to_string()
                }
            })
            .collect::<Vec<_>>()
            .join("\n");
        let err = Error::with_kind_source_span(
            ErrorKind::Runtime,
            "boom".to_string(),
            Some("testdemo.monkey".to_string()),
            Span::new(Position(27, 5), Position(27, 24)),
        );
        let rendered = err.render(&source, Some("testdemo.monkey"));
        assert!(
            rendered.contains("27 |     print(result.value)"),
            "{rendered}"
        );
        assert!(
            rendered.contains("   |     ^^^^^^^^^^^^^^^^^^^"),
            "{rendered}"
        );
    }
}
