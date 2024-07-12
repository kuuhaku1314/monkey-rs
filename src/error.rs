use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub msg: String,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error: {}", self.msg)
    }
}

impl std::error::Error for Error {}

pub fn make_error(msg: String) -> Error {
    Error { msg }
}
