use crate::helpers::Pos;
use crate::lexer::Token;

#[derive(Debug)]
pub enum ErrorType {
    Syntax,
    UnexpectedToken
}

#[derive(Debug)]
pub struct Error {
    message: String,
    error: ErrorType,
    position: Pos,
    token: Option<Token>,
    mode: String
}

pub struct Logger<'a> {
    errors: Vec<Error>,
    filename: &'a str,
    file_contents: String,
}

impl Logger<'_> {
    pub fn new<'a>(filename: &'a str, file_contents: String) -> Logger<'a> {
        Logger { filename, errors: Vec::new(), file_contents }
    }
    pub fn error(&mut self, error: Error) {
        self.errors.push(
            error
        );
    }
}

impl Error {
    pub fn new(message: String, error: ErrorType, position: Pos, token: Option<Token>, mode: String) -> Error {
        Error {
            message,
            error,
            position,
            token,
            mode
        }
    }
}
