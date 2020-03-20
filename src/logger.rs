use std::fs;
use crate::helpers::Pos;

pub enum ErrorType {
    Syntax,
    UnexpectedToken
}

struct Error {
    message: String,
    error: ErrorType,
    position: Pos
}

pub struct Logger<'a> {
    errors: Vec<Error>,
    filename: &'a str,
    file_contents: fs::File,
}

impl Logger<'_> {
    pub fn new(filename: &str, file_contents: fs::File) -> Logger {
        Logger { filename, errors: Vec::new(), file_contents }
    }
    pub fn error(&mut self, message: String, error: ErrorType, position: Pos) {
        self.errors.push(
            Error { message, error, position }
        );
    }
}


