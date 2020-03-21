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
    file_contents: String,
}

impl Logger<'_> {
    pub fn new<'a>(filename: &'a str, file_contents: String) -> Logger<'a> {
        Logger { filename, errors: Vec::new(), file_contents }
    }
    pub fn error(&mut self, message: String, error: ErrorType, position: Pos) {
        self.errors.push(
            Error { message, error, position }
        );
    }
}


