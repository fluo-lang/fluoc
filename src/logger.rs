use crate::helpers::Pos;
use crate::lexer::Token;

#[derive(Debug, Clone)]
/// An error type, i.e `Syntax` error or `UnexpectedToken` error
pub enum ErrorType {
    Syntax,
    UnexpectedToken
}

#[derive(Debug, Clone)]
/// Error display mode
pub enum ErrorDisplayType {
    /// "Simple" mode, only report in one location at given position
    Simple
}

#[derive(Debug, Clone)]
/// An full on error containing useful info. Note it does nto contain filename info.
pub struct Error {
    /// Error message
    message: String,
    /// Error type
    error: ErrorType,
    /// Error position
    position: Pos,
    /// Token error occurred on (useful for syntax errors), optional
    token: Option<Token>,
    /// Error display mode
    mode: ErrorDisplayType
}

/// Logger object for one file
pub struct Logger<'a> {
    /// Vector of errors
    errors: Vec<Vec<Error>>,
    /// Filename the logger serves
    filename: &'a str,
    /// Contents of file
    file_contents: String,
}

impl Logger<'_> {
    /// Return a new logger object.
    /// 
    /// # Arguments
    /// 
    /// * `filename`: filename of the file to report
    /// * `file_contents`: contents of the file
    pub fn new<'a>(filename: &'a str, file_contents: String) -> Logger<'a> {
        Logger { filename, errors: Vec::new(), file_contents }
    }

    /// Pushes an error onto the error vector. 
    pub fn error(&mut self, error: Vec<Error>) {
        self.errors.push(
            error
        );
    }

    /// Raises all the errors on the error vector.
    /// Note: doesn't exit out of the program.
    pub fn raise(&mut self) {
        
    }

    /// Static method for error that parses the furthest.
    /// Useful when you have multiple errors and want to know which one is the most accurate.
    pub fn longest(errors: Vec<Vec<Error>>) -> Vec<Error> {
        let first = errors.first().unwrap().clone();
        let mut longest = (first.clone().first().unwrap().position.e, first);

        for error in errors {
            let last_pos = error.last().unwrap().position.e;
            if last_pos > longest.0 {
                longest = (last_pos, error);
            }
        }

        longest.1.to_vec()
    }
}

impl Error {
    /// Returns an error object
    /// 
    /// # Arguments
    /// 
    /// * `message`: error message
    /// * `error`: error type
    /// * `position`: position of error
    /// * `token`: optional token associated with error
    /// * `mode`: mode of error report
    pub fn new(message: String, error: ErrorType, position: Pos, token: Option<Token>, mode: ErrorDisplayType) -> Error {
        Error {
            message,
            error,
            position,
            token,
            mode
        }
    }
}
