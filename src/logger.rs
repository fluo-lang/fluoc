use crate::helpers::Pos;
use crate::lexer::Token;

#[derive(Debug, Clone)]
/// An error type, i.e `Syntax` error or `UnexpectedToken` error
pub enum ErrorType {
    Syntax,
    UnexpectedToken
}

impl ErrorType {
    fn as_str(&self) -> String {
        match *self {
            ErrorType::Syntax => String::from("syntax_error"),
            ErrorType::UnexpectedToken => String::from("unexpected_token"),
        }
    }
}

#[derive(Debug, Clone)]
/// Error display mode
pub enum ErrorDisplayType {
    /// "Error" mode, make underline red and text red
    Error,
    /// "Warning" mode, make underline yellow and text yellow?
    Warning
}

impl ErrorDisplayType {
    fn plural(&self) -> &str {
        match self {
            ErrorDisplayType::Error => "Errors",
            ErrorDisplayType::Warning => "Warnings",
        }
    }

    fn singular(&self) -> &str {
        match self {
            ErrorDisplayType::Error => "Error",
            ErrorDisplayType::Warning => "Warning",
        }
    }
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
    mode: ErrorDisplayType,
    /// Filename of error
    filename: Option<String>
}

/// Logger object for one file
pub struct Logger<'a> {
    /// Vector of errors
    errors: Vec<Vec<Error>>,
    /// Filename the logger serves
    filename: &'a str,
    /// Contents of file
    file_contents: String,
    /// Amount of indentation used for error
    indentation: String
}

pub mod color {
    pub const BLACK: &'static str = "\x1b[30m";
    pub const BOLD: &'static str= "\x1b[1m";
    pub const RED: &'static str = "\x1b[31m";
    pub const GREEN: &'static str = "\x1b[32m";
    pub const YELLOW: &'static str = "\x1b[33m";
    pub const BLUE: &'static str = "\x1b[34m";
    pub const MAGENTA: &'static str = "\x1b[35m";
    pub const CYAN: &'static str = "\x1b[36m";
    pub const WHITE: &'static str = "\x1b[37m";
    pub const UNDERLINE: &'static str = "\x1b[4m";
    pub const RESET: &'static str = "\x1b[0m";
}

impl Logger<'_> {
    /// Return a new logger object.
    /// 
    /// # Arguments
    /// 
    /// * `filename`: filename of the file to report
    /// * `file_contents`: contents of the file
    pub fn new<'a>(filename: &'a str, file_contents: String) -> Logger<'a> {
        Logger { filename, errors: Vec::new(), file_contents, indentation: String::from("  ") }
    }

    /// Pushes an error onto the error vector. 
    pub fn error(&mut self, error: Vec<Error>) {
        self.errors.push(
            error
        );
    }

    fn get_lineno(&mut self, pos: usize) -> (usize, usize) {
        let mut lineno = 1;
        let mut relative_pos = 1;
        for c in (&self.file_contents[..pos]).chars() {
            if c == '\n' {
                lineno += 1;
                relative_pos = 1;
            }
            relative_pos += 1;
        }
        (lineno, relative_pos)
    }

    fn single_error(&mut self, error: &Error, colored: &'static str) -> String {
        let position_range = (self.get_lineno(error.position.s), self.get_lineno(error.position.e));
        let mut code_block = String::new();

        let mut lines: Vec<&str> = self.file_contents.lines().collect();
        
        let start_visible_line = 
            if (position_range.0).0-1 == 0 { 
                (position_range.0).0-1
            } else { 
                (position_range.0).0-2
            };
        
        let end_visible_line =
            if (position_range.1).0 == lines.len() { 
                (position_range.0).0
            } else {
                (position_range.0).0+1
            };

        lines = lines[
            start_visible_line
            ..
            end_visible_line
        ].to_vec();

        let length_largest_line_no = end_visible_line.to_string().len();
        code_block.push_str(
            &format!("{}{}{} --> {}{}:{}:{}\n", self.indentation.repeat(2), color::BLUE, color::BOLD, color::RESET, self.filename, (position_range.0).0, (position_range.0).1-1)
        );

        code_block.push_str(&format!(
            "{}{}{}{} |{}{}\n", 
            color::BOLD,
            self.indentation.repeat(2), 
            " ".repeat(
                length_largest_line_no
            ), 
            color::BLUE,
            self.indentation,
            color::RESET
        ));

        for (i, line) in lines.iter().enumerate() {
            code_block.push_str(
                &format!(
                    "{}{}{}{}{} |{}{} {}\n", 
                    color::BOLD,
                    self.indentation.repeat(2), 
                    " ".repeat(
                        length_largest_line_no-(
                            (i+start_visible_line+1)
                                .to_string()
                                .len()
                        )
                    ), 
                    color::BLUE,
                    i+start_visible_line+1,
                    self.indentation,
                    color::RESET,
                    line
                )
            );

            if i+start_visible_line+1 == (position_range.0).0 {
                code_block.push_str(
                    &format!(
                        "{}{}{}{} |{}{}{}{}{}{}", self.indentation.repeat(2), 
                        color::BLUE,
                        color::BOLD,
                        " ".repeat(length_largest_line_no),
                        " ".repeat((position_range.0).1-1),
                        self.indentation,
                        color::RESET,
                        color::BOLD,
                        colored,
                        "^".repeat(
                            if (position_range.1).0 == i+start_visible_line+1 { 
                                (position_range.1).1 - (position_range.0).1 
                            } else { 
                                line.len()-(position_range.0).1-2 
                            }
                        ),
                    )
                );

                code_block.push_str(
                    match error.error {
                        ErrorType::Syntax => " unexpected token",
                        _ => ""
                    }
                );
                code_block.push_str(&format!("{}\n", color::RESET));
            }
        }
        code_block.push_str("\n");
        code_block
    }

    fn get_code(&mut self, error: Vec<Error>, colored: &'static str) -> String {
        if error.len() == 1 {
            self.single_error(error.first().unwrap(), colored)
        } else {
            String::from("")
        }
    }

    fn raise_type(&mut self, errors: Vec<Vec<Error>>, message_type: ErrorDisplayType) {
        let colored = match message_type {
            ErrorDisplayType::Error => color::RED,
            ErrorDisplayType::Warning => color::YELLOW
        };

        if errors.len() > 0 { 
            eprintln!(
                "{}{}{}{} {} Found:{}\n", 
                colored, 
                color::UNDERLINE, 
                color::BOLD,
                errors.len(), 
                if errors.len() == 1 {
                    message_type.singular()
                } else {
                    message_type.plural()
                },
                color::RESET
            ); 
        }
        
        for error in errors.clone() {
            if error.len() == 1 {
                let first_error = error.first().unwrap();
                match first_error.error {
                    ErrorType::Syntax => {
                        eprintln!(
                            "{}{}{}{}{}: {}{}, found {} {}\n", 
                            self.indentation, 
                            color::BOLD, 
                            colored, 
                            first_error.error.as_str(), 
                            color::RESET, 
                            color::BOLD, 
                            first_error.message, 
                            first_error.token.as_ref().unwrap(),
                            color::RESET
                        );
                        eprintln!("{}", self.get_code(error, colored));
                    },
                    _ => {}
                }
            } else {
                // We have a more detailed error
            }
        }
    }

    /// Raises all the errors on the error vector.
    /// Note: doesn't exit out of the program.
    pub fn raise(&mut self) {
        let warnings: Vec<Vec<Error>> = self.errors
                                        .iter()
                                        .cloned()
                                        .filter(|x| if let ErrorDisplayType::Warning = x.first().unwrap().mode { true } else { false })
                                        .collect();

        self.raise_type(warnings, ErrorDisplayType::Warning);

        let errors: Vec<Vec<Error>> = self.errors
                                      .iter()
                                      .cloned()
                                      .filter(|x| if let ErrorDisplayType::Error = x.first().unwrap().mode { true } else { false })
                                      .collect();

        self.raise_type(errors, ErrorDisplayType::Error);
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
    pub fn new(message: String, error: ErrorType, position: Pos, token: Option<Token>, mode: ErrorDisplayType, filename: Option<String>) -> Error {
        Error {
            message,
            error,
            position,
            token,
            mode,
            filename
        }
    }
}
