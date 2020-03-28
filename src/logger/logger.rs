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

#[derive(Debug, Clone, Copy)]
/// Error display mode
pub enum ErrorDisplayType {
    /// "Error" mode, make underline red and text red
    Error,
    /// "Warning" mode, make underline yellow and text yellow
    Warning,
    /// "Info" mode, make underline Blue and text blue
    Info
}

impl ErrorDisplayType {
    fn plural(&self) -> &str {
        match self {
            ErrorDisplayType::Error => "Errors",
            ErrorDisplayType::Warning => "Warnings",
            _ => "Errors"
        }
    }

    fn singular(&self) -> &str {
        match self {
            ErrorDisplayType::Error => "Error",
            ErrorDisplayType::Warning => "Warning",
            _ => "Error"
        }
    }

    fn get_underline(&self) -> &str {
        match self {
            ErrorDisplayType::Error => "^",
            ErrorDisplayType::Warning => "╌",
            ErrorDisplayType::Info => "━"
        }
    }

    fn get_color(&self) -> &str {
        match self {
            ErrorDisplayType::Error => color::RED,
            ErrorDisplayType::Warning => color::YELLOW,
            ErrorDisplayType::Info => color::BLUE
        }
    }
}

#[derive(Debug, Clone)]
/// Underlines and such
pub struct ErrorAnnotation {
    /// Error message
    message: String,
    /// Error position
    position: Pos,
    /// Error display mode
    mode: ErrorDisplayType
}

impl ErrorAnnotation {
    /// Returns an error annotation
    /// 
    /// # Arguments
    /// 
    /// * `message`: error message
    /// * `position`: position of error
    /// * `mode`: mode of error report
    pub fn new(message: String, position: Pos, mode: ErrorDisplayType) -> ErrorAnnotation {
        ErrorAnnotation {
            message,
            position,
            mode
        }
    }
}

#[derive(Debug, Clone)]
/// An full on error containing useful info.
pub struct Error {
    /// Error message
    message: String,
    /// Error type
    error: ErrorType,
    /// Error position
    position: Pos,
    /// Error display mode
    mode: ErrorDisplayType,
    /// Filename of error
    filename: Option<String>,
    /// Annotations
    annotations: Vec<ErrorAnnotation>
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
    pub fn new(message: String, error: ErrorType, position: Pos, mode: ErrorDisplayType, filename: Option<String>, annotations: Vec<ErrorAnnotation>) -> Error {
        Error {
            message,
            error,
            position,
            mode,
            filename,
            annotations
        }
    }
}

/// Logger object for one file
pub struct Logger<'a> {
    /// Vector of errors
    errors: Vec<Error>,
    /// Filename the logger serves
    filename: &'a str,
    /// Contents of file
    file_contents: String,
    /// Amount of indentation used for error
    indentation: String,
    /// Pipe character used
    pipe_char: String,
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
        Logger { filename, errors: Vec::new(), file_contents, indentation: String::from("  "), pipe_char: String::from("|") }
    }

    /// Pushes an error onto the error vector. 
    pub fn error(&mut self, error: Error) {
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

    fn single_pipe(&self, length_largest_line_no: usize) -> String {
        format!(
            "{}{}{}{} {}{}{}", 
            color::BOLD,
            self.indentation.repeat(2), 
            " ".repeat(
                length_largest_line_no
            ), 
            color::BLUE,
            self.pipe_char,
            self.indentation,
            color::RESET
        )
    }

    fn single_error(&mut self, error: &ErrorAnnotation, message_type: ErrorDisplayType) -> String {
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
            &format!(
                "{}{}{} --> {}{}:{}:{}\n", 
                self.indentation.repeat(2), 
                color::BLUE, 
                color::BOLD, 
                color::RESET, 
                self.filename, 
                (position_range.0).0, 
                (position_range.0).1-1
            )
        );

        code_block.push_str(
            &self.single_pipe(length_largest_line_no)
        );

        code_block.push_str(&"\n".to_string());

        for (i, line) in lines.iter().enumerate() {
            code_block.push_str(
                &format!(
                    "{}{}{}{}{} {}{}{}{}\n", 
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
                    self.pipe_char,
                    self.indentation,
                    color::RESET,
                    line
                )
            );

            if i+start_visible_line+1 == (position_range.0).0 {
                code_block.push_str(
                    &self.single_pipe(length_largest_line_no)
                );

                code_block.push_str(
                    &format!(
                        "{}{}{}{}",
                        color::BOLD,
                        message_type.get_color(),
                        " ".repeat((position_range.0).1-1),
                        message_type.get_underline().repeat(
                            if (position_range.1).0 == i+start_visible_line+1 { 
                                (position_range.1).1 - (position_range.0).1 
                            } else { 
                                line.len()-(position_range.0).1-2 
                            }
                        ),
                    )
                );

                code_block.push_str(
                    &error.message
                );
                code_block.push_str(&format!("{}\n", color::RESET));
            }
        }
        code_block
    }

    fn get_code(&mut self, errors: Vec<ErrorAnnotation>, message_type: ErrorDisplayType) -> String {
        if errors.len() == 1 {
            self.single_error(errors.first().unwrap(), message_type)
        } else {
            String::from("")
        }
    }

    fn raise_type(&mut self, errors: Vec<Error>, message_type: ErrorDisplayType) {
        if errors.len() > 0 { 
            eprintln!(
                "{}{}{}{} {} Found:{}\n", 
                message_type.get_color(), 
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
        } else {
            return ();
        }
        
        for error in errors {
            eprintln!(
                "{}{}{}{}{}: {}{}{}\n", 
                self.indentation, 
                color::BOLD, 
                message_type.get_color(), 
                error.error.as_str(), 
                color::RESET, 
                color::BOLD, 
                error.message,
                color::RESET
            );
            eprintln!("{}", self.get_code(error.annotations, message_type));
        }
    }

    /// Raises all the errors on the error vector.
    /// Note: doesn't exit out of the program.
    pub fn raise(&mut self) {
        let warnings: Vec<Error> = self.errors
                                        .iter()
                                        .cloned()
                                        .filter(|x| if let ErrorDisplayType::Warning = x.mode { true } else { false })
                                        .collect();

        self.raise_type(warnings, ErrorDisplayType::Warning);

        let errors: Vec<Error> = self.errors
                                      .iter()
                                      .cloned()
                                      .filter(|x| if let ErrorDisplayType::Error = x.mode { true } else { false })
                                      .collect();

        self.raise_type(errors, ErrorDisplayType::Error);
    }

    /// Static method for error that parses the furthest.
    /// Useful when you have multiple errors and want to know which one is the most accurate.
    pub fn longest(errors: Vec<Error>) -> Error {
        let first = errors.first().unwrap().clone();
        let mut longest = (first.clone().position.e, first);

        for error in errors {
            let last_pos = error.position.e;
            if last_pos > longest.0 {
                longest = (last_pos, error);
            }
        }

        longest.1
    }
}
