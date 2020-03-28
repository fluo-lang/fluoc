use crate::helpers::Pos;
use crate::logger::buffer_writer::{Color, Style, Buffer, Font, color};
use std::collections::HashMap;

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

    fn get_color(&self) -> &'static str {
        match self {
            ErrorDisplayType::Error => color::RED,
            ErrorDisplayType::Warning => color::YELLOW,
            ErrorDisplayType::Info => color::BLUE
        }
    }

    fn get_color_class(&self) -> Color {
        match self {
            ErrorDisplayType::Error => Color::RED,
            ErrorDisplayType::Warning => Color::YELLOW,
            ErrorDisplayType::Info => Color::BLUE
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
    mode: ErrorDisplayType,
    /// Filename of annotation
    filename: String
}

impl<'a> ErrorAnnotation {
    /// Returns an error annotation
    /// 
    /// Arguments
    /// 
    /// * `message`: error message
    /// * `position`: position of error
    /// * `mode`: mode of error report
    /// * `filename`: filename of annotation
    pub fn new(message: String, position: Pos, mode: ErrorDisplayType, filename: String) -> ErrorAnnotation {
        ErrorAnnotation {
            message,
            position,
            mode,
            filename
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
    filename: String,
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
    pub fn new(message: String, error: ErrorType, position: Pos, mode: ErrorDisplayType, filename: String, annotations: Vec<ErrorAnnotation>) -> Error {
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

#[derive(Clone)]
/// Logger object for one file
pub struct Logger {
    /// Vector of errors
    errors: Vec<Error>,
    /// Filename + file contents the logger serves
    filename_contents: HashMap<String, String>,
    /// Amount of indentation used for error
    indentation: String,
    /// Buffer object
    buffer: Buffer
}

impl Logger {
    /// Return a new logger object.
   
    pub fn new() -> Logger {
        let buffer = Buffer::new();
        Logger { errors: Vec::new(), filename_contents: HashMap::new(),  indentation: String::from("  "), buffer }
    }

    /// Adds a file to the logger hash map
    /// 
    /// Arguments
    /// 
    /// * `filename`: filename of the file
    /// * `file_contents`: contents of the file
    pub fn add_file(&mut self, filename: String, file_contents: String) {
        self.filename_contents.insert(filename, file_contents);
    }

    /// Pushes an error onto the error vector. 
    pub fn error(&mut self, error: Error) {
        self.errors.push(
            error
        );
    }

    fn get_lineno(&mut self, pos: usize, filename: &String) -> (usize, usize) {
        let mut lineno = 1;
        let mut relative_pos = 1;
        for c in (&self.filename_contents[filename][..pos]).chars() {
            if c == '\n' {
                lineno += 1;
                relative_pos = 1;
            }
            relative_pos += 1;
        }
        (lineno, relative_pos)
    }

    fn add_pipe(&mut self, ln: usize, max_line_size: usize) -> (usize, usize) {
        self.buffer.writel(ln, 0, &" ".repeat(max_line_size+1), Style::new(None, None));
        self.buffer.writel(ln, max_line_size, "|", Style::new(Some(Color::BLUE), Some(Font::BOLD)))
    }

    fn get_max_line_size(&mut self, errors: &Vec<ErrorAnnotation>, filename: &String) -> usize {
        let mut max_line_size = 0;
        for error in errors {
            let temp = self.get_lineno(error.position.e, filename).0;
            if temp > max_line_size {
                max_line_size = temp;
            }
        }
        max_line_size
    }

    fn format_line(&mut self, errors: Vec<ErrorAnnotation>, writer_pos: &mut (usize, usize)) {

    }

    fn get_code(&mut self, errors: Vec<ErrorAnnotation>) {
        let first = errors.first().unwrap();
        let max_line_size: usize = self.get_max_line_size(&errors, &first.filename);
        let mut last_printed_line: usize = 1;
        let mut writer_pos: (usize, usize) = (1, 1);
        
        let mut position;
        let error = errors.first().unwrap();
        position = (self.get_lineno(error.position.s, &first.filename), self.get_lineno(error.position.e, &first.filename));
        
        writer_pos = self.buffer.writel(writer_pos.0-1, writer_pos.1-1, &" ".repeat(max_line_size-1), Style::new(None, None));
        writer_pos = self.buffer.writel(writer_pos.0-1, writer_pos.1-1, "-->", Style::new(Some(Color::BLUE), Some(Font::BOLD)));
        writer_pos = self.buffer.writeln(writer_pos.0-1, writer_pos.1-1, &format!("{}:{}:{}", &first.filename, (position.0).0, (position.0).1), Style::new(None, None));

        writer_pos = self.add_pipe(writer_pos.0-1, max_line_size);
        writer_pos.0 += 1;
        
        // annotations in the same lines together
        for error in errors {
            
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
            self.get_code(error.annotations);
            self.buffer.render();
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
