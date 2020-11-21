use super::{Color, ErrorGen, ErrorValue, Font};

use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use std::cell::RefCell;
use std::cmp::Reverse;
use std::collections::{HashMap, HashSet};
use std::env::current_dir;
use std::path;
use std::rc::Rc;

use crate::sourcemap::SourceMap;

pub type Logger = Rc<RefCell<LoggerInner>>;

#[derive(Clone)]
/// Logger object for one file
pub struct LoggerInner {
    /// Vector of errors
    errors: Vec<ErrorValue>,
    /// Amount of indentation used for error
    indentation: String,
    verbose: bool,
    sourcemap: SourceMap,

    printed_lines: HashMap<usize, HashSet<usize>>,
    disabled: bool,
}

impl LoggerInner {
    pub fn new(verbose: bool, sourcemap: SourceMap) -> Logger {
        Rc::new(RefCell::new(LoggerInner {
            errors: Vec::new(),
            indentation: "  ".to_string(),
            verbose,
            sourcemap,

            printed_lines: HashMap::new(),
            disabled: false,
        }))
    }

    pub fn disable(&mut self) {
        self.disabled = true;
    }

    /// Pushes an error onto the error vector.
    pub fn error(&mut self, error: ErrorValue) {
        self.errors.push(error);
    }

    /// Push multiple errors onto the error vector
    pub fn append_errors(&mut self, mut errors: Vec<ErrorValue>) {
        self.errors.append(&mut errors);
    }

    pub fn log(&self, logged_val: String) {
        if !self.disabled {
            eprintln!(
                "{}> {}{}{}",
                Color::Cyan,
                Color::Green,
                logged_val,
                Font::Reset
            );
        }
    }

    pub fn log_verbose(&self, logged_val: &dyn Fn() -> String) {
        if self.verbose & !self.disabled {
            eprintln!(
                "{}> {}{}{}",
                Color::Cyan,
                Color::Green,
                logged_val(),
                Font::Reset
            );
        }
    }

    fn make_relative<'a>(&self, path: &'a path::Path) -> Option<&'a path::Path> {
        path.strip_prefix(current_dir().ok()?).ok()
    }

    /// Raises all the errors on the error vector.
    /// Note: doesn't exit out of the program.
    pub fn raise(&mut self) {
        let borrowed_sourcemap = self.sourcemap.borrow();
        let fluo_path_mappings = borrowed_sourcemap.get_ids();

        let mut files = SimpleFiles::new();
        let mut logger_path_mappings = HashMap::new();

        for (file_id, file) in borrowed_sourcemap.get_files() {
            logger_path_mappings.insert(
                *file_id,
                files.add(
                    self.make_relative(&fluo_path_mappings[file_id])
                        .unwrap_or(&fluo_path_mappings[file_id])
                        .to_string_lossy(),
                    file,
                ),
            );
        }

        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        for err in std::mem::replace(&mut self.errors, Vec::new()).into_iter() {
            term::emit(
                &mut writer.lock(),
                &config,
                &files,
                &err.to_diagnostic(&logger_path_mappings),
            )
            .expect("Failed to write error");
        }
    }

    /// Static method for error that parses the furthest.
    /// Useful when you have multiple errors and want to know which one is the most accurate.
    pub fn longest(errors: Vec<ErrorGen>) -> ErrorGen {
        errors.into_iter().max_by_key(|x| {
            (
                // Urgents have a greater priority (i.e. a wrong scope error)
                if x.urgent { 1 } else { 0 },
                // If its urgent, the one first wins
                Reverse(if x.urgent { x.position.s } else { 0 }),
                // Otherwise classify as the end position (the error that parses the furthest)
                x.position.e,
            )
        }).unwrap()
    }
}
