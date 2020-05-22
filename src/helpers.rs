use crate::logger::buffer_writer::color;
use crate::logger::logger::{Error, ErrorLevel};
use std::fs;
use std::path;
use std::process;

pub fn read_file(filename: &path::Path) -> String {
    let f = fs::read_to_string(filename);
    match f {
        Ok(file_cont) => file_cont,
        Err(e) => {
            eprintln!(
                "{}{}Fluo: Fatal Error{}{}: {}: `{}`{}",
                color::RED,
                color::BOLD,
                color::RESET,
                color::BOLD,
                e,
                filename.display(),
                color::RESET
            );
            process::exit(1);
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
/// Position helper struct
///
/// Used to keep track of all tokens and nodes.
/// Note that there are no line numbers, a `\n` character counts as on character.
pub struct Pos<'a> {
    /// Start position in characters
    pub s: usize,

    /// End position in characters
    pub e: usize,

    /// Filename of pos
    pub filename: &'a path::Path,
}

impl<'a> Pos<'a> {
    pub fn new(s: usize, e: usize, filename: &'a path::Path) -> Pos {
        Pos { s, e, filename }
    }

    pub fn to_tuple(&self) -> (usize, usize) {
        (self.s, self.e)
    }
}

pub fn get_high_priority<'a>(errors: Vec<(Error<'a>, ErrorLevel)>) -> Vec<Error<'a>> {
    // The different errors to have different priorities
    // We want to show the errors with the highest priority
    // Show all of the errors that have the the same priority
    let max_error_priority = errors.iter().max_by_key(|x| x.1 as usize).unwrap().1 as usize;
    errors
        .into_iter()
        .filter(|error| error.1 as usize == max_error_priority)
        .map(|error| error.0)
        .collect()
}

pub fn read_file_leak(filename: &path::Path) -> &'static str {
    Box::leak(read_file(filename).into_boxed_str())
}

pub fn canonicalize_file<'a>(file_path: &'a path::Path) -> path::PathBuf {
    match file_path.canonicalize() {
        Ok(val) => val,
        Err(e) => {
            eprintln!(
                "{}{}Fluo: Fatal Error{}{}: {}{}",
                color::RED,
                color::BOLD,
                color::RESET,
                color::BOLD,
                e,
                color::RESET
            );
            process::exit(1);
        }
    }
}
