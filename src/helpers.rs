use crate::logger::buffer_writer::color;
use std::fs;
use std::process;

pub fn read_file(filename: &str) -> String {
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
                filename,
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
    pub filename: &'a str,
}

impl<'a> Pos<'a> {
    pub fn new(s: usize, e: usize, filename: &'a str) -> Pos {
        Pos { s, e, filename }
    }

    pub fn to_tuple(&self) -> (usize, usize) {
        (self.s, self.e)
    }
}
