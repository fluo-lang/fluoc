use std::fs;
use std::io;

pub fn read_file<'a>(filename: &'a str) -> io::Result<String> {
    let f = fs::read_to_string(filename)?;
    Ok(f)
}

#[derive(Debug, Clone)]
/// Position helper struct
/// 
/// Used to keep track of all tokens and nodes.
/// Note that there are no line numbers, a `\n` character counts as on character.
pub struct Pos {
    /// Start position in characters
    pub s: i64,

    /// End position in characters
    pub e: i64
}

impl Pos {
    pub fn new(s: i64, e: i64) -> Pos {
        Pos { s, e }
    }
}
