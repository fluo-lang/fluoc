use crate::helpers;
use std::fmt;
use std::fs;
use std::io;

enum Token {
    STRING(String),
    INLINE(String),
    FUNC,
    IMPORT,
    RETURN,
    NULL,
    LET,
    IDENTIFIER(String),
    INTEGER(String),
    BINOP(String),
    LP,
    RP,
    LCP,
    RCP,
    EQUALS,
    DOT,
    COMMA,
    SEMICOLON,
    QUESTION
}

pub struct Lexer<'a> {
    pub filename: &'a str,
    pub file_contents: fs::File,
    pub position: i64
}

impl Lexer<'_> {
    pub fn new(filename: &str) -> Result<Lexer, io::Error> {
        let file_contents = helpers::read_file(&filename);

        match file_contents {
            Ok(filec) => Ok(Lexer { filename, file_contents: filec, position: 0 }),
            Err(e) => Err(e)
        }
    }

    pub fn next() {

    }
}

impl fmt::Debug for Lexer<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Lexer")
         .field("Filename", &self.filename)
         .field("File contents", &self.file_contents)
         .finish()
    }
}
