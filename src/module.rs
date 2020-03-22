use crate::lexer;
use crate::parser::parser;
use crate::logger;
use std::io;

pub struct Module<'a> { 
    pub logger: logger::Logger<'a>,
    pub parser: parser::Parser<'a>
}

impl Module<'_> {
    pub fn new(filename: &str) -> io::Result<Module> {
        let l = lexer::Lexer::new(filename)?;
        let log = logger::Logger::new(filename, l.clone().file_contents);
        let p = parser::Parser::new(l);
        Ok(Module { logger: log, parser: p})
    }
}
