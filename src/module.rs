use crate::lexer;
use crate::logger;
use std::io;

pub struct Module<'a> { 
    pub lexer: lexer::Lexer<'a>,
    pub logger: logger::Logger<'a>
}

impl Module<'_> {
    pub fn new(filename: &str) -> io::Result<Module> {
        let l = lexer::Lexer::new(filename)?;
        let log = logger::Logger::new(filename, l.file_contents.try_clone()?);
        Ok(Module { lexer: l, logger: log})
    }
}
