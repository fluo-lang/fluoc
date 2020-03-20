use crate::lexer;
use crate::logger;
use std::io;

pub struct Module { 
    pub lexer: lexer::Lexer,
    pub logger: logger::Logger
}

impl Module {
    pub fn new(filename: &'static str) -> io::Result<Module> {
        let l = lexer::Lexer::new(filename)?;
        let log = logger::Logger::new(filename, l.file_contents.try_clone()?);
        Ok(Module { lexer: l, logger: log})
    }
}
