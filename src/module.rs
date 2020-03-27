use crate::lexer;
use crate::parser::parser;
use crate::logger;
use std::io;

/// Module object
/// 
/// There is one module object per file (yes, each file has its own codegen, parser, lexer, and logger objects).
pub struct Module<'a> { 
    //pub logger: logger::Logger<'a>,
    pub parser: parser::Parser<'a>
}

impl Module<'_> {
    /// Return new module object.
    /// 
    /// Arguments
    /// * `filename` - the filename of the file to read
    pub fn new(filename: &str) -> io::Result<Module> {
        let l = lexer::Lexer::new(filename)?;
        let log = logger::Logger::new(filename, l.clone().file_contents);
        let p = parser::Parser::new(l, log);
        Ok(Module { parser: p })
    }
}
