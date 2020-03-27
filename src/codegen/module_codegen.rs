use crate::lexer;
use crate::parser::parser;
use crate::logger;
use crate::parser::parser::Parser;
use inkwell::module;
use std::process;
use std::io;

/// Module object
/// 
/// There is one module object per file (yes, each file has its own codegen, parser, lexer, and logger objects).
pub struct CodeGenModule<'a> {
    pub module: module::Module<'a>,
    parser: Parser<'a>,
}

impl CodeGenModule<'_> {
    /// Return new module object.
    /// 
    /// Arguments
    /// * `filename` - the filename of the file to read
    pub fn new<'a>(filename: &'a str, module: module::Module<'a>) -> io::Result<CodeGenModule<'a>> {
        let l = lexer::Lexer::new(filename)?;
        let log = logger::Logger::new(filename, l.clone().file_contents);
        let p = parser::Parser::new(l, log);
        Ok(CodeGenModule { module, parser: p })
    }

    pub fn generate(&mut self) {
        self.parser.parse();
        
        match &self.parser.ast {
            Some(ast) => {
                println!("{:?}", ast);
            },
            None => { process::exit(1); }
        };
    }
}
