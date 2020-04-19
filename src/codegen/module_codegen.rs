use crate::lexer;
use crate::parser::parser;
use crate::parser::parser::Parser;
use crate::logger::logger::Error;
use inkwell::module;
use std::io;

/// Module object
/// 
/// There is one module object per file (yes, each file has its own codegen, parser, lexer object).
pub struct CodeGenModule<'a> {
    pub module: module::Module<'a>,
    pub parser: Parser<'a>
}

impl<'a> CodeGenModule<'a> {
    /// Return new module object.
    /// 
    /// Arguments
    /// * `filename` - the filename of the file to read
    pub fn new(module: module::Module<'a>, filename: String) -> io::Result<CodeGenModule<'a>> {
        let l = lexer::Lexer::new(filename)?;
        let p = parser::Parser::new(l);
        Ok(CodeGenModule { module, parser: p })
    }

    pub fn generate(&mut self) -> Result<(), Vec<Error>> {
        self.parser.parse()?;
        
        print!("\n{}", self.parser.ast.as_ref().unwrap().to_string());

        Ok(())
    }
}
