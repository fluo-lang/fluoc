use crate::lexer;
use crate::parser::parser;
use crate::parser::parser::{ Parser };
use crate::parser::ast;
use crate::logger::logger::Error;
use inkwell::module;
use std::io;
use std::collections::HashMap;

/// Module object
/// 
/// There is one module object per file (yes, each file has its own codegen, parser, lexer object).
pub struct CodeGenModule<'a> {
    pub module: module::Module<'a>,
    pub parser: Parser<'a>,
    pub symtab: HashMap<ast::NameID, ast::Expr>
}

impl<'a> CodeGenModule<'a> {
    /// Return new module object.
    /// 
    /// Arguments
    /// * `filename` - the filename of the file to read
    pub fn new(module: module::Module<'a>, filename: String) -> io::Result<CodeGenModule<'a>> {
        let l = lexer::Lexer::new(filename)?;
        let p = parser::Parser::new(l);
        Ok(CodeGenModule { module, parser: p, symtab: HashMap::new() })
    }

    pub fn generate(&mut self) -> Result<(), Vec<Error>> {
        self.parser.parse()?;
        
        print!("\n{}", self.parser.ast.as_ref().unwrap().to_string());
        self.type_check()?;

        Ok(())
    }

    pub fn type_check(&mut self) -> Result<(), Vec<Error>> {

        Ok(())
    }
}
