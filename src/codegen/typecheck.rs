use crate::lexer;
use crate::parser::{ parser, ast };
use crate::parser::parser::{ Parser };
use crate::logger::logger::Error;
use std::io;
use std::collections::HashMap;

/// Typecheck object
pub struct TypeCheckModule<'a> {
    pub parser: Parser<'a>,
    pub symtab: HashMap<ast::NameID, ast::Expr>
}

impl<'a> TypeCheckModule<'a> {
    /// Return new module object.
    /// 
    /// Arguments
    /// * `filename` - the filename of the file to read
    pub fn new(filename: String) -> io::Result<TypeCheckModule<'a>> {
        let l = lexer::Lexer::new(filename)?;
        let p = parser::Parser::new(l);
        Ok(TypeCheckModule { parser: p, symtab: HashMap::new() })
    }

    pub fn type_check(&mut self) -> Result<(), Vec<Error>> {
        self.parser.parse()?;
        print!("\n{}", self.parser.ast.as_ref().unwrap().to_string());

        // Do type checking

        Ok(())
    }
}
