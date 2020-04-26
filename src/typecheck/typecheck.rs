use crate::parser::{ parser, ast };
use crate::parser::parser::{ Parser };
use crate::logger::logger::Error;
use crate::typecheck::ast_typecheck;
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
        let mut p = parser::Parser::new(filename)?;
        p.initialize_expr();
        Ok(TypeCheckModule { parser: p, symtab: HashMap::new() })
    }

    pub fn type_check(&mut self) -> Result<(), Vec<Error>> {
        self.parser.parse()?;
        print!("\n{:#?}\n", self.parser.ast.as_ref().unwrap());

        // Do type checking
        match (self.parser.ast.as_ref().unwrap() as &dyn ast_typecheck::TypeCheck).type_check(&None) {
            Ok(_) => Ok(()),
            Err(e) => Err(e.unwrap_other())
        }
    }
}
