use crate::logger::logger::Error;
use crate::parser::parser::Parser;
use crate::parser::{ast, parser};
use crate::typecheck::ast_typecheck;

use std::collections::HashMap;

/// Typecheck object
pub struct TypeCheckModule<'a> {
    pub parser: Parser<'a>,
    pub symtab: HashMap<ast::NameID<'a>, ast::Expr<'a>>,
}

impl<'a> TypeCheckModule<'a> {
    /// Return new module object.
    ///
    /// Arguments
    /// * `filename` - the filename of the file to read
    pub fn new(filename: &'a str, file_contents: &'a str) -> TypeCheckModule<'a> {
        let mut p = parser::Parser::new(filename, file_contents);
        p.initialize_expr();
        TypeCheckModule {
            parser: p,
            symtab: HashMap::new(),
        }
    }

    pub fn type_check(&mut self) -> Result<(), Vec<Error<'a>>> {
        self.parser.parse()?;
        println!("{}", self.parser.ast.as_ref().unwrap().to_string());

        // Do type checking
        match (self.parser.ast.as_ref().unwrap() as &dyn ast_typecheck::TypeCheck).type_check(None)
        {
            Ok(_) => Ok(()),
            Err(e) => Err(e.unwrap_other()),
        }
    }
}
