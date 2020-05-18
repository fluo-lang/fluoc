use crate::helpers;
use crate::logger::logger::Error;
use crate::parser::parser;
use crate::parser::parser::Parser;
use crate::typecheck::ast_typecheck;

/// Typecheck object
pub struct TypeCheckModule<'a> {
    pub parser: Parser<'a>,
    pub symtab: ast_typecheck::TypeCheckSymbTab<'a>,
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
            symtab: ast_typecheck::TypeCheckSymbTab::new(),
        }
    }

    pub fn type_check(&mut self) -> Result<(), Vec<Error<'a>>> {
        self.parser.parse()?;

        // Do type checking
        match (self.parser.ast.as_mut().unwrap() as &mut dyn ast_typecheck::TypeCheck)
            .type_check(None, &mut self.symtab)
        {
            Ok(_) => Ok(()),
            Err(e) => Err(helpers::get_high_priority(e.as_vec())),
        }
    }
}
