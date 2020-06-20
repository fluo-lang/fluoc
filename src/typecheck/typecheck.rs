use crate::fluo_core;
use crate::fluo_std;
use crate::helpers;
use crate::logger::logger::{Error, Logger};
use crate::parser::parser::Parser;
use crate::parser::{ast, parser};
use crate::typecheck::ast_typecheck;

use std::cell::RefCell;
use std::collections::HashMap;
use std::path;
use std::rc::Rc;
use std::time::Instant;

/// Typecheck object
pub struct TypeCheckModule<'a> {
    pub parser: Parser<'a>,
    pub modules: HashMap<&'a path::Path, ast::Block<'a>>,
    pub symtab: ast_typecheck::TypeCheckSymbTab<'a>,
}

impl<'a> TypeCheckModule<'a> {
    /// Return new module object.
    ///
    /// Arguments
    /// * `filename` - the filename of the file to read
    pub fn new(
        filename: &'a path::Path,
        file_contents: &'a str,
        logger: Rc<RefCell<Logger<'a>>>,
    ) -> Result<TypeCheckModule<'a>, Vec<Error<'a>>> {
        let mut p = parser::Parser::new(filename, file_contents, logger);
        p.initialize_expr();
        Ok(TypeCheckModule {
            parser: p,
            modules: HashMap::new(),
            symtab: ast_typecheck::TypeCheckSymbTab::new(),
        })
    }

    pub fn type_check(&mut self) -> Result<(), Vec<Error<'a>>> {
        let parser_start = Instant::now();
        // Load core lib on outer scope
        self.parser.parse()?;
        self.parser
            .ast
            .as_mut()
            .unwrap()
            .nodes
            .append(&mut fluo_core::core::load_core(Rc::clone(
                &self.parser.logger,
            )));

        self.parser
            .ast
            .as_mut()
            .unwrap()
            .nodes
            .append(&mut fluo_std::std::load_std(Rc::clone(&self.parser.logger)));

        self.parser.logger.borrow().log_verbose(&|| {
            format!(
                "{}: Parsed and lexed",
                helpers::display_duration(parser_start.elapsed())
            )
        }); // Lazily run it so no impact on performance

        let typecheck_start = Instant::now();

        // Do type checking
        match (
            self.parser
                .ast
                .as_mut()
                .unwrap()
                .first_pass(&mut self.symtab),
            (self.parser.ast.as_mut().unwrap() as &mut dyn ast_typecheck::TypeCheck<'_>)
                .type_check(None, &mut self.symtab),
        ) {
            (Ok(_), Ok(_)) => {
                self.parser.logger.borrow().log_verbose(&|| {
                    format!(
                        "{}: Typechecked",
                        helpers::display_duration(typecheck_start.elapsed())
                    )
                }); // Lazily run it so no impact on performance
                Ok(())
            }
            (Err(e), _) => Err(helpers::get_high_priority(e.as_vec())),
            (_, Err(e)) => Err(helpers::get_high_priority(e.as_vec())),
        }
    }
}
