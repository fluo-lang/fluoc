use super::context;

use crate::helpers;
use crate::logger::{ErrorValue, Logger};
use crate::parser::{ast, Parser};
use crate::sourcemap::SourceMap;

use std::collections::HashMap;
use std::rc::Rc;
use std::time::Instant;

/// Typecheck object
pub struct TypeCheckModule {
    parser: Parser,
    modules: HashMap<usize, ast::Block>,
    symtab: context::Context,
    sourcemap: SourceMap,
    logger: Logger,
}

impl TypeCheckModule {
    /// Return new module object.
    pub fn new(filename_id: usize, logger: Logger, sourcemap: SourceMap) -> TypeCheckModule {
        let mut p = Parser::new(filename_id, Rc::clone(&logger), Rc::clone(&sourcemap));
        p.initialize_expr();
        TypeCheckModule {
            logger,
            parser: p,
            modules: HashMap::new(),
            symtab: context::Context::new(),
            sourcemap,
        }
    }

    pub fn type_check(&mut self) -> Result<(), Vec<ErrorValue>> {
        let parser_start = Instant::now();
        // Load core lib on outer scope
        self.parser.parse()?;

        self.logger.borrow().log_verbose(&|| {
            format!(
                "{}: Parsed and lexed",
                helpers::display_duration(parser_start.elapsed())
            )
        }); // Lazily run it so no impact on performance

        let typecheck_start = Instant::now();

        self.logger.borrow().log_verbose(&|| {
            format!(
                "{}: Typechecked",
                helpers::display_duration(typecheck_start.elapsed())
            )
        }); // Lazily run it so no impact on performance

        Ok(())
    }
}
