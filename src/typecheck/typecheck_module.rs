use super::{annotation::TypedStmt, annotation};
use super::context::Context;
use super::constraint_gen::generate;
use super::substitute;
use super::unifier::unify;

use crate::helpers;
use crate::logger::{ErrorValue, Logger};
use crate::parser::Parser;
use crate::sourcemap::SourceMap;

use std::rc::Rc;
use std::time::Instant;

/// Typecheck object
pub struct TypeCheckModule {
    parser: Parser,
    sourcemap: SourceMap,
    logger: Logger,
}

impl TypeCheckModule {
    /// Return new typecheck module object.
    pub fn new(filename_id: usize, logger: Logger, sourcemap: SourceMap) -> TypeCheckModule {
        let mut p = Parser::new(filename_id, Rc::clone(&logger), Rc::clone(&sourcemap));
        p.initialize_expr();
        TypeCheckModule {
            logger,
            parser: p,
            sourcemap,
        }
    }

    pub fn type_check(&mut self) -> Result<Vec<TypedStmt>, Vec<ErrorValue>> {
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

        let mut context = Context::new();

        // Generate annotations for the ast
        let mut annotator = annotation::Annotator::new();
        // Ast with types (has some unknowns)
        let mut typed_ast: Vec<annotation::TypedStmt> = annotator
            .annotate(
                std::mem::replace(&mut self.parser.ast, None).unwrap(),
                &mut context,
            )
            .map_err(|e| vec![e])?;

        let constraints = generate(&typed_ast, None, None);
        println!("{}", constraints);

        let solved_constraints = unify(constraints).map_err(|e| vec![e])?;

        Ok(substitute(typed_ast, solved_constraints))
    }
}
