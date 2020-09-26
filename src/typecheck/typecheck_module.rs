use super::constraint_gen::generate;
use super::context::Context;
use super::substitute;
use super::unifier::unify;
use super::{annotation, annotation::TypedStmt};

use crate::helpers;
use crate::logger::{ErrorValue, Logger};
use crate::parser::Parser;
use crate::paths;
use crate::sourcemap::SourceMap;

use std::path;
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

        // Load prelude
        let mut prelude_path: path::PathBuf = helpers::CORE_LOC.to_owned();
        prelude_path.pop();
        prelude_path.pop();
        prelude_path.push("prelude.fl");

        let contents = paths::read_file(&prelude_path);
        let ast_filename = self
            .sourcemap
            .borrow_mut()
            .insert_file(prelude_path, contents);

        let mut p = Parser::new(
            ast_filename,
            Rc::clone(&self.logger),
            Rc::clone(&self.sourcemap),
        );
        p.initialize_expr();
        p.parse()?;

        // Add prelude to path
        self.parser
            .ast
            .as_mut()
            .unwrap()
            .append(p.ast.as_mut().unwrap());

        self.logger.borrow().log_verbose(&|| {
            format!(
                "{}: Parsed and lexed",
                helpers::display_duration(parser_start.elapsed())
            )
        }); // Lazily run it so no impact on performance

        let typecheck_start = Instant::now();

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
        let solved_constraints = unify(constraints).map_err(|e| vec![e])?;

        substitute(&mut typed_ast, solved_constraints)?;

        self.logger.borrow().log_verbose(&|| {
            format!(
                "{}: Typechecked",
                helpers::display_duration(typecheck_start.elapsed())
            )
        }); // Lazily run it so no impact on performance

        Ok(typed_ast)
    }
}
