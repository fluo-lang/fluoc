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

#[cfg(test)]
mod typecheck_tests {
    use super::*;

    use crate::logger::{ErrorType, LoggerInner};
    use crate::sourcemap::SourceMapInner;

    macro_rules! set_up_typecheck {
        ($code: expr) => {{
            let filename = path::PathBuf::from("this_is_another_filename_test.fl");
            let sourcemap = SourceMapInner::new();
            let filename_id = sourcemap.borrow_mut().insert_file(
                filename,
                concat!("@[no_std]\n@[no_core]\n", $code).to_string(),
            );

            let logger = LoggerInner::new(true, Rc::clone(&sourcemap));

            TypeCheckModule::new(filename_id, logger, sourcemap)
        }};
    }

    macro_rules! assert_error {
        ($code: expr, $expected_error: expr, $name: ident) => {
            #[test]
            fn $name() {
                for _ in 0..100 {
                    let mut typechecker = set_up_typecheck!($code);
                    assert_eq!(
                        typechecker
                            .type_check()
                            .expect_err("Failed to typecheck")
                            .into_iter()
                            .map(|err| err.get_error_type())
                            .collect::<Vec<_>>(),
                        $expected_error
                    );
                }
            }
        };
    }

    macro_rules! assert_ok {
        ($code: expr, $name: ident) => {
            #[test]
            fn $name() {
                for _ in 0..100 {
                    let mut typechecker = set_up_typecheck!($code);
                    assert!(typechecker.type_check().is_ok());
                }
            }
        };
    }

    assert_error!(
        r#"let entry = () -> () {
    x;
};"#,
        vec![ErrorType::UndefinedSymbol],
        undef_var
    );

    assert_error!(
        r#"let entry = () -> (i32, i32) {
    return (123, 123 is i64);
};"#,
        vec![ErrorType::TypeMismatch],
        tuple_type_mismatch
    );

    assert_error!(
        r#"let entry = () -> i64 {
};"#,
        vec![ErrorType::TypeMismatch],
        implict_block_return_mismatch
    );

    assert_error!(
        r#"let entry = () {
    return 1923;
};"#,
        vec![ErrorType::TypeMismatch],
        implicit_return_mismatch
    );

    assert_error!(
        r#"let entry = () -> i32 {
    return 1923 is i64;
};"#,
        vec![ErrorType::TypeMismatch],
        return_is_mismatch
    );

    assert_error!(
        r#"let entry = () -> () {
    return 1923;
};"#,
        vec![ErrorType::TypeMismatch],
        explicit_return_mismatch
    );

    assert_error!(
        r#"let entry = () -> (bool, i32) {
    let y = 10;
    let y = y;
    let my_func = () -> _ {
        return y;
    };
    return (x(x(y)), my_func());
};

let x = (a: _) -> _ {
    return a;
};"#,
        vec![ErrorType::TypeMismatch],
        complex_infer_mismatch
    );

    assert_ok!(
        r#"let entry = () {
    let x: i32 = 10;
};"#,
        ok_implicit_empty
    );

    assert_ok!(
        r#"let entry = () -> (i32, i32, i64) {
    let x = 10;
    return (x, 123 is i32, 123);
};"#,
        tuple_match
    );

    assert_ok!(
        r#"let entry = () -> i32 {
    let x = 10;
    return x;
};"#,
        basic_i32_infer
    );

    assert_ok!(
        r#"let entry = () -> i32 {
    let y = 10;
    return x(y);
};

let x = (a: _) -> _ {
    return a;
};"#,
        identity_func_infer
    );

    assert_ok!(
        r#"let entry = () -> i32 {
    yield 10;
};

let x = () -> _ {
    yield entry();
};"#,
        reverse_test
    );

    assert_ok!(
        r#"let entry = () -> (i32, i32) {
    let y = 10;
    let y = y;
    let my_func = () -> _ {
        return y;
    };
    return (x(x(y)), my_func());
};

let x = (a: _) -> _ {
    return a;
};"#,
        complex
    );
}
