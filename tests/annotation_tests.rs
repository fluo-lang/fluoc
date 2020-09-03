#![feature(bindings_after_at)]

use lib::logger::LoggerInner;
use lib::parser::Parser;
use lib::sourcemap::SourceMapInner;
use lib::{typecheck, typecheck::annotation::*};

use std::path;
use std::rc::Rc;

macro_rules! parser_run {
    ($code: expr) => {{
        let sourcemap = SourceMapInner::new();
        let filename_code = sourcemap
            .borrow_mut()
            .insert_file(path::PathBuf::from("teset.f"), $code.to_string());
        let logger = LoggerInner::new(true, Rc::clone(&sourcemap));
        let mut parser = Parser::new(filename_code, logger, sourcemap);
        parser.initialize_expr();
        parser.parse().unwrap();
        parser.ast.unwrap()
    }};
}

#[test]
fn annotate_identity() {
    let ast = parser_run!("(a: i32) -> _ { yield a; };");
    let mut annotator = Annotator::new();

    let typed_ast = annotator
        .annotate(ast, &mut typecheck::context::Context::new())
        .unwrap();

    match &typed_ast[0] {
        TypedStmt {
            stmt: TypedStmtEnum::Expression(TypedExpr {
                expr: TypedExprEnum::Function(TypedFunction {
                    ty: ty @ AnnotationType::Function(ref arguments, _),
                    block,
                }),
                pos: _,
            }),
            pos: _,
        } => {
            assert_eq!(arguments.len(), 1);

            assert_ne!(ty, block.ty());
            assert_ne!(&arguments[0].ty, ty);

            assert_ne!(&arguments[0].ty, block.ty())
        }
        _ => panic!(),
    }
}
