use super::{typed_ast, AdditionalContraints, AnnotationType};

use crate::helpers::Span;
use crate::logger::ErrorValue;
use crate::parser::{ast, ast::Statement};
use crate::typecheck::context::Context;

use std::rc::Rc;

pub struct Annotator {
    /// Assign unique unknown types to be solved
    type_counter: usize,
}

impl Annotator {
    pub fn new() -> Self {
        Annotator { type_counter: 0 }
    }

    pub fn annotate(
        &mut self,
        mut ast: Vec<ast::Statement>,
        context: &mut Context<AnnotationType>,
    ) -> Result<Vec<super::TypedStmt>, ErrorValue> {
        let mut statements = Vec::with_capacity(ast.len());

        for stmt in ast.iter_mut() {
            self.annotate_stmt_1(stmt, context)?;
        }

        for stmt in ast.into_iter() {
            statements.push(self.annotate_stmt_2(stmt, context)?);
        }

        Ok(statements)
    }

    fn annotate_stmt_1(
        &mut self,
        stmt: &mut Statement,
        context: &mut Context<AnnotationType>,
    ) -> Result<(), ErrorValue> {
        match stmt {
            Statement::ExpressionStatement(expr_stmt) => expr_stmt.pass_1(self, context),
            _ => Ok(()),
        }
    }

    fn annotate_stmt_2(
        &mut self,
        stmt: Statement,
        context: &mut Context<AnnotationType>,
    ) -> Result<typed_ast::TypedStmt, ErrorValue> {
        match stmt {
            Statement::ExpressionStatement(expr_stmt) => expr_stmt.pass_2(self, context),
            Statement::Tag(tag) => Ok(typed_ast::TypedStmt {
                pos: tag.pos,
                stmt: typed_ast::TypedStmtEnum::Tag(tag),
            }),
            _ => unimplemented!(),
        }
    }

    pub fn annon_type(&mut self, ty: &ast::Type) -> AnnotationType {
        match &ty.value {
            // The important part!
            ast::TypeType::Unknown => self.unique(ty.pos),

            ast::TypeType::Type(namespace) => AnnotationType::Type(Rc::clone(namespace), ty.pos),
            ast::TypeType::Tuple(tuple) => AnnotationType::Tuple(
                Rc::new(
                    tuple
                        .into_iter()
                        .map(|item| self.annon_type(&item))
                        .collect(),
                ),
                ty.pos,
            ),
            ast::TypeType::Function(_, _) => todo!(),
        }
    }

    pub fn unique(&mut self, pos: Span) -> AnnotationType {
        self.type_counter += 1;
        AnnotationType::Infer(self.type_counter, None, pos)
    }

    pub fn unique_literal(
        &mut self,
        pos: Span,
        literal: Option<AdditionalContraints>,
    ) -> AnnotationType {
        self.type_counter += 1;
        AnnotationType::Infer(self.type_counter, literal, pos)
    }
}

#[cfg(test)]
pub mod annotator_test {
    use super::*;

    use crate::logger::LoggerInner;
    use crate::parser::Parser;
    use crate::sourcemap::SourceMapInner;
    use crate::{typecheck, typecheck::annotation::*};

    use std::path;
    use std::rc::Rc;

    macro_rules! assert_infer {
        ($left: expr, $right: expr) => {
            match $right {
                AnnotationType::Infer(value, con, _) => {
                    assert_eq!($left, value);

                    // Sanity check
                    assert_eq!(con, None);
                }
                _ => panic!("Not an infer node"),
            }
        };
    }

    #[test]
    fn test_unique() {
        let mut annotator = Annotator::new();
        let _1 = annotator.unique(Span::new(0, 0, 0));
        let _2 = annotator.unique(Span::new(0, 0, 0));
        let _3 = annotator.unique(Span::new(0, 0, 0));
        let _4 = annotator.unique(Span::new(0, 0, 0));

        assert_infer!(1, _1);
        assert_infer!(2, _2);
        assert_infer!(3, _3);
        assert_infer!(4, _4);
    }

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
                stmt:
                    TypedStmtEnum::Expression(TypedExpr {
                        expr:
                            TypedExprEnum::Function(TypedFunction {
                                ty: ty @ AnnotationType::Function(ref arguments, _, _),
                                block,
                                ..
                            }),
                        pos: _,
                    }),
                pos: _,
            } => {
                assert_eq!(arguments.len(), 1);

                assert_ne!(ty, block.ty());
                assert_ne!(&arguments[0], ty);

                assert_ne!(&arguments[0], block.ty())
            }
            _ => panic!(),
        }
    }
}
