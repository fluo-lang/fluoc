use super::{typed_ast, AnnotationType};

use crate::helpers::Pos;
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
            ast::TypeType::Function(args, ret) => unimplemented!(),
        }
    }

    pub fn unique(&mut self, pos: Pos) -> AnnotationType {
        self.type_counter += 1;
        AnnotationType::Infer(self.type_counter, pos)
    }
}

#[cfg(test)]
pub mod AnnotatorTests {
    use super::*;

    macro_rules! assert_infer {
        ($left: expr, $right: expr) => {
            match $right {
                AnnotationType::Infer(value, _) => {
                    assert_eq!($left, value);
                }
                _ => panic!("Not an infer node"),
            }
        };
    }

    #[test]
    fn test_unique() {
        let mut annotator = Annotator::new();
        let _1 = annotator.unique(Pos::new(0, 0, 0));
        let _2 = annotator.unique(Pos::new(0, 0, 0));
        let _3 = annotator.unique(Pos::new(0, 0, 0));
        let _4 = annotator.unique(Pos::new(0, 0, 0));

        assert_infer!(1, _1);
        assert_infer!(2, _2);
        assert_infer!(3, _3);
        assert_infer!(4, _4);
    }
}
