use crate::helpers;
use crate::parser::ast;
use crate::typecheck::annotation::{
    AnnotationType, TypedBinder, TypedExpr, TypedExprEnum, TypedFunction, TypedStmt, TypedStmtEnum,
};

use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

#[derive(PartialEq, Eq, Hash)]
pub struct Constraint<'a> {
    a: Cow<'a, AnnotationType>,
    b: Cow<'a, AnnotationType>,
}

impl<'a> Constraint<'a> {
    fn new(a: Cow<'a, AnnotationType>, b: Cow<'a, AnnotationType>) -> Constraint<'a> {
        Self { a, b }
    }
}

impl fmt::Display for Constraint<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} \u{2261} {}", self.a, self.b)
    }
}

/// Our wrapper around HashSet
pub struct Constraints<'a>(HashSet<Constraint<'a>>);

impl<'a> Constraints<'a> {
    pub fn new() -> Self {
        Constraints(HashSet::new())
    }
}

impl<'a> Deref for Constraints<'a> {
    type Target = HashSet<Constraint<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> DerefMut for Constraints<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl fmt::Display for Constraints<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for c in &self.0 {
            write!(f, "{}\n", c)?;
        }

        Ok(())
    }
}

pub fn generate<'a>(
    ast: &'a [TypedStmt],
    outer_ty: Option<&'a AnnotationType>,
    inner_ty: Option<&'a AnnotationType>,
) -> Constraints<'a> {
    let mut constraints = Constraints::new();
    for node in ast {
        match &node.stmt {
            TypedStmtEnum::Expression(expr) => {
                constraints.extend(generate_expr(expr, outer_ty, inner_ty).0);
            }
            TypedStmtEnum::Tag(_) => {}
            _ => panic!("Unimplemented {:?}", node),
        }
    }
    constraints
}

fn generate_expr<'a>(
    expr: &'a TypedExpr,
    outer_ty: Option<&'a AnnotationType>,
    inner_ty: Option<&'a AnnotationType>,
) -> Constraints<'a> {
    let mut constraints = Constraints::new();
    match &expr.expr {
        TypedExprEnum::Function(TypedFunction {
            ty: ty @ AnnotationType::Function(ref func_args, ref ret),
            block,
        }) => {
            constraints.extend(
                // Set the outer and inner return type to be function annotation type
                generate_expr(block, Some(ret), Some(ret)).0,
            );
            constraints.insert(Constraint::new(
                Cow::Borrowed(&ty),
                Cow::Owned(AnnotationType::Function(
                    Rc::clone(func_args),
                    Box::new(block.ty().clone()),
                )),
            ));
        }

        TypedExprEnum::FunctionCall(func_call) => {
            for arg in &func_call.arguments {
                constraints.extend(generate_expr(&arg, outer_ty, inner_ty).0);
            }

            constraints.insert(Constraint::new(
                Cow::Borrowed(&func_call.func_ty),
                Cow::Owned(AnnotationType::Function(
                    Rc::new(func_call
                        .arguments
                        .iter()
                        .map(|arg| TypedBinder {
                            name: None,
                            ty: arg.ty().clone(),
                            pos: helpers::Pos::new(0, 0, 0)
                        })
                        .collect()),
                    Box::new(func_call.ty.clone()),
                )),
            ));
        }

        TypedExprEnum::VariableAssignDeclaration(assign_dec) => {
            constraints.extend(generate_expr(assign_dec.expr.as_ref(), outer_ty, inner_ty).0);
            constraints.insert(Constraint::new(
                Cow::Borrowed(&assign_dec.binder.ty),
                Cow::Borrowed(assign_dec.expr.as_ref().ty()),
            ));
        }

        TypedExprEnum::Yield(yield_expr) => {
            constraints.extend(generate_expr(yield_expr.expr.as_ref(), outer_ty, inner_ty).0);
            constraints.insert(Constraint::new(
                // We can unwrap because parser shouldn't let `yield` be in a
                // position where this a problem
                Cow::Borrowed(inner_ty.unwrap()),
                Cow::Borrowed(yield_expr.expr.ty()),
            ));
        }

        TypedExprEnum::Return(yield_expr) => {
            constraints.extend(generate_expr(yield_expr.expr.as_ref(), outer_ty, inner_ty).0);
            constraints.insert(Constraint::new(
                // We can unwrap because parser shouldn't let `return` be in a
                // position where this a problem
                Cow::Borrowed(outer_ty.unwrap()),
                Cow::Borrowed(yield_expr.expr.ty()),
            ));
        }

        TypedExprEnum::Block(block) => {
            constraints.extend(generate(&block.stmts, outer_ty, Some(&block.ty)).0);
        }

        TypedExprEnum::RefID(_) => {}

        TypedExprEnum::Is(is) => {
            constraints.extend(generate_expr(is.expr.as_ref(), outer_ty, inner_ty).0);
            constraints.insert(Constraint::new(
                Cow::Borrowed(is.expr.ty()),
                Cow::Borrowed(&is.ty),
            ));
        }

        // No constraints are needed for literals
        TypedExprEnum::Literal(_) => {}
        _ => panic!("Unimplemented {:?}", expr),
    }
    constraints
}
