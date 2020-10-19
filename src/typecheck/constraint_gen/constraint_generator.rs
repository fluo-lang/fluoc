use crate::typecheck::annotation::{
    AnnotationType, TypedExpr, TypedExprEnum, TypedFunction, TypedStmt, TypedStmtEnum,
};

use either::Either;
use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Constraint {
    pub a: AnnotationType,
    pub b: AnnotationType,
}

impl Constraint {
    pub fn new(a: AnnotationType, b: AnnotationType) -> Constraint {
        Self { a, b }
    }
}

impl fmt::Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} \u{2261} {}", self.a, self.b)
    }
}

/// Our wrapper around HashSet
pub struct Constraints(pub HashSet<Constraint>);

impl Constraints {
    pub fn new() -> Self {
        Constraints(HashSet::new())
    }

    pub fn with_capacity(size: usize) -> Self {
        Constraints(HashSet::with_capacity(size))
    }
}

impl fmt::Display for Constraints {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for c in &self.0 {
            write!(f, "{}\n", c)?;
        }

        Ok(())
    }
}

pub fn generate(
    ast: &[TypedStmt],
    outer_ty: Option<AnnotationType>,
    inner_ty: Option<AnnotationType>,
) -> Constraints {
    let mut constraints = Constraints::new();
    for node in ast {
        match &node.stmt {
            TypedStmtEnum::Expression(expr) => {
                constraints
                    .0
                    .extend(generate_expr(expr, outer_ty.clone(), inner_ty.clone()).0);
            }
            TypedStmtEnum::Tag(_) => {}
            _ => panic!("Unimplemented {:?}", node),
        }
    }
    constraints
}

fn generate_expr(
    expr: &TypedExpr,
    outer_ty: Option<AnnotationType>,
    inner_ty: Option<AnnotationType>,
) -> Constraints {
    let mut constraints = Constraints::new();
    match &expr.expr {
        TypedExprEnum::Function(TypedFunction {
            ty: ty @ AnnotationType::Function(ref func_args, ref ret, _),
            block,
            arg_names: _,
        }) => {
            constraints.0.extend(
                // Set the outer and inner return type to be function annotation type
                generate_expr(
                    block,
                    Some(ret.as_ref().clone()),
                    Some(ret.as_ref().clone()),
                )
                .0,
            );
            constraints.0.insert(Constraint::new(
                ty.clone(),
                AnnotationType::Function(
                    Rc::clone(func_args),
                    Rc::new(block.ty().clone()),
                    expr.pos,
                ),
            ));
        }

        TypedExprEnum::FunctionCall(func_call) => {
            for arg in &func_call.arguments {
                constraints
                    .0
                    .extend(generate_expr(&arg, outer_ty.clone(), inner_ty.clone()).0);
            }

            constraints.0.insert(Constraint::new(
                func_call.func_ty.clone(),
                AnnotationType::Function(
                    Rc::new(
                        func_call
                            .arguments
                            .iter()
                            .map(|arg| arg.ty().clone())
                            .collect(),
                    ),
                    Rc::new(func_call.ty.clone()),
                    expr.pos,
                ),
            ));
        }

        TypedExprEnum::VariableAssignDeclaration(assign_dec) => {
            if let Either::Left(expr) = &assign_dec.expr {
                constraints
                    .0
                    .extend(generate_expr(expr.as_ref(), outer_ty, inner_ty).0);
                constraints.0.insert(Constraint::new(
                    assign_dec.binder.ty.clone(),
                    expr.as_ref().ty().clone(),
                ));
            }
        }

        TypedExprEnum::Yield(yield_expr) => {
            constraints
                .0
                .extend(generate_expr(yield_expr.expr.as_ref(), outer_ty, inner_ty.clone()).0);
            constraints.0.insert(Constraint::new(
                // We can unwrap because parser shouldn't let `yield` be in a
                // position where this a problem
                inner_ty.unwrap(),
                yield_expr.expr.ty().clone(),
            ));
        }

        TypedExprEnum::Return(yield_expr) => {
            constraints
                .0
                .extend(generate_expr(yield_expr.expr.as_ref(), outer_ty.clone(), inner_ty).0);
            constraints.0.insert(Constraint::new(
                // We can unwrap because parser shouldn't let `return` be in a
                // position where this a problem
                outer_ty.unwrap(),
                yield_expr.expr.ty().clone(),
            ));
        }

        TypedExprEnum::Block(block) => {
            constraints
                .0
                .extend(generate(&block.stmts, outer_ty, Some(block.ty.clone())).0);
        }

        TypedExprEnum::RefID(_) => {}

        TypedExprEnum::Is(is) => {
            constraints
                .0
                .extend(generate_expr(is.expr.as_ref(), outer_ty, inner_ty).0);
            constraints
                .0
                .insert(Constraint::new(is.expr.ty().clone(), is.ty.clone()));
        }

        // No constraints are needed for literals
        TypedExprEnum::Literal(_) => {}

        TypedExprEnum::Tuple(tup) => {
            for expr in &tup.exprs {
                constraints
                    .0
                    .extend(generate_expr(&expr, outer_ty.clone(), inner_ty.clone()).0);
            }

            constraints.0.insert(Constraint::new(
                tup.ty.clone(),
                AnnotationType::Tuple(
                    Rc::new(tup.exprs.iter().map(|arg| arg.ty().clone()).collect()),
                    tup.pos,
                ),
            ));
        }

        _ => unimplemented!(),
    }
    constraints
}
