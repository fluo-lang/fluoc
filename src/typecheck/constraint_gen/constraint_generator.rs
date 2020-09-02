use crate::typecheck::annotation::{
    AnnotationType, TypedExpr, TypedFunction, TypedStmt, TypedStmtEnum,
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

pub fn generate(ast: &[TypedStmt]) -> Constraints {
    let mut constraints = Constraints::new();
    for node in ast {
        match &node.stmt {
            TypedStmtEnum::Function(TypedFunction {
                ty: ty @ AnnotationType::Function(ref func_args, _),
                block,
            }) => {
                constraints.extend(generate(&block.stmts).0);
                constraints.insert(Constraint::new(
                    Cow::Borrowed(&ty),
                    Cow::Owned(AnnotationType::Function(
                        Rc::clone(func_args),
                        Box::new(block.ty.clone()),
                    )),
                ));
            }
            TypedStmtEnum::Expression(expr) => {
                constraints.extend(generate_expr(expr).0);
            }
            _ => {}
        }
    }
    constraints
}

fn generate_expr(expr: &TypedExpr) -> Constraints {
    unimplemented!()
}
