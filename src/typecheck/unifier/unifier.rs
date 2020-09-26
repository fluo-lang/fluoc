use crate::helpers::plural;
use crate::helpers::Pos;
use crate::logger::{ErrorAnnotation, ErrorDisplayType, ErrorType, ErrorValue};
use crate::typecheck::{
    annotation::AnnotationType,
    constraint_gen::{Constraint, Constraints},
};

use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Default)]
pub struct Substitutions {
    pub subs: HashMap<usize, AnnotationType>,
}

impl fmt::Display for Substitutions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (infer_ty, ty) in &self.subs {
            write!(f, "T{} \u{2261} {}\n", infer_ty, ty)?;
        }

        Ok(())
    }
}

impl Substitutions {
    pub fn empty() -> Self {
        Self {
            subs: HashMap::new(),
        }
    }

    fn from_pair(val: usize, ty: AnnotationType) -> Substitutions {
        let mut sub = Substitutions::empty();
        sub.subs.insert(val, ty);
        sub
    }

    fn apply_constraints(&self, constraints: Constraints) -> Constraints {
        Constraints(
            constraints
                .0
                .into_iter()
                .map(|c| self.apply_constraint(c))
                .collect(),
        )
    }

    fn apply_constraint(&self, constraint: Constraint) -> Constraint {
        let first = self.apply(constraint.a.clone());
        let second = self.apply(constraint.b.clone());
        Constraint::new(first, second)
    }

    fn apply(&self, ty: AnnotationType) -> AnnotationType {
        self.subs.iter().fold(ty, |result, solution| {
            let (&infer_num, ty) = solution;
            self.substitute(result, infer_num, ty)
        })
    }

    fn substitute(
        &self,
        ty: AnnotationType,
        infer_num: usize,
        replacement: &AnnotationType,
    ) -> AnnotationType {
        match ty {
            AnnotationType::Type(_, _) => ty,
            AnnotationType::Function(args_ty, ret_ty, pos) => AnnotationType::Function(
                Rc::new(
                    args_ty
                        .iter()
                        .map(|arg_ty| self.substitute(arg_ty.clone(), infer_num, replacement))
                        .collect(),
                ),
                Rc::new(self.substitute((*ret_ty).clone(), infer_num, replacement)),
                pos,
            ),
            AnnotationType::Never(_) => ty,
            AnnotationType::Tuple(tys, pos) => AnnotationType::Tuple(
                Rc::new(
                    tys.iter()
                        .map(|ty| self.substitute(ty.clone(), infer_num, replacement))
                        .collect(),
                ),
                pos,
            ),
            AnnotationType::Infer(infer_num2, _) => {
                if infer_num == infer_num2 {
                    replacement.clone()
                } else {
                    ty
                }
            }
        }
    }
}

pub fn unify(mut constraints: Constraints) -> Result<Substitutions, ErrorValue> {
    if constraints.0.is_empty() {
        Ok(Substitutions::empty())
    } else {
        // Get the first element of the constraints map (random order, actually)
        let first = constraints.0.iter().next().unwrap().clone();
        constraints.0.remove(&first);

        let mut subst = unify_one(&first).map_err(|err| err.with_note(
            format!("note: expected type `{}`\n         found type `{}`", first.a, first.b)
        ))?;

        let subst_rest = unify(subst.apply_constraints(constraints))?;

        subst.subs.extend(subst_rest.subs);
        Ok(subst)
    }
}

fn unify_one(constraint: &Constraint) -> Result<Substitutions, ErrorValue> {
    match (constraint.a.clone(), constraint.b.clone()) {
        (type1 @ AnnotationType::Type(_, _), type2 @ AnnotationType::Type(_, _)) => {
            if type1 == type2 {
                Ok(Substitutions::empty())
            } else {
                Err(type_mismatch_err(&type1, &type2))
            }
        }
        (
            AnnotationType::Function(args1, return1, pos1),
            AnnotationType::Function(args2, return2, pos2),
        ) => {
            let mut constraints = Constraints::with_capacity(args1.len() + 1);

            if args1.len() != args2.len() {
                // Different amount of arguments
                return Err(diff_arguments_err(args1.len(), args2.len(), pos1, pos2));
            }

            for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                constraints
                    .0
                    .insert(Constraint::new(arg1.clone(), arg2.clone()));
            }

            constraints
                .0
                .insert(Constraint::new((*return1).clone(), (*return2).clone()));

            unify(constraints)
        }
        (AnnotationType::Infer(val, pos), ty) => unify_infer(val, pos, &ty),
        (ty, AnnotationType::Infer(val, pos)) => unify_infer(val, pos, &ty),
        (AnnotationType::Never(_), _) => Ok(Substitutions::empty()),
        (_, AnnotationType::Never(_)) => Ok(Substitutions::empty()),
        (AnnotationType::Tuple(tys1, pos1), AnnotationType::Tuple(tys2, pos2)) => {
            let mut constraints = Constraints::with_capacity(tys1.len());
            if tys1.len() != tys2.len() {
                return Err(diff_tuple_err(tys1.len(), tys2.len(), pos1, pos2));
            }

            for (ty1, ty2) in tys1.iter().zip(tys2.iter()) {
                constraints
                    .0
                    .insert(Constraint::new(ty1.clone(), ty2.clone()));
            }

            unify(constraints)
        }
        (ty1, ty2) => Err(type_mismatch_err(&ty1, &ty2)),
    }
}

fn unify_infer(
    infer_num: usize,
    pos: Pos,
    ty: &AnnotationType,
) -> Result<Substitutions, ErrorValue> {
    match &ty {
        AnnotationType::Infer(val, _) if *val == infer_num => Ok(Substitutions::empty()),
        AnnotationType::Infer(_, _) => Ok(Substitutions::from_pair(infer_num, ty.clone())),

        // This will infinitely recurse, big problem!
        _ if ty.occurs(infer_num) => Err(infinite_recurse_err(ty.pos(), pos)),

        _ => Ok(Substitutions::from_pair(infer_num, ty.clone())),
    }
}

impl AnnotationType {
    fn occurs(&self, other: usize) -> bool {
        match self {
            AnnotationType::Infer(this, _) => *this == other,
            AnnotationType::Function(args, ret, _) => {
                args.iter().any(|arg| arg.occurs(other)) || ret.occurs(other)
            }
            AnnotationType::Tuple(tys, _) => tys.iter().any(|ty| ty.occurs(other)),
            _ => false,
        }
    }
}

fn type_mismatch_err(ty1: &AnnotationType, ty2: &AnnotationType) -> ErrorValue {
    ErrorValue::new(
        format!("`{}` and `{}` are not the same type", ty1, ty2),
        ErrorType::TypeMismatch,
        ty1.pos(),
        ErrorDisplayType::Error,
        vec![
            ErrorAnnotation::new(
                Some(format!("`{}` type here", ty1)),
                ty1.pos(),
                ErrorDisplayType::Info,
            ),
            ErrorAnnotation::new(
                Some(format!("`{}` type here", ty2)),
                ty2.pos(),
                ErrorDisplayType::Info,
            ),
        ],
    )
}

fn infinite_recurse_err(pos1: Pos, pos2: Pos) -> ErrorValue {
    ErrorValue::new(
        "infinitely recursing types".to_string(),
        ErrorType::Infer,
        pos1,
        ErrorDisplayType::Error,
        vec![
            ErrorAnnotation::new(
                Some("the nesting occurs here first".to_string()),
                pos1,
                ErrorDisplayType::Info,
            ),
            ErrorAnnotation::new(
                Some("this type is nested".to_string()),
                pos2,
                ErrorDisplayType::Info,
            ),
        ],
    )
}

fn diff_tuple_err(args1: usize, args2: usize, pos1: Pos, pos2: Pos) -> ErrorValue {
    ErrorValue::new(
        "tuples have different numbers of fields".to_string(),
        ErrorType::TypeMismatch,
        pos2,
        ErrorDisplayType::Error,
        vec![
            ErrorAnnotation::new(
                Some(format!("has {} {}", args1, plural(args1, "field"))),
                pos1,
                ErrorDisplayType::Info,
            ),
            ErrorAnnotation::new(
                Some(format!("has {} {}", args2, plural(args2, "field"))),
                pos2,
                ErrorDisplayType::Info,
            ),
        ],
    )
}
fn diff_arguments_err(args1: usize, args2: usize, pos1: Pos, pos2: Pos) -> ErrorValue {
    ErrorValue::new(
        format!("expected {} arguments, found {} arguments", args1, args2),
        ErrorType::TypeMismatch,
        pos2,
        ErrorDisplayType::Error,
        vec![
            ErrorAnnotation::new(
                Some("function defined here".to_string()),
                pos1,
                ErrorDisplayType::Info,
            ),
            ErrorAnnotation::new(
                Some("called here".to_string()),
                pos2,
                ErrorDisplayType::Info,
            ),
        ],
    )
}
