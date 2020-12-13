use crate::parser::ast::Namespace;
use crate::typecheck::annotation::{
    TypedAssign, TypedBinder, TypedExpr, TypedExprEnum, TypedStmt, TypedStmtEnum,
};

use either::Either;

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub fn transform_closures(input_mir: Vec<TypedStmt>) -> Vec<TypedStmt> {
    for i in 0..(input_mir.len()) {
        if let TypedStmtEnum::Expression(ref expr) = input_mir[i].stmt {
            if let TypedExprEnum::VariableAssignDeclaration(TypedAssign {
                binder: TypedBinder {
                    name: Some(name), ..
                },
                expr: Either::Left(func_expr),
                ..
            }) = &expr.expr
            {
                if let TypedExprEnum::Function(ref func) = func_expr.expr {
                    let mut symbtab = HashMap::new();
                    get_symbtab(&mut symbtab, &func.block, Rc::clone(&name));
                    println!("{:?}", symbtab);
                }
            }
        }
    }
    input_mir
}

/// Generate a special symbol table for checking closure uses
fn get_symbtab(
    symbtab: &mut HashMap<Rc<Namespace>, HashSet<Rc<Namespace>>>,
    input_ast: &TypedExpr,
    func_name: Rc<Namespace>,
) {
    match &input_ast.expr {
        TypedExprEnum::Block(block) => {
            for stmt in block.stmts.iter() {
                match &stmt.stmt {
                    TypedStmtEnum::Expression(expr) => {
                        get_symbtab(symbtab, &expr, Rc::clone(&func_name));
                    }
                    TypedStmtEnum::VariableDeclaration(var_dec) => {
                        symbtab
                            .entry(Rc::clone(&func_name))
                            .or_insert(HashSet::with_capacity(1))
                            .insert(Rc::clone(var_dec.name.as_ref().unwrap()));
                    }
                    _ => {}
                }
            }
        }
        TypedExprEnum::VariableAssignDeclaration(TypedAssign {
            binder: TypedBinder {
                name: Some(name), ..
            },
            expr: Either::Left(func_expr),
            ..
        }) => {
            if let TypedExprEnum::Function(ref func) = func_expr.expr {
                get_symbtab(symbtab, &func.block, Rc::clone(&name));
            } else {
                symbtab
                    .entry(func_name)
                    .or_insert(HashSet::with_capacity(1))
                    .insert(Rc::clone(name));
            }
        }
        TypedExprEnum::VariableAssignDeclaration(var_assign) => {
            symbtab
                .entry(func_name)
                .or_insert(HashSet::with_capacity(1))
                .insert(Rc::clone(var_assign.binder.name.as_ref().unwrap()));
        }
        TypedExprEnum::Tuple(tup) => {
            for expr in tup.exprs.iter() {
                get_symbtab(symbtab, &expr, Rc::clone(&func_name));
            }
        }
        TypedExprEnum::FunctionCall(func_call) => {
            for expr in func_call.arguments.iter() {
                get_symbtab(symbtab, &expr, Rc::clone(&func_name));
            }
        }
        TypedExprEnum::Is(is) => {
            get_symbtab(symbtab, is.expr.as_ref(), Rc::clone(&func_name));
        }
        _ => {}
    }
}
