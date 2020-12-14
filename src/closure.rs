use crate::parser::ast::{self, Namespace};
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
                    let captured_variables: HashMap<Rc<Namespace>, HashSet<Rc<Namespace>>> =
                        symbtab
                            .iter()
                            .map(|(fn_name, fn_vars)| {
                                (
                                    Rc::clone(fn_name),
                                    fn_vars
                                        .1
                                        .difference(&fn_vars.0)
                                        .cloned()
                                        .collect::<HashSet<_>>(),
                                )
                            })
                            .collect();
                    println!("{:?} ### {:?}", symbtab, captured_variables);
                }
            }
        }
    }
    input_mir
}

/// Generate a special symbol table for checking closure uses
fn get_symbtab(
    symbtab: &mut HashMap<Rc<Namespace>, (HashSet<Rc<Namespace>>, HashSet<Rc<Namespace>>)>,
    input_ast: &TypedExpr,
    func_name: Rc<Namespace>,
) {
    match &input_ast.expr {
        TypedExprEnum::Return(ret) => {
            get_symbtab(symbtab, &ret.expr, func_name);
        }
        TypedExprEnum::Yield(yield_stmt) => {
            get_symbtab(symbtab, &yield_stmt.expr, func_name);
        }
        TypedExprEnum::Function(func) => {
            let new_pos = func_name.scopes[0].sourcemap.borrow_mut().insert_string(
                format!(
                    "annon_{}_{}_{}",
                    func.block.pos.s, func.block.pos.e, func.block.pos.filename_id
                ),
                func.block.pos,
            );
            let namespace = Namespace::from_name_id(ast::NameID {
                sourcemap: Rc::clone(&func_name.scopes[0].sourcemap),
                pos: new_pos,
            });
            get_symbtab(symbtab, &func.block, namespace);
        }
        TypedExprEnum::Literal(_) => {}
        TypedExprEnum::RefID(name) => {
            symbtab
                .entry(func_name)
                .or_insert((HashSet::new(), HashSet::with_capacity(1)))
                .1
                .insert(Rc::clone(&name.name));
        }
        TypedExprEnum::Block(block) => {
            for stmt in block.stmts.iter() {
                match &stmt.stmt {
                    TypedStmtEnum::Expression(expr) => {
                        get_symbtab(symbtab, &expr, Rc::clone(&func_name));
                    }
                    TypedStmtEnum::VariableDeclaration(var_dec) => {
                        symbtab
                            .entry(Rc::clone(&func_name))
                            .or_insert((HashSet::with_capacity(1), HashSet::new()))
                            .0
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
                    .or_insert((HashSet::with_capacity(1), HashSet::new()))
                    .0
                    .insert(Rc::clone(name));
            }
        }
        TypedExprEnum::VariableAssignDeclaration(var_assign) => {
            symbtab
                .entry(func_name)
                .or_insert((HashSet::with_capacity(1), HashSet::new()))
                .0
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
    }
}
