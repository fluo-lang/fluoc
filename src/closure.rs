use crate::parser::ast::{self, Namespace};
use crate::typecheck::annotation::{
    AnnotationType, TypedAssign, TypedBinder, TypedExpr, TypedExprEnum, TypedRefID, TypedStmt,
    TypedStmtEnum,
};

use either::Either;
use indexmap::IndexMap;

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub fn transform_closures(mut input_mir: Vec<TypedStmt>) -> Vec<TypedStmt> {
    for i in 0..(input_mir.len()) {
        if let TypedStmtEnum::Expression(ref mut expr) = input_mir[i].stmt {
            if let TypedExprEnum::VariableAssignDeclaration(TypedAssign {
                binder: TypedBinder {
                    name: Some(name), ..
                },
                expr: Either::Left(func_expr),
                ..
            }) = &mut expr.expr
            {
                if let TypedExprEnum::Function(ref mut func) = func_expr.expr {
                    let mut symbtab = HashMap::new();
                    let mut function_map = HashMap::new();
                    gen_symbtab(
                        &mut symbtab,
                        &mut function_map,
                        &func.block,
                        Rc::clone(&name),
                    );
                    let mut subs: HashMap<Rc<Namespace>, IndexMap<Rc<Namespace>, AnnotationType>> =
                        HashMap::new();
                    for (fn_name, (defined_variables, used_variables, tys)) in symbtab.iter() {
                        for captured in used_variables.difference(&defined_variables) {
                            let mut function = fn_name;
                            while let Some(name) = function_map.get(function) {
                                // If not, add it to the arguments map
                                subs.entry(Rc::clone(function))
                                    .or_insert(IndexMap::with_capacity(1))
                                    .insert(Rc::clone(captured), tys[captured].clone());
                                // Then repeat, until we find one
                                function = name;

                                // Check if captured variable is in function above
                                if symbtab.contains_key(name) && symbtab[name].0.contains(captured)
                                {
                                    break;
                                }
                            }
                        }
                    }
                    func.block.sub_func(&subs);
                }
            }
        }
    }
    input_mir
}

impl TypedExpr {
    /// Substitute function
    fn sub_func(&mut self, subs: &HashMap<Rc<Namespace>, IndexMap<Rc<Namespace>, AnnotationType>>) {
        match &mut self.expr {
            TypedExprEnum::FunctionCall(ref mut func) => {
                if let Some(absorbed) = subs.get(&func.name) {
                    for (name, ty) in absorbed {
                        func.arguments.push(TypedExpr {
                            pos: name.pos,
                            expr: TypedExprEnum::RefID(TypedRefID {
                                name: Rc::clone(name),
                                ty: ty.clone(),
                            }),
                        })
                    }
                }
            }
            TypedExprEnum::Tuple(ref mut tup) => {
                for expr in tup.exprs.iter_mut() {
                    expr.sub_func(subs);
                }
            }
            TypedExprEnum::Block(ref mut block) => {
                for stmt in block.stmts.iter_mut() {
                    match stmt.stmt {
                        TypedStmtEnum::Expression(ref mut expr) => expr.sub_func(subs),
                        _ => {}
                    }
                }
            }
            TypedExprEnum::VariableAssignDeclaration(TypedAssign {
                binder: TypedBinder {
                    name: Some(name), ..
                },
                expr: Either::Left(expr),
                ..
            }) => {
                if let TypedExprEnum::Function(ref mut func) = expr.as_mut().expr {
                    match &mut func.ty {
                        AnnotationType::Function(ref mut arg_tys, ..) => {
                            if let Some(absorbed) = subs.get(name) {
                                for (name, ty) in absorbed {
                                    func.arg_names.push(Rc::clone(name));
                                    Rc::make_mut(arg_tys).push(ty.clone());
                                }
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                expr.sub_func(subs)
            }
            TypedExprEnum::Return(ref mut ret) => {
                ret.expr.sub_func(subs);
            }
            TypedExprEnum::Yield(ref mut yield_stmt) => {
                yield_stmt.expr.sub_func(subs);
            }
            TypedExprEnum::Is(ref mut is) => {
                is.expr.sub_func(subs);
            }
            TypedExprEnum::Function(ref mut func) => func.block.sub_func(subs),
            TypedExprEnum::VariableAssignDeclaration(..)
            | TypedExprEnum::RefID(..)
            | TypedExprEnum::Literal(..) => {}
        }
    }
}

/// Generate a special symbol table for checking closure uses
fn gen_symbtab(
    symbtab: &mut HashMap<
        Rc<Namespace>,
        (
            HashSet<Rc<Namespace>>,
            HashSet<Rc<Namespace>>,
            HashMap<Rc<Namespace>, AnnotationType>,
        ),
    >,
    def_tree: &mut HashMap<Rc<Namespace>, Rc<Namespace>>,
    input_ast: &TypedExpr,
    func_name: Rc<Namespace>,
) {
    match &input_ast.expr {
        TypedExprEnum::Return(ret) => {
            gen_symbtab(symbtab, def_tree, &ret.expr, func_name);
        }
        TypedExprEnum::Yield(yield_stmt) => {
            gen_symbtab(symbtab, def_tree, &yield_stmt.expr, func_name);
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
            gen_symbtab(symbtab, def_tree, &func.block, namespace);
        }
        TypedExprEnum::Literal(..) => {}
        TypedExprEnum::RefID(name) => {
            let (_, used, tys) = symbtab.entry(func_name).or_insert((
                HashSet::new(),
                HashSet::with_capacity(1),
                HashMap::with_capacity(1),
            ));
            used.insert(Rc::clone(&name.name));
            tys.insert(Rc::clone(&name.name), name.ty.clone());
        }
        TypedExprEnum::Block(block) => {
            for stmt in block.stmts.iter() {
                match &stmt.stmt {
                    TypedStmtEnum::Expression(expr) => {
                        gen_symbtab(symbtab, def_tree, &expr, Rc::clone(&func_name));
                    }
                    TypedStmtEnum::VariableDeclaration(var_dec) => {
                        symbtab
                            .entry(Rc::clone(&func_name))
                            .or_insert((HashSet::with_capacity(1), HashSet::new(), HashMap::new()))
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
                def_tree.insert(Rc::clone(&name), func_name);
                gen_symbtab(symbtab, def_tree, &func.block, Rc::clone(&name));
            } else {
                symbtab
                    .entry(func_name)
                    .or_insert((HashSet::with_capacity(1), HashSet::new(), HashMap::new()))
                    .0
                    .insert(Rc::clone(name));
            }
        }
        TypedExprEnum::VariableAssignDeclaration(var_assign) => {
            symbtab
                .entry(func_name)
                .or_insert((HashSet::with_capacity(1), HashSet::new(), HashMap::new()))
                .0
                .insert(Rc::clone(var_assign.binder.name.as_ref().unwrap()));
        }
        TypedExprEnum::Tuple(tup) => {
            for expr in tup.exprs.iter() {
                gen_symbtab(symbtab, def_tree, &expr, Rc::clone(&func_name));
            }
        }
        TypedExprEnum::FunctionCall(func_call) => {
            for expr in func_call.arguments.iter() {
                gen_symbtab(symbtab, def_tree, &expr, Rc::clone(&func_name));
            }
        }
        TypedExprEnum::Is(is) => {
            gen_symbtab(symbtab, def_tree, is.expr.as_ref(), Rc::clone(&func_name));
        }
    }
}
