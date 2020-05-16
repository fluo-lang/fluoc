use crate::helpers;
use crate::logger::logger::{
    Error, ErrorAnnotation, ErrorDisplayType, ErrorLevel, ErrorOrVec, ErrorType,
};
use crate::parser::ast::*;

use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum SymbTabObj<'a> {
    CustomType(TypeCheckType<'a>),
    Function(TypeCheckType<'a>),
    Variable(TypeCheckType<'a>),
}

impl<'a> SymbTabObj<'a> {
    fn unwrap_type(&self) -> &TypeCheckType<'a> {
        match self {
            SymbTabObj::Variable(expr) => expr,
            SymbTabObj::Function(expr) => expr,
            SymbTabObj::CustomType(expr) => expr,
        }
    }

    fn unwrap_type_owned(self) -> TypeCheckType<'a> {
        match self {
            SymbTabObj::Variable(expr) => expr,
            SymbTabObj::Function(expr) => expr,
            SymbTabObj::CustomType(expr) => expr,
        }
    }
}

impl<'a> fmt::Display for SymbTabObj<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let representation = match self {
            SymbTabObj::CustomType(_) => "custom type",
            SymbTabObj::Function(_) => "function",
            SymbTabObj::Variable(_) => "variable",
        };
        write!(f, "{}", representation)
    }
}

#[derive(Clone)]
pub struct TypeCheckSymbTab<'a> {
    items: HashMap<Rc<Namespace<'a>>, SymbTabObj<'a>>,
}

#[derive(Debug, Clone)]
pub struct UnionType<'a> {
    pub types: Vec<TypeCheckType<'a>>,
    pos: helpers::Pos<'a>,
    inferred: bool,
}

impl<'a> PartialEq for UnionType<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.types == other.types
    }
}

impl<'a> fmt::Display for UnionType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut final_string = Vec::new();
        for type_val in self.types.iter() {
            final_string.push(type_val.to_string());
        }
        write!(f, "{}", final_string.join(", "))
    }
}

impl<'a> UnionType<'a> {
    fn to_tuple_type(self) -> TypeCheckType<'a> {
        TypeCheckType {
            pos: self.pos,
            value: TypeCheckTypeType::TupleType(self),
            inferred: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ArgumentsTypeCheck<'a> {
    positional: Vec<(NameID<'a>, TypeCheckType<'a>)>,
    pos: helpers::Pos<'a>,
}

impl<'a> fmt::Display for ArgumentsTypeCheck<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut final_string = Vec::new();
        for type_val in self.positional.iter() {
            final_string.push(type_val.1.to_string());
        }
        write!(f, "{}", final_string.join(", "))
    }
}

impl<'a> PartialEq for ArgumentsTypeCheck<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.positional == other.positional
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum TypeCheckTypeType<'a> {
    FunctionSig(ArgumentsTypeCheck<'a>, Box<TypeCheckType<'a>>),
    SingleType(Rc<Type<'a>>),
    TupleType(UnionType<'a>),
    ArrayType(Box<TypeCheckType<'a>>, Box<TypeCheckType<'a>>),
    CustomType(Rc<Namespace<'a>>),
}

impl<'a> TypeCheckTypeType<'a> {
    fn unwrap_func_return(self) -> TypeCheckType<'a> {
        match self {
            TypeCheckTypeType::FunctionSig(_, ret_type) => *ret_type,
            _ => panic!("Tried to unwrap func return from type check type"),
        }
    }

    fn unwrap_func(&self) -> (&ArgumentsTypeCheck<'a>, &TypeCheckType<'a>) {
        match self {
            TypeCheckTypeType::FunctionSig(arguments, ret_type) => (arguments, ret_type),
            _ => panic!("Tried to unwrap func from type check type"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeCheckType<'a> {
    pub value: TypeCheckTypeType<'a>,
    pos: helpers::Pos<'a>,
    inferred: bool,
}

impl<'a> PartialEq for TypeCheckType<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<'a> fmt::Display for TypeCheckType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rep = match &self.value {
            TypeCheckTypeType::FunctionSig(arguments, return_type) => {
                format!("({}) -> {}", arguments, return_type)
            }
            TypeCheckTypeType::SingleType(type_val) => format!("{}", type_val),
            TypeCheckTypeType::TupleType(types) => format!("({})", types),
            TypeCheckTypeType::ArrayType(elements_types, length_type) => {
                format!("[{}; {}]", *elements_types, *length_type)
            }
            TypeCheckTypeType::CustomType(namespace) => format!("{}", namespace),
        };
        write!(f, "{}", rep)
    }
}

impl<'a> TypeCheckType<'a> {
    /// Check for equivalence of types
    fn sequiv<'b>(left: &Self, right: &Self, context: &'b TypeCheckSymbTab<'a>) -> bool {
        match (left.cast_to_basic(context), right.cast_to_basic(context)) {
            (Ok(left_ok), Ok(right_ok)) => return left_ok == right_ok,
            _ => {}
        }
        if left.is_tuple() && right.is_tuple() {
            match (left, right) {
                (
                    TypeCheckType {
                        value: TypeCheckTypeType::TupleType(left_contents),
                        pos: _,
                        inferred: _,
                    },
                    TypeCheckType {
                        value: TypeCheckTypeType::TupleType(right_contents),
                        pos: _,
                        inferred: _,
                    },
                ) if left_contents.types.len() == right_contents.types.len() => {
                    return left_contents.types.iter().zip(&right_contents.types).all(
                        |(left_val, right_val)| TypeCheckType::sequiv(left_val, right_val, context),
                    )
                }
                _ => {}
            }
        }

        false
    }

    fn is_tuple_empty(&self) -> bool {
        match self {
            TypeCheckType {
                value: TypeCheckTypeType::TupleType(tuple_contents),
                pos: _,
                inferred: _,
            } if tuple_contents.types.is_empty() => true,
            _ => false,
        }
    }

    fn is_tuple(&self) -> bool {
        match self {
            TypeCheckType {
                value: TypeCheckTypeType::TupleType(_),
                pos: _,
                inferred: _,
            } => true,
            _ => false,
        }
    }

    pub fn cast_to_basic<'b>(
        &'b self,
        context: &'b TypeCheckSymbTab<'a>,
    ) -> Result<&'static str, ErrorOrVec<'a>> {
        match &self.value {
            TypeCheckTypeType::FunctionSig(_, _) => Err(ErrorOrVec::Error(
                Error::new(
                    "cannot cast to primitive".to_string(),
                    ErrorType::TypeCastError,
                    self.pos,
                    ErrorDisplayType::Error,
                    vec![ErrorAnnotation::new(
                        Some(format!("has type `{}`", self)),
                        self.pos,
                        ErrorDisplayType::Error,
                    )],
                    true,
                ),
                ErrorLevel::TypeError,
            )),
            TypeCheckTypeType::SingleType(type_val) => {
                type_val.value.is_basic_type().or_else(|_| {
                    Err(ErrorOrVec::Error(
                        Error::new(
                            format!("`{}` is not primitive", self),
                            ErrorType::TypeCastError,
                            self.pos,
                            ErrorDisplayType::Error,
                            vec![ErrorAnnotation::new(
                                Some(format!("has type `{}`", self)),
                                self.pos,
                                ErrorDisplayType::Error,
                            )],
                            true,
                        ),
                        ErrorLevel::TypeError,
                    ))
                })
            }
            TypeCheckTypeType::TupleType(_) => Err(ErrorOrVec::Error(
                Error::new(
                    "tuple cannot be casted as primitive".to_string(),
                    ErrorType::TypeCastError,
                    self.pos,
                    ErrorDisplayType::Error,
                    vec![ErrorAnnotation::new(
                        Some(format!("has type `{}`", self)),
                        self.pos,
                        ErrorDisplayType::Error,
                    )],
                    true,
                ),
                ErrorLevel::TypeError,
            )),
            TypeCheckTypeType::ArrayType(_, _) => Err(ErrorOrVec::Error(
                Error::new(
                    "array cannot be casted as primitive".to_string(),
                    ErrorType::TypeCastError,
                    self.pos,
                    ErrorDisplayType::Error,
                    vec![ErrorAnnotation::new(
                        Some(format!("has type `{}`", self)),
                        self.pos,
                        ErrorDisplayType::Error,
                    )],
                    true,
                ),
                ErrorLevel::TypeError,
            )),
            TypeCheckTypeType::CustomType(name_type) => {
                let val = context.get_basic_type(Rc::clone(name_type))?;
                val.unwrap_type().cast_to_basic(context)
            }
        }
    }

    fn all_same_type<'b>(
        types: &[TypeCheckType<'a>],
        context: &'b TypeCheckSymbTab<'a>,
    ) -> Result<(), ErrorOrVec<'a>> {
        types
            .windows(2)
            .map(|w| {
                if TypeCheckType::sequiv(&w[0], &w[1], context) {
                    Ok(())
                } else {
                    return Err(ErrorOrVec::Error(
                        Error::new(
                            "mismatched return type".to_string(),
                            ErrorType::TypeMismatch,
                            w[0].pos,
                            ErrorDisplayType::Error,
                            vec![
                                ErrorAnnotation::new(
                                    Some(format!("type `{}`", w[0])),
                                    w[0].pos,
                                    ErrorDisplayType::Error,
                                ),
                                ErrorAnnotation::new(
                                    Some(format!("is not the same as `{}`", w[1])),
                                    w[1].pos,
                                    ErrorDisplayType::Error,
                                ),
                            ],
                            true,
                        ),
                        ErrorLevel::TypeError,
                    ));
                }
            })
            .collect::<Result<(), ErrorOrVec>>()
    }

    pub fn as_type(val: Rc<Type<'a>>) -> TypeCheckType<'a> {
        if let Ok(_) = val.value.is_basic_type() {
            TypeCheckType {
                pos: val.pos,
                inferred: val.inferred,
                value: TypeCheckTypeType::SingleType(val),
            }
        } else if let TypeType::Tuple(types) = &val.value {
            TypeCheckType {
                value: TypeCheckTypeType::TupleType(UnionType {
                    types: types
                        .into_iter()
                        .map(|x| TypeCheckType::as_type(Rc::clone(&x)))
                        .collect::<Vec<TypeCheckType>>(),
                    pos: val.pos,
                    inferred: val.inferred,
                }),
                pos: val.pos,
                inferred: val.inferred,
            }
        } else if let TypeType::Type(other) = &val.value {
            TypeCheckType {
                value: TypeCheckTypeType::CustomType(Rc::clone(other)),
                pos: val.pos,
                inferred: val.inferred,
            }
        } else {
            panic!(format!(
                "Unknown type {:?} tried to convert to type check type",
                val
            ));
        }
    }

    pub fn construct_basic(type_name: &'a str, pos: helpers::Pos<'a>) -> TypeCheckType<'a> {
        TypeCheckType {
            value: TypeCheckTypeType::SingleType(Rc::new(Type {
                value: TypeType::Type(Rc::new(Namespace {
                    scopes: vec![NameID {
                        value: type_name,
                        pos,
                    }],
                    pos,
                })),
                pos,
                inferred: false,
            })),
            pos,
            inferred: false,
        }
    }

    fn from_expr<'b>(
        val: &mut Expr<'a>,
        context: &'b mut TypeCheckSymbTab<'a>,
    ) -> Result<TypeCheckType<'a>, ErrorOrVec<'a>> {
        Ok(match val {
            Expr::Tuple(tuple_val) => {
                tuple_val.type_val = Some(TypeCheckType {
                    pos: tuple_val.pos,
                    value: TypeCheckTypeType::TupleType(UnionType {
                        pos: tuple_val.pos,
                        types: tuple_val
                            .values
                            .iter_mut()
                            .map(|x| TypeCheckType::from_expr(x, context))
                            .collect::<Result<Vec<TypeCheckType>, _>>()?,
                        inferred: false,
                    }),
                    inferred: false,
                });
                tuple_val.type_val.clone().unwrap()
            }
            Expr::Literal(lit) => lit.type_val.clone().unwrap(),
            Expr::FunctionCall(func_call) => {
                // Validate function call
                let func_call_arg_types = func_call
                    .arguments
                    .positional
                    .iter_mut()
                    .map(|mut argument| TypeCheckType::from_expr(&mut argument, context))
                    .collect::<Result<Vec<TypeCheckType>, _>>()?;
                let func = context.get_function(Rc::clone(&func_call.name))?;
                let func_arg_types: Vec<&TypeCheckType> = func
                    .unwrap_type()
                    .value
                    .unwrap_func()
                    .0
                    .positional
                    .iter()
                    .map(|argument| &argument.1)
                    .collect();

                for (real_type, call_type) in func_arg_types.iter().zip(&func_call_arg_types) {
                    // Check if called argument type matches with expected type
                    if !TypeCheckType::sequiv(real_type, call_type, context) {
                        return Err(ErrorOrVec::Error(
                            Error::new(
                                "type mismatch between function and function call".to_string(),
                                ErrorType::TypeMismatch,
                                func_call.pos,
                                ErrorDisplayType::Error,
                                vec![
                                    ErrorAnnotation::new(
                                        Some(format!(
                                            "`{}` has arguments of type `{}`",
                                            func_call.name,
                                            func_arg_types
                                                .iter()
                                                .map(|x| x.to_string())
                                                .collect::<Vec<String>>()
                                                .join(", ")
                                        )),
                                        func_call.pos,
                                        ErrorDisplayType::Info,
                                    ),
                                    ErrorAnnotation::new(
                                        Some(format!(
                                            "{} but called with `{}`",
                                            " ".repeat(
                                                func_call.name.to_string().chars().count() + 8
                                            ),
                                            func_call_arg_types
                                                .iter()
                                                .map(|x| x.to_string())
                                                .collect::<Vec<String>>()
                                                .join(", ")
                                        )),
                                        func_call.pos,
                                        ErrorDisplayType::Info,
                                    ),
                                ],
                                true,
                            ),
                            ErrorLevel::TypeError,
                        ));
                    }
                }

                func.unwrap_type().value.clone().unwrap_func_return()
            }
            Expr::VariableAssignDeclaration(variable_assignment_dec) => {
                let value_type =
                    TypeCheckType::from_expr(&mut *variable_assignment_dec.expr, context)?;
                let cast_type =
                    TypeCheckOrType::as_typecheck_type(Cow::Borrowed(&variable_assignment_dec.t));
                variable_assignment_dec.t =
                    if !TypeCheckType::sequiv(&cast_type, &value_type, context) {
                        // Not the same type, error
                        return Err(ErrorOrVec::Error(
                            Error::new(
                                "cannot cast value to specified type annotation".to_string(),
                                ErrorType::TypeMismatch,
                                cast_type.pos,
                                ErrorDisplayType::Error,
                                vec![
                                    ErrorAnnotation::new(
                                        Some(format!("value has type `{}`", value_type)),
                                        value_type.pos,
                                        ErrorDisplayType::Info,
                                    ),
                                    ErrorAnnotation::new(
                                        Some(format!("type annotation has type `{}`", cast_type)),
                                        cast_type.pos,
                                        ErrorDisplayType::Info,
                                    ),
                                ],
                                true,
                            ),
                            ErrorLevel::TypeError,
                        ));
                    } else {
                        let type_val =
                            TypeCheckType::from_expr(&mut variable_assignment_dec.expr, context)?;
                        context.set_value(
                            Rc::clone(&variable_assignment_dec.name),
                            SymbTabObj::Variable(type_val),
                        );

                        TypeCheckOrType::TypeCheckType(cast_type)
                    };
                variable_assignment_dec.t.unwrap_type_check_ref().clone()
            }
            Expr::VariableAssign(_variable_assign) => {
                // TODO: add variable assign to type checker
                panic!("Inference for variable assign not implemented yet");
            }
            Expr::Infix(infix) => {
                let left_type = TypeCheckType::from_expr(&mut *infix.left, context)?;
                let right_type = TypeCheckType::from_expr(&mut *infix.right, context)?;

                // TODO: check if the types can be casted to on another
                if TypeCheckType::sequiv(&left_type, &right_type, context) {
                    // Same type
                    left_type
                } else {
                    return Err(ErrorOrVec::Error(
                        Error::new(
                            format!("type mismatch between operands of {}", infix.operator.token),
                            ErrorType::TypeMismatch,
                            infix.pos,
                            ErrorDisplayType::Error,
                            vec![
                                ErrorAnnotation::new(
                                    Some(format!("has type `{}`", left_type)),
                                    left_type.pos,
                                    ErrorDisplayType::Info,
                                ),
                                ErrorAnnotation::new(
                                    Some(format!("has type `{}`", right_type)),
                                    right_type.pos,
                                    ErrorDisplayType::Info,
                                ),
                            ],
                            true,
                        ),
                        ErrorLevel::TypeError,
                    ));
                }
            }
            Expr::RefID(ref_namespace) => {
                let val = context.get_variable(Rc::clone(&ref_namespace.value))?;
                val.clone().unwrap_type_owned()
            }
            _ => panic!("Type expression inference not implemented yet"),
        })
    }
}

pub trait TypeCheck<'a>: std::fmt::Debug {
    fn type_check<'b>(
        &mut self,
        _return_type: Option<Cow<'a, TypeCheckType<'a>>>,
        _context: &'b mut TypeCheckSymbTab<'a>,
    ) -> Result<Cow<'a, TypeCheckType<'a>>, ErrorOrVec<'a>> {
        panic!(format!(
            "TypeCheckType type_check not implemented for {:?}",
            self
        ))
    }
}

impl<'a> Return<'a> {
    fn construct_implicit_err(
        &self,
        value: TypeCheckType<'a>,
        returned_type: TypeCheckType<'a>,
    ) -> ErrorOrVec<'a> {
        ErrorOrVec::Error(
            Error::new(
                "mismatched return type".to_string(),
                ErrorType::TypeMismatch,
                self.expression.pos(),
                ErrorDisplayType::Error,
                vec![
                    ErrorAnnotation::new(
                        Some("but the block implicitly returns `()`".to_string()),
                        value.pos,
                        ErrorDisplayType::Info,
                    ),
                    ErrorAnnotation::new(
                        Some(format!("found return type `{}`", returned_type)),
                        self.pos,
                        ErrorDisplayType::Error,
                    ),
                ],
                true,
            ),
            ErrorLevel::TypeError,
        )
    }

    fn construct_err(
        &self,
        value: TypeCheckType<'a>,
        returned_type: TypeCheckType<'a>,
    ) -> ErrorOrVec<'a> {
        ErrorOrVec::Error(
            Error::new(
                "mismatched return type".to_string(),
                ErrorType::TypeMismatch,
                self.pos,
                ErrorDisplayType::Error,
                vec![
                    ErrorAnnotation::new(
                        Some(format!("return type `{}` found here", value)),
                        value.pos,
                        ErrorDisplayType::Info,
                    ),
                    ErrorAnnotation::new(
                        Some(format!("`{}` is returned here", returned_type)),
                        self.pos,
                        ErrorDisplayType::Error,
                    ),
                ],
                true,
            ),
            ErrorLevel::TypeError,
        )
    }
}

impl<'a> TypeCheck<'a> for Return<'a> {
    fn type_check<'b>(
        &mut self,
        return_type: Option<Cow<'a, TypeCheckType<'a>>>,
        context: &'b mut TypeCheckSymbTab<'a>,
    ) -> Result<Cow<'a, TypeCheckType<'a>>, ErrorOrVec<'a>> {
        let returned_type = TypeCheckType::from_expr(&mut self.expression, context)?;
        match return_type {
            Some(return_type_some) if return_type_some.is_tuple_empty() => {
                // The block returns (), so we can implicitly do this during codegen. This is the negate case for below.
                if let TypeCheckType {
                    value: TypeCheckTypeType::TupleType(tuple_returned_contents),
                    pos: _,
                    inferred: _,
                } = &returned_type
                {
                    if !tuple_returned_contents.types.is_empty() && return_type_some.inferred {
                        return Err(self
                            .construct_implicit_err(return_type_some.into_owned(), returned_type));
                    } else if !tuple_returned_contents.types.is_empty()
                        && !return_type_some.inferred
                    {
                        return Err(
                            self.construct_err(return_type_some.into_owned(), returned_type)
                        );
                    } else {
                        Ok(return_type_some)
                    }
                } else {
                    if return_type_some.inferred {
                        return Err(self
                            .construct_implicit_err(return_type_some.into_owned(), returned_type));
                    } else {
                        return Err(
                            self.construct_err(return_type_some.into_owned(), returned_type)
                        );
                    }
                }
            }
            Some(value) => {
                // Make sure returns are of the same type or can be casted
                if TypeCheckType::sequiv(&returned_type, &value, context) {
                    // Same type
                    Ok(value)
                } else {
                    return Err(self.construct_err(value.into_owned(), returned_type));
                }
            }
            None => {
                // No required return type (i.e. in outer scope)
                Ok(Cow::Owned(TypeCheckType::from_expr(
                    &mut self.expression,
                    context,
                )?))
            }
        }
    }
}

impl<'a> Block<'a> {
    fn insert_implicit_return(&mut self) {
        // Push implicit return to variable
        self.nodes.push(Statement::Return(Return {
            expression: Box::new(Expr::Tuple(Tuple {
                values: Vec::new(),
                type_val: Some(TypeCheckType {
                    value: TypeCheckTypeType::TupleType(UnionType {
                        types: Vec::new(),
                        pos: self.pos,
                        inferred: false,
                    }),
                    pos: self.pos,
                    inferred: false,
                }),
                pos: self.pos,
            })),
            pos: self.pos,
        }));
    }
}

impl<'a> TypeCheck<'a> for Block<'a> {
    fn type_check<'b>(
        &mut self,
        return_type: Option<Cow<'a, TypeCheckType<'a>>>,
        context: &'b mut TypeCheckSymbTab<'a>,
    ) -> Result<Cow<'a, TypeCheckType<'a>>, ErrorOrVec<'a>> {
        let mut errors: Vec<(Error, ErrorLevel)> = Vec::new();
        let mut ret_types: Vec<&mut Expr> = Vec::new();

        let mut context = context.clone();

        for node in &mut self.nodes {
            match node.type_check(return_type.clone(), &mut context) {
                Ok(_) => {}
                Err(e) => {
                    errors.append(&mut e.as_vec());
                }
            }

            if let Statement::Return(Return { expression, pos: _ }) = node {
                ret_types.push(expression)
            }
        }

        if !errors.is_empty() {
            return Err(ErrorOrVec::ErrorVec(errors));
        }

        if !ret_types.is_empty() {
            let ret_types = ret_types
                .iter_mut()
                .map(|expr| Ok(TypeCheckType::from_expr(expr, &mut context)?))
                .collect::<Result<Vec<TypeCheckType<'a>>, ErrorOrVec>>()?;
            TypeCheckType::all_same_type(&ret_types[..], &mut context)?;
            Ok(Cow::Owned(ret_types.into_iter().nth(0).unwrap()))
        } else {
            match return_type {
                Some(value) if value.is_tuple_empty() => {
                    self.insert_implicit_return();
                    Ok(Cow::Owned(TypeCheckType {
                        value: TypeCheckTypeType::TupleType(UnionType {
                            types: Vec::new(),
                            pos: self.pos,
                            inferred: false,
                        }),
                        pos: self.pos,
                        inferred: false,
                    }))
                }
                Some(other) => {
                    // Error, not the right return type
                    // Found `()` expected `other`
                    Err(ErrorOrVec::Error(
                        Error::new(
                            "mismatched return type".to_string(),
                            ErrorType::TypeMismatch,
                            other.pos,
                            ErrorDisplayType::Error,
                            vec![
                                ErrorAnnotation::new(
                                    Some("return type found here".to_string()),
                                    other.pos,
                                    ErrorDisplayType::Error,
                                ),
                                ErrorAnnotation::new(
                                    Some("but `()` implicitly returned".to_string()),
                                    self.pos,
                                    ErrorDisplayType::Info,
                                ),
                            ],
                            true,
                        ),
                        ErrorLevel::TypeError,
                    ))
                }
                None => {
                    self.insert_implicit_return();
                    Ok(Cow::Owned(TypeCheckType {
                        value: TypeCheckTypeType::TupleType(UnionType {
                            types: Vec::new(),
                            pos: self.pos,
                            inferred: false,
                        }),
                        pos: self.pos,
                        inferred: false,
                    }))
                }
            }
        }
    }
}

impl<'a> FunctionDefine<'a> {
    fn as_fn_type(&mut self) -> Result<TypeCheckType<'a>, ErrorOrVec<'a>> {
        let mut positional = Vec::new();
        for idx in 0..self.arguments.positional.len() {
            self.arguments.positional[idx] = (
                self.arguments.positional[idx].0,
                TypeCheckOrType::TypeCheckType(TypeCheckOrType::as_typecheck_type(Cow::Owned(
                    self.arguments.positional[idx].1.clone(),
                ))),
            );
        }
        for argument in &self.arguments.positional {
            positional.push((argument.0, (argument.1.clone()).unwrap_type_check()))
        }
        Ok(TypeCheckType {
            value: TypeCheckTypeType::FunctionSig(
                ArgumentsTypeCheck {
                    positional,
                    pos: self.arguments.pos,
                },
                Box::new(self.return_type.clone().unwrap_type_check()),
            ),
            pos: helpers::Pos::new(
                self.arguments.pos.s,
                if self.return_type.unwrap_type_check_ref().inferred {
                    self.return_type.unwrap_type_check_ref().pos.e
                } else {
                    self.arguments.pos.e
                },
                self.arguments.pos.filename,
            ),
            inferred: false,
        })
    }
}

impl<'a> TypeCheck<'a> for FunctionDefine<'a> {
    fn type_check<'b>(
        &mut self,
        _return_type: Option<Cow<'a, TypeCheckType<'a>>>,
        context: &'b mut TypeCheckSymbTab<'a>,
    ) -> Result<Cow<'a, TypeCheckType<'a>>, ErrorOrVec<'a>> {
        for idx in 0..self.arguments.positional.len() {
            self.arguments.positional[idx] = (
                self.arguments.positional[idx].0,
                TypeCheckOrType::TypeCheckType(TypeCheckType::as_type(
                    (&self.arguments.positional[idx].1).unwrap_type(),
                )),
            );
        }
        for argument in &self.arguments.positional {
            context.set_value(
                Namespace::from_name_id(argument.0),
                SymbTabObj::Variable((argument.1.clone()).unwrap_type_check()),
            )
        }
        self.return_type =
            TypeCheckOrType::TypeCheckType(TypeCheckType::as_type(self.return_type.unwrap_type()));
        context.set_value(
            Namespace::from_name_id(*self.name.deref()),
            SymbTabObj::Function(self.as_fn_type()?),
        );
        (&mut self.block as &mut dyn TypeCheck).type_check(
            Some(Cow::Owned(TypeCheckOrType::as_typecheck_type(
                Cow::Borrowed(&self.return_type),
            ))),
            context,
        )
    }
}

impl<'a> TypeCheck<'a> for ExpressionStatement<'a> {
    fn type_check<'b>(
        &mut self,
        _return_type: Option<Cow<'a, TypeCheckType<'a>>>,
        context: &'b mut TypeCheckSymbTab<'a>,
    ) -> Result<Cow<'a, TypeCheckType<'a>>, ErrorOrVec<'a>> {
        let type_val = TypeCheckType::from_expr(&mut self.expression, context)?;
        Ok(Cow::Owned(type_val))
    }
}

impl<'a> TypeCheck<'a> for Statement<'a> {
    fn type_check<'b>(
        &mut self,
        return_type: Option<Cow<'a, TypeCheckType<'a>>>,
        context: &'b mut TypeCheckSymbTab<'a>,
    ) -> Result<Cow<'a, TypeCheckType<'a>>, ErrorOrVec<'a>> {
        match self {
            Statement::Return(statement) => {
                (statement as &mut dyn TypeCheck).type_check(return_type, context)
            }
            Statement::FunctionDefine(statement) => {
                (statement as &mut dyn TypeCheck).type_check(return_type, context)
            }
            Statement::ExpressionStatement(statement) => {
                (statement as &mut dyn TypeCheck).type_check(return_type, context)
            }
            _ => panic!(format!(
                "Type_check not implemented for statement `{}`",
                &self.to_string()
            )),
        }
    }
}

impl<'a> TypeType<'a> {
    pub fn is_basic_type(&self) -> Result<&'static str, ()> {
        match &self {
            TypeType::Type(value) => {
                if let Some(val) = value.scopes.get(0) {
                    match val.value {
                        "int" => return Ok("int"),
                        "str" => return Ok("str"),
                        _ => {}
                    }
                };
            }
            _ => {}
        };
        Err(())
    }
}

impl<'a> TypeCheckSymbTab<'a> {
    pub fn new() -> TypeCheckSymbTab<'a> {
        TypeCheckSymbTab {
            items: HashMap::new(),
        }
    }

    fn set_value(&mut self, namespace: Rc<Namespace<'a>>, value: SymbTabObj<'a>) {
        self.items.insert(namespace, value);
    }

    fn get_function(
        &self,
        namespace: Rc<Namespace<'a>>,
    ) -> Result<&SymbTabObj<'a>, ErrorOrVec<'a>> {
        match self.items.get(&namespace) {
            Some(val) => match val.deref() {
                SymbTabObj::Function(_) => return Ok(val),
                other => {
                    return Err(ErrorOrVec::Error(
                        Error::new(
                            format!("`{}` is not a function", namespace),
                            ErrorType::UndefinedSymbol,
                            namespace.pos,
                            ErrorDisplayType::Error,
                            vec![ErrorAnnotation::new(
                                Some(format!("`{}` is a `{}`, not a function", namespace, other)),
                                namespace.pos,
                                ErrorDisplayType::Error,
                            )],
                            true,
                        ),
                        ErrorLevel::TypeError,
                    ));
                }
            },
            None => {}
        }
        Err(ErrorOrVec::Error(
            Error::new(
                "function does not exist".to_string(),
                ErrorType::UndefinedSymbol,
                namespace.pos,
                ErrorDisplayType::Error,
                vec![ErrorAnnotation::new(
                    Some(format!("undefined function `{}`", namespace)),
                    namespace.pos,
                    ErrorDisplayType::Error,
                )],
                true,
            ),
            ErrorLevel::NoneExistentValue,
        ))
    }

    fn get_type(&self, namespace: Rc<Namespace<'a>>) -> Result<&SymbTabObj<'a>, ErrorOrVec<'a>> {
        match self.items.get(&namespace) {
            Some(val) => match val.deref() {
                SymbTabObj::CustomType(_) => return Ok(val),
                other => {
                    return Err(ErrorOrVec::Error(
                        Error::new(
                            format!("`{}` is not a type", namespace),
                            ErrorType::UndefinedSymbol,
                            namespace.pos,
                            ErrorDisplayType::Error,
                            vec![ErrorAnnotation::new(
                                Some(format!("`{}` is a `{}`, not a type", namespace, other)),
                                namespace.pos,
                                ErrorDisplayType::Error,
                            )],
                            true,
                        ),
                        ErrorLevel::TypeError,
                    ));
                }
            },
            None => {}
        }
        Err(ErrorOrVec::Error(
            Error::new(
                "type does not exist".to_string(),
                ErrorType::UndefinedSymbol,
                namespace.pos,
                ErrorDisplayType::Error,
                vec![ErrorAnnotation::new(
                    Some(format!("undefined type `{}`", namespace)),
                    namespace.pos,
                    ErrorDisplayType::Error,
                )],
                true,
            ),
            ErrorLevel::NoneExistentValue,
        ))
    }

    fn get_basic_type(
        &self,
        namespace: Rc<Namespace<'a>>,
    ) -> Result<&SymbTabObj<'a>, ErrorOrVec<'a>> {
        let mut type_val = self.get_type(namespace)?;
        while let SymbTabObj::CustomType(TypeCheckType {
            value: TypeCheckTypeType::CustomType(name),
            pos: _,
            inferred: _,
        }) = type_val
        {
            type_val = self.get_type(Rc::clone(name))?;
        }
        Ok(type_val)
    }

    fn get_variable(
        &self,
        namespace: Rc<Namespace<'a>>,
    ) -> Result<&SymbTabObj<'a>, ErrorOrVec<'a>> {
        match self.items.get(&namespace) {
            Some(val) => match val.deref() {
                SymbTabObj::Variable(_) => return Ok(val),
                other => {
                    return Err(ErrorOrVec::Error(
                        Error::new(
                            format!("`{}` is not a variable", namespace),
                            ErrorType::UndefinedSymbol,
                            namespace.pos,
                            ErrorDisplayType::Error,
                            vec![ErrorAnnotation::new(
                                Some(format!("`{}` is a `{}`, not a variable", namespace, other)),
                                namespace.pos,
                                ErrorDisplayType::Error,
                            )],
                            true,
                        ),
                        ErrorLevel::TypeError,
                    ));
                }
            },
            None => {}
        }
        Err(ErrorOrVec::Error(
            Error::new(
                "variable does not exist".to_string(),
                ErrorType::UndefinedSymbol,
                namespace.pos,
                ErrorDisplayType::Error,
                vec![ErrorAnnotation::new(
                    Some(format!("undefined variable `{}`", namespace)),
                    namespace.pos,
                    ErrorDisplayType::Error,
                )],
                true,
            ),
            ErrorLevel::NoneExistentValue,
        ))
    }
}
