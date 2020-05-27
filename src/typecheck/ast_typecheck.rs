use crate::core;
use crate::helpers;
use crate::logger::logger::{
    Error, ErrorAnnotation, ErrorDisplayType, ErrorLevel, ErrorOrVec, ErrorType,
};
use crate::parser::ast::*;

use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;
use std::iter::Extend;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum SymbTabObj<'a> {
    CustomType(TypeCheckType<'a>),
    Function(TypeCheckType<'a>, helpers::Pos<'a>),
    Variable(TypeCheckType<'a>, bool),
}

impl<'a> SymbTabObj<'a> {
    fn unwrap_variable_ref(&self) -> (&TypeCheckType<'a>, bool) {
        match self {
            SymbTabObj::Variable(expr, safe) => (expr, *safe),
            _ => panic!(
                "Tried to unwrap variable from symbtab object, got `{:?}`",
                self
            ),
        }
    }
    fn unwrap_type_ref(&self) -> &TypeCheckType<'a> {
        match self {
            SymbTabObj::CustomType(val) => val,
            _ => panic!("Tried to unwrap type from symbtab object, got `{:?}`", self),
        }
    }

    fn unwrap_function_ref(&self) -> (&TypeCheckType<'a>, helpers::Pos<'a>) {
        match self {
            SymbTabObj::Function(val, pos) => (val, *pos),
            _ => panic!(
                "Tried to unwrap varialbe from symbtab object, got `{:?}`",
                self
            ),
        }
    }

    pub fn pos(&self) -> helpers::Pos<'a> {
        match &self {
            SymbTabObj::CustomType(val) => val.pos,
            SymbTabObj::Function(val, _) => val.pos,
            SymbTabObj::Variable(val, _) => val.pos,
        }
    }
}

impl<'a> fmt::Display for SymbTabObj<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let representation = match self {
            SymbTabObj::CustomType(_) => "custom type",
            SymbTabObj::Function(_, _) => "function",
            SymbTabObj::Variable(_, _) => "variable",
        };
        write!(f, "{}", representation)
    }
}

#[derive(Clone, Debug)]
pub struct TypeCheckSymbTab<'a> {
    pub items: HashMap<Rc<Namespace<'a>>, SymbTabObj<'a>>,
    curr_prefix: Vec<NameID<'a>>, // Current Namespace to push
}

impl<'a> fmt::Display for TypeCheckSymbTab<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut val = String::new();
        for (key, value) in self.items.iter() {
            val += &format!("{}: {}\n", key, value)[..];
        }
        write!(f, "{}", val)
    }
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
    FunctionSig(ArgumentsTypeCheck<'a>, Box<TypeCheckType<'a>>, Visibility),
    SingleType(Rc<Type<'a>>),
    TupleType(UnionType<'a>),
    ArrayType(Box<TypeCheckType<'a>>, usize), // Length
    CustomType(Rc<Namespace<'a>>, Option<Box<TypeCheckType<'a>>>), // Single Type
    Placeholder,                              // For things with no return type, most statements
}

impl<'a> TypeCheckTypeType<'a> {
    fn unwrap_func_return(self) -> TypeCheckType<'a> {
        match self {
            TypeCheckTypeType::FunctionSig(_, ret_type, _) => *ret_type,
            _ => panic!("Tried to unwrap func return from type check type"),
        }
    }

    fn unwrap_func(&self) -> (&ArgumentsTypeCheck<'a>, &TypeCheckType<'a>, Visibility) {
        match self {
            TypeCheckTypeType::FunctionSig(arguments, ret_type, visibility) => {
                (arguments, ret_type, *visibility)
            }
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
            TypeCheckTypeType::FunctionSig(arguments, return_type, visibility) => {
                format!("{} ({}) -> {}", visibility, arguments, return_type)
            }
            TypeCheckTypeType::SingleType(type_val) => format!("{}", type_val),
            TypeCheckTypeType::TupleType(types) => format!("({})", types),
            TypeCheckTypeType::ArrayType(elements_types, length_type) => {
                format!("[{}; {}]", *elements_types, *length_type)
            }
            TypeCheckTypeType::CustomType(namespace, _) => format!("{}", namespace),
            TypeCheckTypeType::Placeholder => panic!("Tried to print placeholder value"),
        };
        write!(f, "{}", rep)
    }
}

impl<'a> TypeCheckType<'a> {
    /// Check for equivalence of types
    fn sequiv<'b>(
        left: &Self,
        right: &Self,
        context: &'b TypeCheckSymbTab<'a>,
    ) -> Result<bool, ErrorOrVec<'a>> {
        match (left.cast_to_basic(context), right.cast_to_basic(context)) {
            (Ok(left_ok), Ok(right_ok)) => return Ok(left_ok == right_ok),
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
                    return Ok(left_contents
                        .types
                        .iter()
                        .zip(&right_contents.types)
                        .map(|(left_val, right_val)| {
                            TypeCheckType::sequiv(left_val, right_val, context)
                        })
                        .collect::<Result<Vec<_>, _>>()? // Check if all values are ok
                        .iter()
                        .all(|x| *x)); // All values *are* ok, check if they are all true
                }
                _ => {}
            }
        }
        Ok(false)
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
            TypeCheckTypeType::FunctionSig(_, _, _) => Err(ErrorOrVec::Error(
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
            TypeCheckTypeType::CustomType(name_type, _) => {
                let val = context.get_basic_type(Rc::clone(name_type))?;
                val.unwrap_type_ref().cast_to_basic(context)
            }
            TypeCheckTypeType::Placeholder => {
                panic!("Tried to cast placeholder to basic type, found placeholder value")
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
                if TypeCheckType::sequiv(&w[0], &w[1], context)? {
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

    pub fn from_type<'b>(
        val: Rc<Type<'a>>,
        context: &'b mut TypeCheckSymbTab<'a>,
        is_none: bool, // Special case on whether to eval the custom type into a basic type or not
    ) -> Result<TypeCheckType<'a>, ErrorOrVec<'a>> {
        if let Ok(_) = val.value.is_basic_type() {
            Ok(TypeCheckType {
                pos: val.pos,
                inferred: val.inferred,
                value: TypeCheckTypeType::SingleType(val),
            })
        } else if let TypeType::Tuple(types) = &val.value {
            Ok(TypeCheckType {
                value: TypeCheckTypeType::TupleType(UnionType {
                    types: types
                        .into_iter()
                        .map(|x| TypeCheckType::from_type(Rc::clone(&x), context, is_none))
                        .collect::<Result<Vec<TypeCheckType>, _>>()?,
                    pos: val.pos,
                    inferred: val.inferred,
                }),
                pos: val.pos,
                inferred: val.inferred,
            })
        } else if let TypeType::Type(other) = &val.value {
            Ok(TypeCheckType {
                value: TypeCheckTypeType::CustomType(
                    Rc::clone(other),
                    if is_none {
                        None
                    } else {
                        Some(Box::new(
                            context
                                .get_basic_type(Rc::clone(&other))?
                                .unwrap_type_ref()
                                .clone(),
                        ))
                    },
                ),
                pos: val.pos,
                inferred: val.inferred,
            })
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

                // Name of function gotten and function signature
                let func_union_namespace = context.get_function(Rc::clone(&func_call.name))?;
                let func = func_union_namespace.0.unwrap_function_ref();

                func_call.name = func_union_namespace.1;

                if func.0.value.unwrap_func().2 == Visibility::Private
                    && func.0.pos.filename != func_call.pos.filename
                {
                    // Private function and not from the same file
                    return Err(ErrorOrVec::Error(
                        Error::new(
                            "use of private function".to_string(),
                            ErrorType::VisibilityError,
                            func_call.pos,
                            ErrorDisplayType::Error,
                            vec![
                                ErrorAnnotation::new(
                                    Some(format!("function `{}` defined here", func_call.name)),
                                    func.1,
                                    ErrorDisplayType::Info,
                                ),
                                ErrorAnnotation::new(
                                    Some("function used here".to_string()),
                                    func_call.pos,
                                    ErrorDisplayType::Error,
                                ),
                            ],
                            true,
                        ),
                        ErrorLevel::TypeError,
                    ));
                };
                let func_arg_types: Vec<&TypeCheckType> = func
                    .0
                    .value
                    .unwrap_func()
                    .0
                    .positional
                    .iter()
                    .map(|argument| &argument.1)
                    .collect();

                for (real_type, call_type) in func_arg_types.iter().zip(&func_call_arg_types) {
                    // Check if called argument type matches with expected type
                    if !TypeCheckType::sequiv(real_type, call_type, context)? {
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

                func.0.value.clone().unwrap_func_return()
            }
            Expr::VariableAssignDeclaration(variable_assignment_dec) => {
                let value_type =
                    TypeCheckType::from_expr(&mut *variable_assignment_dec.expr, context)?;
                let cast_type = TypeCheckOrType::as_typecheck_type(
                    Cow::Borrowed(&variable_assignment_dec.t),
                    context,
                )?;

                variable_assignment_dec.t =
                    if !TypeCheckType::sequiv(&cast_type, &value_type, context)? {
                        // Not the same type, error
                        return Err(ErrorOrVec::Error(
                            Error::new(
                                "cannot cast value to specified type annotation".to_string(),
                                ErrorType::TypeMismatch,
                                cast_type.pos,
                                ErrorDisplayType::Error,
                                vec![
                                    ErrorAnnotation::new(
                                        Some(format!("type annotation has type `{}`", cast_type)),
                                        cast_type.pos,
                                        ErrorDisplayType::Error,
                                    ),
                                    ErrorAnnotation::new(
                                        Some(format!("value has type `{}`", value_type)),
                                        value_type.pos,
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
                            SymbTabObj::Variable(type_val, true),
                        );

                        TypeCheckOrType::TypeCheckType(cast_type)
                    };
                variable_assignment_dec.t.unwrap_type_check_ref().clone()
            }
            Expr::VariableAssign(variable_assign) => {
                let value_type = TypeCheckType::from_expr(&mut *variable_assign.expr, context)?;
                let cast_type = context
                    .get_variable(Rc::clone(&variable_assign.name))?
                    .unwrap_variable_ref()
                    .0
                    .clone();
                variable_assign.type_val =
                    if !TypeCheckType::sequiv(&cast_type, &value_type, context)? {
                        // Not the same type, error
                        return Err(ErrorOrVec::Error(
                            Error::new(
                                "cannot cast value to specified type annotation".to_string(),
                                ErrorType::TypeMismatch,
                                cast_type.pos,
                                ErrorDisplayType::Error,
                                vec![
                                    ErrorAnnotation::new(
                                        Some(format!("type annotation has type `{}`", cast_type)),
                                        cast_type.pos,
                                        ErrorDisplayType::Info,
                                    ),
                                    ErrorAnnotation::new(
                                        Some(format!("value has type `{}`", value_type)),
                                        value_type.pos,
                                        ErrorDisplayType::Info,
                                    ),
                                ],
                                true,
                            ),
                            ErrorLevel::TypeError,
                        ));
                    } else {
                        let type_val =
                            TypeCheckType::from_expr(&mut variable_assign.expr, context)?;
                        context.set_value(
                            Rc::clone(&variable_assign.name),
                            SymbTabObj::Variable(type_val, true), // Value is safe, it's guaranteed to have a value
                        );

                        Some(cast_type)
                    };
                variable_assign.type_val.clone().unwrap()
            }
            Expr::Infix(infix) => {
                let left_type = TypeCheckType::from_expr(&mut *infix.left, context)?;
                let right_type = TypeCheckType::from_expr(&mut *infix.right, context)?;

                // TODO: check if the types can be casted to on another
                if TypeCheckType::sequiv(&left_type, &right_type, context)? {
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
                let variable = val.unwrap_variable_ref();
                if variable.1 {
                    // Safe value
                    variable.0.clone()
                } else {
                    // Unsafe, possible use of uninitialized value
                    return Err(ErrorOrVec::Error(
                        Error::new(
                            "use of possibly uninitialized variable".to_string(),
                            ErrorType::PossibleUninitVal,
                            ref_namespace.pos,
                            ErrorDisplayType::Error,
                            vec![ErrorAnnotation::new(
                                Some(format!(
                                    "`{}` used here",
                                    ref_namespace.value.deref().to_string()
                                )),
                                ref_namespace.pos,
                                ErrorDisplayType::Error,
                            )],
                            true,
                        ),
                        ErrorLevel::TypeError,
                    ));
                }
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
                if TypeCheckType::sequiv(&returned_type, &value, context)? {
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

    pub fn get_core<'b>(&mut self) -> Result<TypeCheckSymbTab<'a>, ErrorOrVec<'a>> {
        if !self.tags.iter().any(|&i| i.content.value == "no_core") {
            // Load core module
            match core::generate_symbtab() {
                Ok(val) => Ok(val),
                Err(e) => Err(ErrorOrVec::ErrorVec(
                    e.into_iter().map(|x| (x, ErrorLevel::CoreError)).collect(),
                )),
            }
        } else {
            Ok(TypeCheckSymbTab::new())
        }
    }

    pub fn process_tags<'b>(&mut self) -> Result<(), ErrorOrVec<'a>> {
        for i in 0..self.nodes.len() {
            match &self.nodes[i] {
                Statement::Tag(tag) => {
                    self.tags.push(*tag);
                    let mut placeholder = Statement::Empty(Empty { pos: self.pos });
                    std::mem::swap(&mut placeholder, &mut self.nodes[i]); // Replace node with placeholder
                }
                _ => {}
            }
        }
        Ok(())
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

        for mut node in &mut self.nodes {
            // First pass
            match &mut node {
                Statement::FunctionDefine(func_def) => func_def.generate_proto(context)?,
                Statement::Unit(unit) => {
                    unit.block.process_tags()?;

                    let mut std_lib = unit.block.get_core()?;
                    std::mem::swap(&mut std_lib.curr_prefix, &mut context.curr_prefix);

                    match node.type_check(None, &mut std_lib) {
                        Ok(_) => Ok(()),
                        Err(e) => Err(e),
                    }?;

                    context.items.extend(std_lib.items);
                    std::mem::swap(&mut std_lib.curr_prefix, &mut context.curr_prefix);
                }
                Statement::TypeAssign(type_assign) => {
                    type_assign.type_check(None, context)?;
                }
                _ => {}
            }
        }

        for node in &mut self.nodes {
            if let Statement::Unit(_) = node {
                continue;
            }

            match node.type_check(return_type.clone(), context) {
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
                .map(|expr| Ok(TypeCheckType::from_expr(expr, context)?))
                .collect::<Result<Vec<TypeCheckType<'a>>, ErrorOrVec>>()?;
            TypeCheckType::all_same_type(&ret_types[..], context)?;
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
    fn generate_proto<'b>(
        &mut self,
        context: &'b mut TypeCheckSymbTab<'a>,
    ) -> Result<(), ErrorOrVec<'a>> {
        let mut positional = Vec::new();
        for idx in 0..self.arguments.positional.len() {
            self.arguments.positional[idx] = (
                self.arguments.positional[idx].0,
                TypeCheckOrType::TypeCheckType(TypeCheckType::from_type(
                    (&self.arguments.positional[idx].1).unwrap_type(),
                    context,
                    false,
                )?),
            );
        }

        for argument in &self.arguments.positional {
            positional.push((argument.0, (argument.1.clone()).unwrap_type_check()))
        }

        self.return_type = TypeCheckOrType::TypeCheckType(TypeCheckType::from_type(
            self.return_type.unwrap_type(),
            context,
            false,
        )?);

        self.name = context.set_value(
            Rc::clone(&self.name),
            SymbTabObj::Function(
                TypeCheckType {
                    value: TypeCheckTypeType::FunctionSig(
                        ArgumentsTypeCheck {
                            positional,
                            pos: self.arguments.pos,
                        },
                        Box::new(self.return_type.clone().unwrap_type_check()),
                        self.visibility,
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
                },
                self.pos,
            ),
        );

        Ok(())
    }
}

impl<'a> TypeCheck<'a> for FunctionDefine<'a> {
    fn type_check<'b>(
        &mut self,
        _return_type: Option<Cow<'a, TypeCheckType<'a>>>,
        context: &'b mut TypeCheckSymbTab<'a>,
    ) -> Result<Cow<'a, TypeCheckType<'a>>, ErrorOrVec<'a>> {
        let mut context = context.clone();

        for argument in &self.arguments.positional {
            context.set_value(
                Namespace::from_name_id(argument.0),
                SymbTabObj::Variable((argument.1.clone()).unwrap_type_check(), true),
            );
        }

        (&mut self.block as &mut dyn TypeCheck).type_check(
            Some(Cow::Owned(TypeCheckOrType::as_typecheck_type(
                Cow::Borrowed(&self.return_type),
                &mut context,
            )?)),
            &mut context,
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

impl<'a> TypeCheck<'a> for Unit<'a> {
    fn type_check<'b>(
        &mut self,
        _return_type: Option<Cow<'a, TypeCheckType<'a>>>,
        context: &'b mut TypeCheckSymbTab<'a>,
    ) -> Result<Cow<'a, TypeCheckType<'a>>, ErrorOrVec<'a>> {
        context.push_curr_prefix(self.name.as_vec_nameid());
        (&mut self.block as &mut dyn TypeCheck).type_check(None, context)?;
        context.curr_prefix.pop();

        // Unit returns nothing
        Ok(Cow::Owned(TypeCheckType {
            value: TypeCheckTypeType::Placeholder,
            pos: self.pos,
            inferred: true,
        }))
    }
}

impl<'a> TypeCheck<'a> for VariableDeclaration<'a> {
    fn type_check<'b>(
        &mut self,
        _return_type: Option<Cow<'a, TypeCheckType<'a>>>,
        context: &'b mut TypeCheckSymbTab<'a>,
    ) -> Result<Cow<'a, TypeCheckType<'a>>, ErrorOrVec<'a>> {
        let type_val = TypeCheckType::from_type(self.t.unwrap_type(), context, false)?;
        context.set_value(
            Rc::clone(&self.name),
            SymbTabObj::Variable(type_val.clone(), false),
        );
        self.t = TypeCheckOrType::TypeCheckType(type_val);

        Ok(Cow::Owned(TypeCheckType {
            value: TypeCheckTypeType::Placeholder,
            pos: self.pos,
            inferred: true,
        }))
    }
}

impl<'a> TypeAssign<'a> {
    fn type_check<'b>(
        &mut self,
        _return_type: Option<Cow<'a, TypeCheckType<'a>>>,
        context: &'b mut TypeCheckSymbTab<'a>,
    ) -> Result<Cow<'a, TypeCheckType<'a>>, ErrorOrVec<'a>> {
        self.name = context.set_value(
            Rc::clone(&self.name),
            SymbTabObj::CustomType(self.value.unwrap_type_check_ref().clone()),
        );

        self.value = TypeCheckOrType::TypeCheckType(TypeCheckType::from_type(
            Rc::clone(&self.value.unwrap_type()),
            context,
            true,
        )?);

        // Returns nothing
        Ok(Cow::Owned(TypeCheckType {
            value: TypeCheckTypeType::Placeholder,
            pos: self.pos,
            inferred: true,
        }))
    }
}

impl<'a> TypeCheck<'a> for TypeAssign<'a> {
    fn type_check<'b>(
        &mut self,
        _return_type: Option<Cow<'a, TypeCheckType<'a>>>,
        _context: &'b mut TypeCheckSymbTab<'a>,
    ) -> Result<Cow<'a, TypeCheckType<'a>>, ErrorOrVec<'a>> {
        // Returns nothing
        Ok(Cow::Owned(TypeCheckType {
            value: TypeCheckTypeType::Placeholder,
            pos: self.pos,
            inferred: true,
        }))
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
            Statement::VariableDeclaration(statement) => {
                (statement as &mut dyn TypeCheck).type_check(return_type, context)
            }
            Statement::Unit(statement) => {
                (statement as &mut dyn TypeCheck).type_check(return_type, context)
            }
            Statement::TypeAssign(statement) => {
                (statement as &mut dyn TypeCheck).type_check(return_type, context)
            }
            Statement::Empty(_) => Ok(Cow::Owned(TypeCheckType {
                value: TypeCheckTypeType::Placeholder,
                pos: self.pos(),
                inferred: false,
            })),
            Statement::Tag(_) => Ok(Cow::Owned(TypeCheckType {
                value: TypeCheckTypeType::Placeholder,
                pos: self.pos(),
                inferred: false,
            })),
            _ => panic!(
                "Type_check not implemented for statement `{}`",
                &self.to_string()
            ),
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

impl<'a> Namespace<'a> {
    fn starts_with(&self, prefix: &[NameID]) -> bool {
        &self.scopes[..prefix.len()] == &prefix[..]
    }
}

impl<'a> TypeCheckSymbTab<'a> {
    pub fn new() -> TypeCheckSymbTab<'a> {
        TypeCheckSymbTab {
            items: HashMap::new(),
            curr_prefix: Vec::new(),
        }
    }
    pub fn set_value(
        &mut self,
        namespace: Rc<Namespace<'a>>,
        value: SymbTabObj<'a>,
    ) -> Rc<Namespace<'a>> {
        let adjusted_namespace = Rc::new(
            namespace
                .as_ref()
                .clone()
                .prepend_namespace(self.curr_prefix.clone()),
        );

        self.items.insert(Rc::clone(&adjusted_namespace), value);
        Rc::clone(&adjusted_namespace)
    }

    fn push_curr_prefix(&mut self, prefix: &mut Vec<NameID<'a>>) {
        self.curr_prefix.append(prefix);
    }

    pub fn get_prefix(self, prefix_match: &[NameID]) -> HashMap<Rc<Namespace<'a>>, SymbTabObj<'a>> {
        self.items
            .into_iter()
            .filter(|(key, _)| key.starts_with(prefix_match))
            .collect()
    }

    fn get_function(
        &self,
        namespace: Rc<Namespace<'a>>,
    ) -> Result<(&SymbTabObj<'a>, Rc<Namespace<'a>>), ErrorOrVec<'a>> {
        // Namespace of std
        match self.items.get(&namespace) {
            Some(val) => match val.deref() {
                SymbTabObj::Function(_, _) => return Ok((val, namespace)),
                _ => {}
            },
            None => {}
        }

        // Try other namespace
        let other = namespace
            .as_ref()
            .clone()
            .prepend_namespace(self.curr_prefix.clone());

        match self.items.get(&other) {
            Some(val) => match val.deref() {
                SymbTabObj::Function(_, _) => return Ok((val, Rc::new(other))),
                other => {
                    // Cannot get function with and without current prefix
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
            ErrorLevel::NonExistentFunc,
        ))
    }

    fn get_type(&self, namespace: Rc<Namespace<'a>>) -> Result<&SymbTabObj<'a>, ErrorOrVec<'a>> {
        // Namespace of std
        match self.items.get(&namespace) {
            Some(val) => match val.deref() {
                SymbTabObj::CustomType(_) => return Ok(val),
                _ => {}
            },
            None => {}
        }

        // Try other namespace
        let other = namespace
            .as_ref()
            .clone()
            .prepend_namespace(self.curr_prefix.clone());

        match self.items.get(&other) {
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
            ErrorLevel::NonExistentType,
        ))
    }

    fn get_basic_type(
        &self,
        namespace: Rc<Namespace<'a>>,
    ) -> Result<&SymbTabObj<'a>, ErrorOrVec<'a>> {
        let mut type_val = self.get_type(Rc::clone(&namespace))?;
        while let SymbTabObj::CustomType(TypeCheckType {
            value: TypeCheckTypeType::CustomType(name, _),
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
                SymbTabObj::Variable(_, _) => return Ok(val),
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
            ErrorLevel::NonExistentVar,
        ))
    }
}
