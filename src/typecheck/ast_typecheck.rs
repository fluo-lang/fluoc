use crate::helpers;
use crate::logger::logger::{Error, ErrorAnnotation, ErrorDisplayType, ErrorType};
use crate::parser::ast::*;

use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;

pub enum SymbTabObj<'a> {
    CustomType(TypeCheckType<'a>),
    Function(FunctionDefine<'a>),
    Variable(Expr<'a>),
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

#[derive(Debug, Clone)]
pub enum ErrorOrVec<'a> {
    Error(Error<'a>),
    ErrorVec(Vec<Error<'a>>),
}

impl<'a> ErrorOrVec<'a> {
    pub fn unwrap_error(self) -> Error<'a> {
        match self {
            ErrorOrVec::Error(e) => e,
            ErrorOrVec::ErrorVec(_) => panic!("Tried to unwrap ErrorVec value"),
        }
    }

    pub fn unwrap_vec(self) -> Vec<Error<'a>> {
        match self {
            ErrorOrVec::Error(_) => panic!("Tried to unwrap Error value"),
            ErrorOrVec::ErrorVec(e) => e,
        }
    }

    pub fn as_vec(self) -> Vec<Error<'a>> {
        match self {
            ErrorOrVec::Error(e) => vec![e],
            ErrorOrVec::ErrorVec(e) => e,
        }
    }
}

#[derive(Clone)]
pub struct TypeCheckSymbTab<'a> {
    items: HashMap<&'a Namespace<'a>, &'a SymbTabObj<'a>>,
}

#[derive(Debug, Clone)]
pub struct UnionType<'a> {
    types: Vec<TypeCheckType<'a>>,
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

#[derive(PartialEq, Debug, Clone)]
pub enum TypeCheckTypeType<'a> {
    FunctionSig(UnionType<'a>, Box<TypeCheckType<'a>>),
    SingleType(Type<'a>),
    TupleType(UnionType<'a>),
    ArrayType(Box<TypeCheckType<'a>>, Box<TypeCheckType<'a>>),
    CustomType(Namespace<'a>),
}

#[derive(Debug, Clone)]
pub struct TypeCheckType<'a> {
    value: TypeCheckTypeType<'a>,
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
    fn sequiv<'b>(left: &Self, right: &Self, context: &'b TypeCheckSymbTab<'a>) -> bool {
        match (left.cast_to_basic(context), right.cast_to_basic(context)) {
            (Ok(left_ok), Ok(right_ok)) => return left_ok == right_ok,
            _ => {}
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

    fn cast_to_basic<'b>(
        &'b self,
        context: &'b TypeCheckSymbTab<'a>,
    ) -> Result<&str, ErrorOrVec<'a>> {
        match &self.value {
            TypeCheckTypeType::FunctionSig(_, _) => Err(ErrorOrVec::Error(Error::new(
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
            ))),
            TypeCheckTypeType::SingleType(type_val) => {
                type_val.value.is_basic_type().or_else(|_| {
                    Err(ErrorOrVec::Error(Error::new(
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
                    )))
                })
            }
            TypeCheckTypeType::TupleType(_) => Err(ErrorOrVec::Error(Error::new(
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
            ))),
            TypeCheckTypeType::ArrayType(_, _) => Err(ErrorOrVec::Error(Error::new(
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
            ))),
            TypeCheckTypeType::CustomType(name_type) => {
                context.get_basic_type(&name_type)?.cast_to_basic(context)
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
                if match w[0].cast_to_basic(context) {
                    Ok(val) => val,
                    Err(e) => return Err(e),
                } == match w[1].cast_to_basic(context) {
                    Ok(val) => val,
                    Err(e) => return Err(e),
                } {
                    Ok(())
                } else {
                    return Err(ErrorOrVec::Error(Error::new(
                        "mismatched return type".to_string(),
                        ErrorType::TypeMismatch,
                        w[0].pos,
                        ErrorDisplayType::Error,
                        vec![
                            ErrorAnnotation::new(
                                Some(format!("Type `{}`...", w[0])),
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
                    )));
                }
            })
            .collect::<Result<(), ErrorOrVec>>()
    }

    fn as_type(val: Type<'a>) -> TypeCheckType<'a> {
        if let Ok(_) = val.value.is_basic_type() {
            TypeCheckType {
                pos: val.pos,
                inferred: val.inferred,
                value: TypeCheckTypeType::SingleType(val),
            }
        } else if let TypeType::Tuple(types) = val.value {
            TypeCheckType {
                value: TypeCheckTypeType::TupleType(UnionType {
                    types: types
                        .into_iter()
                        .map(|x| TypeCheckType::as_type(x))
                        .collect::<Vec<TypeCheckType>>(),
                    pos: val.pos,
                    inferred: val.inferred,
                }),
                pos: val.pos,
                inferred: val.inferred,
            }
        } else if let TypeType::Type(other) = val.value {
            TypeCheckType {
                value: TypeCheckTypeType::CustomType(other),
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

    fn construct_basic(type_name: &'a str, pos: helpers::Pos<'a>) -> TypeCheckType<'a> {
        TypeCheckType {
            value: TypeCheckTypeType::SingleType(Type {
                value: TypeType::Type(Namespace {
                    scopes: vec![NameID {
                        value: type_name,
                        pos,
                    }],
                    pos,
                }),
                pos,
                inferred: false,
            }),
            pos,
            inferred: false,
        }
    }

    fn from_expr<'b>(
        val: &Expr<'a>,
        context: &'b TypeCheckSymbTab<'a>,
    ) -> Result<TypeCheckType<'a>, ErrorOrVec<'a>> {
        Ok(match val {
            Expr::Tuple(tuple_val) => TypeCheckType {
                pos: tuple_val.pos,
                value: TypeCheckTypeType::TupleType(UnionType {
                    pos: tuple_val.pos,
                    types: tuple_val
                        .values
                        .iter()
                        .map(|x| TypeCheckType::from_expr(x, context))
                        .collect::<Result<Vec<TypeCheckType>, _>>()?,
                    inferred: false,
                }),
                inferred: false,
            },
            Expr::StringLiteral(string_val) => {
                TypeCheckType::construct_basic("str", string_val.pos)
            }
            Expr::Integer(int_val) => TypeCheckType::construct_basic("int", int_val.pos),
            Expr::FunctionCall(func_call) => {
                // Validate function call
                let func = context.get_function(&func_call.name)?;
                let func_arg_types: Vec<TypeCheckType> = func
                    .arguments
                    .positional
                    .iter()
                    .map(|argument| TypeCheckType::as_type(argument.1.clone()))
                    .collect();

                let func_call_arg_types = func_call
                    .arguments
                    .positional
                    .iter()
                    .map(|argument| TypeCheckType::from_expr(&argument, context))
                    .collect::<Result<Vec<TypeCheckType>, _>>()?;

                for (real_type, call_type) in func_arg_types.iter().zip(&func_call_arg_types) {
                    // Check if called argument type matches with expected type
                    if real_type != call_type {
                        return Err(ErrorOrVec::Error(Error::new(
                            "type mismatch between function and function call".to_string(),
                            ErrorType::TypeMismatch,
                            func_call.pos,
                            ErrorDisplayType::Error,
                            vec![
                                ErrorAnnotation::new(
                                    Some(format!(
                                        "`{}` has arguments `{}`",
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
                                        "`{}` called with `{}`",
                                        " ".repeat(func_call.name.to_string().chars().count() + 2),
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
                        )));
                    }
                }

                TypeCheckType::as_type(func.return_type.clone())
            }
            Expr::VariableAssign(variable_assignment) => {
                // TODO: validate that the variable is declared and has same type
                TypeCheckType::from_expr(&*variable_assignment.expr, context)?
            }
            Expr::VariableAssignDeclaration(variable_assignment_dec) => {
                TypeCheckType::from_expr(&*variable_assignment_dec.expr, context)?
            }
            Expr::Infix(infix) => {
                let left_type = TypeCheckType::from_expr(&*infix.left, context)?;
                let right_type = TypeCheckType::from_expr(&*infix.right, context)?;

                // TODO: check if the types can be casted to on another
                if left_type == right_type {
                    // Exact same type
                    left_type
                } else {
                    // Operands are not basic types
                    // TODO: implement checking for namespace like types (custom types)
                    // For now, an error will be raised

                    return Err(ErrorOrVec::Error(Error::new(
                        format!("type mismatch between operands of {}", infix.operator.token),
                        ErrorType::TypeMismatch,
                        infix.pos,
                        ErrorDisplayType::Error,
                        vec![
                            ErrorAnnotation::new(
                                Some(format!("Has type `{}`", left_type)),
                                left_type.pos,
                                ErrorDisplayType::Info,
                            ),
                            ErrorAnnotation::new(
                                Some(format!("Has type `{}`", right_type)),
                                right_type.pos,
                                ErrorDisplayType::Info,
                            ),
                        ],
                        true,
                    )));
                }
            }
            _ => panic!("Type expression inference not implemented yet"),
        })
    }
}

pub trait TypeCheck<'a>: std::fmt::Debug {
    fn type_check<'b>(
        &self,
        _return_type: Option<Cow<'a, TypeCheckType<'a>>>,
        _context: &'b TypeCheckSymbTab<'a>,
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
        ErrorOrVec::Error(Error::new(
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
                    Some(format!("Found return type `{}`...", returned_type)),
                    returned_type.pos,
                    ErrorDisplayType::Error,
                ),
            ],
            true,
        ))
    }

    fn construct_err(
        &self,
        value: TypeCheckType<'a>,
        returned_type: TypeCheckType<'a>,
    ) -> ErrorOrVec<'a> {
        ErrorOrVec::Error(Error::new(
            "mismatched return type".to_string(),
            ErrorType::TypeMismatch,
            self.expression.pos(),
            ErrorDisplayType::Error,
            vec![
                ErrorAnnotation::new(
                    Some(format!("Return type `{}` found here...", value)),
                    value.pos,
                    ErrorDisplayType::Info,
                ),
                ErrorAnnotation::new(
                    Some(format!("but found `{}` instead", returned_type)),
                    returned_type.pos,
                    ErrorDisplayType::Error,
                ),
            ],
            true,
        ))
    }
}

impl<'a> TypeCheck<'a> for Return<'a> {
    fn type_check<'b>(
        &self,
        return_type: Option<Cow<'a, TypeCheckType<'a>>>,
        context: &'b TypeCheckSymbTab<'a>,
    ) -> Result<Cow<'a, TypeCheckType<'a>>, ErrorOrVec<'a>> {
        let returned_type = TypeCheckType::from_expr(&self.expression, context)?;
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
                        Err(self
                            .construct_implicit_err(return_type_some.into_owned(), returned_type))
                    } else if !tuple_returned_contents.types.is_empty()
                        && !return_type_some.inferred
                    {
                        Err(self.construct_err(return_type_some.into_owned(), returned_type))
                    } else {
                        Ok(return_type_some)
                    }
                } else {
                    if return_type_some.inferred {
                        Err(self
                            .construct_implicit_err(return_type_some.into_owned(), returned_type))
                    } else {
                        Err(self.construct_err(return_type_some.into_owned(), returned_type))
                    }
                }
            }
            Some(value) => {
                // Make sure returns are of the same type or can be casted
                if returned_type == *value {
                    // Same type
                    Ok(value)
                } else if {
                    let possible_basic = returned_type.cast_to_basic(context);
                    let value_basic = value.cast_to_basic(context);
                    possible_basic.is_ok()
                        && value_basic.is_ok()
                        && (possible_basic.unwrap() == value_basic.unwrap())
                } {
                    // Same basic type
                    Ok(value)
                } else {
                    Err(self.construct_err(value.into_owned(), returned_type))
                }
            }
            None => {
                // No required return type (i.e. in outer scope)
                Ok(Cow::Owned(TypeCheckType::from_expr(
                    &self.expression,
                    context,
                )?))
            }
        }
    }
}

impl<'a> TypeCheck<'a> for Block<'a> {
    fn type_check<'b>(
        &self,
        return_type: Option<Cow<'a, TypeCheckType<'a>>>,
        context: &'b TypeCheckSymbTab<'a>,
    ) -> Result<Cow<'a, TypeCheckType<'a>>, ErrorOrVec<'a>> {
        let mut errors: Vec<Error> = Vec::new();
        let mut ret_types: Vec<&Expr> = Vec::new();

        for node in &self.nodes {
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
                .iter()
                .map(|expr| Ok(TypeCheckType::from_expr(&expr, context)?))
                .collect::<Result<Vec<TypeCheckType<'a>>, ErrorOrVec>>()?;
            TypeCheckType::all_same_type(&ret_types[..], context)?;
            Ok(Cow::Owned(ret_types.into_iter().nth(0).unwrap()))
        } else {
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

impl<'a> TypeCheck<'a> for FunctionDefine<'a> {
    fn type_check<'b>(
        &self,
        _return_type: Option<Cow<'a, TypeCheckType<'a>>>,
        context: &'b TypeCheckSymbTab<'a>,
    ) -> Result<Cow<'a, TypeCheckType<'a>>, ErrorOrVec<'a>> {
        (&self.block as &dyn TypeCheck).type_check(
            Some(Cow::Owned(TypeCheckType::as_type(self.return_type.clone()))),
            context,
        )
    }
}

impl<'a> TypeCheck<'a> for Statement<'a> {
    fn type_check<'b>(
        &self,
        return_type: Option<Cow<'a, TypeCheckType<'a>>>,
        context: &'b TypeCheckSymbTab<'a>,
    ) -> Result<Cow<'a, TypeCheckType<'a>>, ErrorOrVec<'a>> {
        match &self {
            Statement::Return(statement) => {
                (statement as &dyn TypeCheck).type_check(return_type, context)
            }
            Statement::FunctionDefine(statement) => {
                (statement as &dyn TypeCheck).type_check(return_type, context)
            }
            _ => panic!(format!(
                "Type_check not implemented for statement `{}`",
                &self.to_string()
            )),
        }
    }
}

impl<'a> TypeType<'a> {
    fn is_basic_type(&self) -> Result<&str, ()> {
        match &self {
            TypeType::Type(Namespace { pos: _, scopes }) => {
                if let Some(val) = scopes.get(0) {
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

    fn get_function<'b>(
        &self,
        namespace: &Namespace<'a>,
    ) -> Result<&'b FunctionDefine<'a>, ErrorOrVec<'a>> {
        match self.items.get(namespace) {
            Some(val) => match val {
                SymbTabObj::Function(function) => return Ok(function),
                other => {
                    return Err(ErrorOrVec::Error(Error::new(
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
                    )));
                }
            },
            None => {}
        }
        Err(ErrorOrVec::Error(Error::new(
            "function does not exist".to_string(),
            ErrorType::UndefinedSymbol,
            namespace.pos,
            ErrorDisplayType::Error,
            vec![ErrorAnnotation::new(
                Some(format!("Undefined function `{}`", namespace)),
                namespace.pos,
                ErrorDisplayType::Error,
            )],
            true,
        )))
    }

    fn get_type(&self, namespace: &Namespace<'a>) -> Result<&TypeCheckType<'a>, ErrorOrVec<'a>> {
        match self.items.get(&namespace) {
            Some(val) => match val {
                SymbTabObj::CustomType(type_val) => return Ok(type_val),
                other => {
                    return Err(ErrorOrVec::Error(Error::new(
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
                    )));
                }
            },
            None => {}
        }
        Err(ErrorOrVec::Error(Error::new(
            "type does not exist".to_string(),
            ErrorType::UndefinedSymbol,
            namespace.pos,
            ErrorDisplayType::Error,
            vec![ErrorAnnotation::new(
                Some(format!("Undefined type `{}`", namespace)),
                namespace.pos,
                ErrorDisplayType::Error,
            )],
            true,
        )))
    }

    fn get_basic_type(
        &self,
        namespace: &Namespace<'a>,
    ) -> Result<&TypeCheckType<'a>, ErrorOrVec<'a>> {
        let mut type_val = self.get_type(namespace)?;
        while let TypeCheckType {
            value: TypeCheckTypeType::CustomType(name),
            pos: _,
            inferred: _,
        } = type_val
        {
            type_val = self.get_type(name)?;
        }
        Ok(type_val)
    }

    fn get_variable(&self, namespace: &Namespace<'a>) -> Result<&Expr<'a>, ErrorOrVec<'a>> {
        match self.items.get(&namespace) {
            Some(val) => match val {
                SymbTabObj::Variable(variable) => return Ok(variable),
                other => {
                    return Err(ErrorOrVec::Error(Error::new(
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
                    )));
                }
            },
            None => {}
        }
        Err(ErrorOrVec::Error(Error::new(
            "Variable does not exist".to_string(),
            ErrorType::UndefinedSymbol,
            namespace.pos,
            ErrorDisplayType::Error,
            vec![ErrorAnnotation::new(
                Some(format!("Undefined variable `{}`", namespace)),
                namespace.pos,
                ErrorDisplayType::Error,
            )],
            true,
        )))
    }
}
