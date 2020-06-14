// Name mangling
// Name mangling is done on all functions and function calls

// i.e.
// Number proceeding modifier (i.e. "N", "P", "R") is length
// N for "name"
// `List_my::get` -> N7List_my_N3get
// `List::my_get` -> N4List_N6my_get

// Types are encoded like
// P for "parameter type"
// A for "argument types"
// R for "return type"
// my_func (int, int) -> bool
// N7my_func_R6V4bool_A15P5V3int_P5V3int

// Tuples
// t for "tuple type"
// V for "type value"
// my_func (int, int) -> (bool, bool)
// N7my_func _ P5 V3int _ P5 V3int _ R14 t11 V3int_V3int

// Operator Overloading
// convert the tokens into characters:
//  + `add`  int, int -> int OR int -> int (infix operator)
//  - `sub`  int, int -> int OR int -> int (infix operator)
//  * `mul`  int, int -> int
//  / `div`  int, int -> float
//  % `mod`  int, int -> int
// %% `mdd`  (MoDulo Divisible)          int, int -> bool
//  > `grt`  (GReater Than)              int, int -> bool
//  < `lst`  (LeSs Than)                 int, int -> bool
//  >= `get` (Greater than or Equal To)  int, int -> bool
//  <= `let` (Less than or Equal To)     int, int -> bool
//  == `eqt` (EQual To)                  int, int -> bool
//
// O for "overloaded value"
// overload + add_ints(int, int) -> int
// O3add_N8add_ints_R5V3int_A15P5V3int_P5V3int

use crate::lexer::TokenType;
use crate::logger::logger::{
    Error, ErrorAnnotation, ErrorDisplayType, ErrorLevel, ErrorOrVec, ErrorType,
};
use crate::parser::ast;
use crate::typecheck::ast_typecheck;

use std::rc::Rc;

pub(crate) fn gen_manged_args<'a, 'b>(
    types: &[&ast_typecheck::TypeCheckType<'a>],
    context: &'b ast_typecheck::TypeCheckSymbTab<'a>,
) -> String {
    let mangled_args = types
        .into_iter()
        .map(|arg_type| {
            let arg_mangled = arg_type.mangle(context);
            format!("P{}{}", arg_mangled.len(), arg_mangled)
        })
        .collect::<Vec<_>>()
        .join("_");
    format!("_A{}{}", mangled_args.len(), mangled_args)
}

impl<'a> TokenType<'a> {
    pub(crate) fn mangle(&self) -> String {
        let mangled_op = match self {
            TokenType::SUB => "sub",
            TokenType::ADD => "add",
            TokenType::DIV => "div",
            TokenType::DMOD => "mdd",
            TokenType::MOD => "mod",
            TokenType::MUL => "mul",
            TokenType::GT => "grt",
            TokenType::LT => "lst",
            TokenType::GE => "get",
            TokenType::LE => "let",
            TokenType::EQ => "eqt",
            _ => panic!("{} cannot be mangled", self),
        };

        format!("O{}{}_", mangled_op.len(), mangled_op)
    }
}

impl<'a> ast::Namespace<'a> {
    /// Mangle function name without types
    /// Mangles name and returns as string
    pub(crate) fn mangle(&self) -> String {
        self.scopes
            .iter()
            .map(|name| {
                let name_mangled = name.mangle();
                format!("N{}{}", name_mangled.len(), name_mangled)
            })
            .collect::<Vec<_>>()
            .join("_")
    }

    /// Mangle operator overload function
    pub(crate) fn mangle_overload<'b>(
        &self,
        types: &[&ast_typecheck::TypeCheckType<'a>],
        return_type: Option<&ast_typecheck::TypeCheckType<'a>>,
        token: TokenType<'a>,
        context: &'b ast_typecheck::TypeCheckSymbTab<'a>,
    ) -> Result<String, ErrorOrVec<'a>> {
        let mut mangled = token.mangle();
        mangled += &self.mangle()[..];

        match return_type {
            Some(ret_type) => {
                let ret_type_mangled = ret_type.mangle(context);
                mangled += &format!("_R{}{}", ret_type_mangled.len(), ret_type_mangled)[..];
            }
            None => {
                let values = context
                    .get_prefix_string(&mangled[..])
                    .into_iter()
                    .collect::<Vec<_>>();
                let length = values.len();
                if length == 1 {
                    let ret_type_mangled = &values
                        .first()
                        .unwrap()
                        .1
                        .unwrap_function_ref()
                        .0
                        .value
                        .unwrap_func_return_ref()
                        .mangle(context)[..];
                    mangled += &format!("_R{}{}", ret_type_mangled.len(), ret_type_mangled)[..];
                } else if length == 0 {
                    mangled.clear();
                } else {
                    let mut err = vec![ErrorAnnotation::new(
                        Some(format!("function `{}` used here", self.to_string())),
                        self.pos,
                        ErrorDisplayType::Error,
                    )];

                    err.append(
                        &mut values
                            .into_iter()
                            .map(|(_, type_val)| {
                                let ret_type = type_val
                                    .unwrap_function_ref()
                                    .0
                                    .value
                                    .unwrap_func_return_ref();
                                ErrorAnnotation::new(
                                    Some(format!("possible return signature of {}", ret_type)),
                                    self.pos,
                                    ErrorDisplayType::Error,
                                )
                            })
                            .collect::<Vec<_>>(),
                    );

                    // Cannot infer value to use
                    return Err(ErrorOrVec::Error(
                        Error::new(
                            "cannot infer type return type of function call".to_string(),
                            ErrorType::InferError,
                            self.pos,
                            ErrorDisplayType::Error,
                            err,
                            true,
                        ),
                        ErrorLevel::TypeError,
                    ));
                }
            }
        }

        mangled += &gen_manged_args(types, context)[..];

        Ok(mangled)
    }

    /// Mangle function name with types
    /// Mangles name and returns as string
    pub(crate) fn mangle_types<'b>(
        &self,
        types: &[&ast_typecheck::TypeCheckType<'a>],
        return_type: Option<&ast_typecheck::TypeCheckType<'a>>,
        context: &'b ast_typecheck::TypeCheckSymbTab<'a>,
    ) -> Result<String, ErrorOrVec<'a>> {
        let mut mangled = self.mangle();

        match return_type {
            Some(ret_type) => {
                let ret_type_mangled = ret_type.mangle(context);
                mangled += &format!("_R{}{}", ret_type_mangled.len(), ret_type_mangled)[..];
            }
            None => {
                let values = context
                    .get_prefix_string(&mangled[..])
                    .into_iter()
                    .collect::<Vec<_>>();
                let length = values.len();
                if length == 1 {
                    let ret_type_mangled = &values
                        .first()
                        .unwrap()
                        .1
                        .unwrap_function_ref()
                        .0
                        .value
                        .unwrap_func_return_ref()
                        .mangle(context)[..];
                    mangled += &format!("_R{}{}", ret_type_mangled.len(), ret_type_mangled)[..];
                } else if length == 0 {
                    mangled.clear();
                } else {
                    let mut err = vec![ErrorAnnotation::new(
                        Some(format!("function `{}` used here", self.to_string())),
                        self.pos,
                        ErrorDisplayType::Error,
                    )];

                    err.append(
                        &mut values
                            .into_iter()
                            .map(|(_, type_val)| {
                                let ret_type = type_val
                                    .unwrap_function_ref()
                                    .0
                                    .value
                                    .unwrap_func_return_ref();
                                ErrorAnnotation::new(
                                    Some(format!("possible return signature of {}", ret_type)),
                                    self.pos,
                                    ErrorDisplayType::Error,
                                )
                            })
                            .collect::<Vec<_>>(),
                    );

                    // Cannot infer value to use
                    return Err(ErrorOrVec::Error(
                        Error::new(
                            "cannot infer return type of function call".to_string(),
                            ErrorType::InferError,
                            self.pos,
                            ErrorDisplayType::Error,
                            err,
                            true,
                        ),
                        ErrorLevel::TypeError,
                    ));
                }
            }
        }

        mangled += &gen_manged_args(types, context)[..];

        Ok(mangled)
    }
}

impl<'a> ast_typecheck::TypeCheckType<'a> {
    /// Mangle typecheck type
    pub(crate) fn mangle<'b>(&self, context: &'b ast_typecheck::TypeCheckSymbTab<'a>) -> String {
        match &self.value {
            ast_typecheck::TypeCheckTypeType::SingleType(val) => val.mangle(),
            ast_typecheck::TypeCheckTypeType::CustomType(val, _) => context
                .get_type(Rc::clone(val))
                .unwrap()
                .unwrap_type_ref()
                .0
                .mangle(context),
            ast_typecheck::TypeCheckTypeType::TupleType(val) => {
                let tuple_items = val
                    .types
                    .iter()
                    .map(|type_val| {
                        let mangled = type_val.mangle(context);
                        format!("{}{}", mangled.len(), mangled)
                    })
                    .collect::<Vec<_>>()
                    .join("_");
                format!("t{}{}", tuple_items.len(), tuple_items)
            }
            _ => panic!("No name mangling for {}", self),
        }
    }
}

impl<'a> ast::TypeType<'a> {
    /// Mangle typecheck type
    pub(crate) fn mangle(&self) -> String {
        match self {
            ast::TypeType::Tuple(_) => panic!("no"),
            ast::TypeType::Type(type_val) => {
                let mangled = type_val.mangle();
                format!("V{}{}", mangled.len(), mangled)
            }
        }
    }
}

impl<'a> ast::NameID<'a> {
    /// Mangle function name + its types
    pub(crate) fn mangle(&self) -> &'a str {
        self.value
    }
}
