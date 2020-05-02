use crate::logger::logger::{Error, ErrorAnnotation, ErrorDisplayType, ErrorType};
use crate::parser::ast::*;

pub enum ErrorOrVec<'a> {
    VecError(Vec<Error<'a>>),
    Error(Error<'a>),
}

impl<'a> ErrorOrVec<'a> {
    pub fn unwrap(self) -> Error<'a> {
        match self {
            ErrorOrVec::VecError(_) => {
                panic!("Tried to unwrap `Error` from `ErrorOrVec`, got `VecError`");
            }
            ErrorOrVec::Error(e) => e,
        }
    }

    pub fn unwrap_other(self) -> Vec<Error<'a>> {
        match self {
            ErrorOrVec::Error(_) => {
                panic!("Tried to unwrap `VecError` from `ErrorOrVec`, got `Error`");
            }
            ErrorOrVec::VecError(e) => e,
        }
    }
}

pub trait TypeCheck<'a> {
    fn type_check(&self, return_type: Option<&Type>) -> Result<(), ErrorOrVec<'a>> {
        Ok(())
    }
}

impl<'a> TypeCheck<'a> for Block<'a> {
    fn type_check(&self, return_type: Option<&Type>) -> Result<(), ErrorOrVec<'a>> {
        let mut errors: Vec<Error> = Vec::new();
        let mut returns = false;

        for node in &self.nodes {
            if let Statement::Return(_) = node {
                returns = true;
            }

            match node.type_check(return_type) {
                Ok(_) => {}
                Err(e) => {
                    errors.push(e.unwrap());
                }
            }
        }

        match return_type {
            Some(Type {
                value: TypeType::Tuple(tuple_contents),
                inferred: _,
                pos: _,
            }) if tuple_contents.is_empty() => {
                // The function returns (), so we can implicitly do this at the end. This is the negate case for below.
                if !returns {}
            }
            Some(Type {
                value: _,
                inferred,
                pos,
            }) => {
                // A case that is not the explicit `()` case
            }
            None => {}
        }

        Ok(())
    }
}

impl<'a> TypeCheck<'a> for FunctionDefine<'a> {
    fn type_check(&self, return_type: Option<&Type>) -> Result<(), ErrorOrVec<'a>> {
        (&self.block as &dyn TypeCheck).type_check(Some(&self.return_type))?;

        Ok(())
    }
}

impl<'a> TypeCheck<'a> for Statement<'a> {}
