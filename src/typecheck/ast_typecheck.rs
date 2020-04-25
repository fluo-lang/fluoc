use crate::parser::ast::*;
use crate::logger::logger::{ Error, ErrorAnnotation, ErrorDisplayType, ErrorType };

pub enum ErrorOrVec {
    VecError(Vec<Error>),
    Error(Error)
}

impl ErrorOrVec {
    pub fn unwrap(self) -> Error {
        match self {
            ErrorOrVec::VecError(_) => { panic!("Tried to unwrap `Error` from `ErrorOrVec`, got `VecError`"); },
            ErrorOrVec::Error(e) => { return e }
        }
    }

    pub fn unwrap_other(self) -> Vec<Error> {
        match self {
            ErrorOrVec::Error(_) => { panic!("Tried to unwrap `VecError` from `ErrorOrVec`, got `Error`"); },
            ErrorOrVec::VecError(e) => { return e; }
        }
    }
}

pub trait TypeCheck { 
    fn type_check(&self, return_type: &Option<&Type>) -> Result<(), ErrorOrVec> {
        Ok(())
    }
}

impl TypeCheck for Block {
    fn type_check(&self, return_type: &Option<&Type>) -> Result<(), ErrorOrVec> {
        let mut errors: Vec<Error> = Vec::new();
        let mut returns = false;

        for node in &self.nodes {
            if let Statement::Return(_) = node {
                returns = true;
            }

            match node.type_check(return_type) {
                Ok(_) => {  },
                Err(e) => { errors.push(e.unwrap()); }
            }
        }

        match return_type {
            Some(Type { value: TypeType::Tuple(tuple_contents), inferred: _, pos: _ }) if tuple_contents.is_empty() => {
                // The function returns (), so we can implicitly do this at the end. This is the negate case for below.
                if !returns {
                    /*self.nodes.push(
                        Statement::Return(
                            Return {
                                expression: 
                            }
                        )
                    )*/
                }
            },
            Some(Type { value: _, inferred: inferred, pos: pos }) => {
                
            },
            None => {  }
        }

        Ok(())
    }
}

impl TypeCheck for FunctionDefine {
    fn type_check(&self, return_type: &Option<&Type>) -> Result<(), ErrorOrVec> {
        
        (&self.block as &dyn TypeCheck).type_check(&Some(&self.return_type))?;

        Ok(())
    }
}

impl TypeCheck for Statement {

}
