use super::AnnotationType;

use crate::typecheck::context::Context;
use crate::parser::ast;
use crate::logger::ErrorValue;

pub struct Annotator {
    /// Assign unique unknown types to be solved
    type_counter: usize,
    context: Context,
}

impl Annotator {
    pub fn new() -> Self {
        Annotator {
            type_counter: 0,
            context: Context::new()
        }
    }

    pub fn annotate(&mut self, ast: Vec<ast::Statement>) -> Result<Vec<super::TypedStmt>, ErrorValue> {
        Ok(Vec::new())
    }

    pub fn unique(&mut self) -> AnnotationType {
        self.type_counter += 1;
        AnnotationType::Infer(self.type_counter)
    }
}

#[cfg(test)]
pub mod AnnotatorTests {
    use super::*;
    
    macro_rules! assert_infer {
        ($left: expr, $right: expr) => {
            match $right {
                AnnotationType::Infer(value) => {
                    assert_eq!($left, value);
                }
                _ => panic!("Not an infer node")
            }
        }
    }

    #[test]
    fn test_unique() {
        let mut annotator = Annotator::new();
        let _1 = annotator.unique();
        let _2 = annotator.unique();
        let _3 = annotator.unique();
        let _4 = annotator.unique();

        assert_infer!(1, _1);
        assert_infer!(2, _2);
        assert_infer!(3, _3);
        assert_infer!(4, _4);
    }
}
