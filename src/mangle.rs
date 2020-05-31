// Name mangling
// Name mangling is done on all functions and function calls

// Single underscore denotes `::`, double underscore is an escaped underscore in a normal name
// i.e.
// `List_my::get` -> List__my_get
// `List::my_get` -> List_my__get

// Types are encoded like
// my_func (int, int) -> int
//

use crate::parser::ast;
use crate::typecheck::ast_typecheck;

impl<'a> ast::Namespace<'a> {
    /// Mangle function name without types
    /// Mangles name and returns as string
    pub(crate) fn mangle(&self) -> String {
        self.scopes
            .iter()
            .map(|name| name.mangle())
            .collect::<Vec<_>>()
            .join("_")
    }
    /// Mangle function name with types
    /// Mangles name and returns as string
    pub(crate) fn mangle_types(
        &self,
        types: &[&ast_typecheck::TypeCheckType<'a>],
        return_type: Option<&ast_typecheck::TypeCheckType<'a>>,
    ) -> String {
        self.scopes
            .iter()
            .map(|name| name.mangle())
            .collect::<Vec<_>>()
            .join("_")
    }
}

impl<'a> ast::NameID<'a> {
    /// Mangle function name + its types
    /// Mangles name in place
    pub(crate) fn mangle(&self) -> String {
        self.value.replace("_", "__")
    }
}
