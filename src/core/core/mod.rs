use crate::helpers;
use crate::logger::logger;
use crate::parser::{ast, parser};
use crate::paths;

use std::cell::RefCell;
use std::rc::Rc;

// Generate core modules
pub fn import_core<'a>() -> Vec<ast::Statement<'a>> {
    let core_path = helpers::get_core_loc();

    let mut contents = String::new();

    contents.push_str(&paths::read_file(&core_path)[..]);

    let file_contents = Box::leak(contents.into_boxed_str());
    let core_path_ref = Box::leak(core_path.into_boxed_path());

    let logger = Rc::new(RefCell::new(logger::Logger::new(true))); // The logger for the core is always verbose in the event of an error

    logger.borrow_mut().add_file(core_path_ref, file_contents);

    let mut parser = parser::Parser::new(core_path_ref, file_contents, Rc::clone(&logger));
    parser.initialize_expr();
    helpers::error_or_other(parser.parse(), logger);
    parser.ast.unwrap().nodes
}
