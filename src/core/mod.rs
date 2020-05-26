pub mod fmt;
use crate::helpers;
use crate::logger::logger;
use crate::paths;
use crate::typecheck::{ast_typecheck, typecheck};

use std::cell::RefCell;
use std::env::current_exe;
use std::ffi::OsStr;
use std::fs;
use std::rc::Rc;

// Generate core modules
pub fn generate_symbtab<'a>() -> Result<ast_typecheck::TypeCheckSymbTab<'a>, Vec<logger::Error<'a>>>
{
    let mut core_path = match current_exe() {
        Ok(val) => val,
        Err(e) => paths::file_error(e, "fluo core lib"),
    };
    core_path.pop();
    core_path.pop();
    core_path.pop();

    core_path.push("src");
    core_path.push("core");
    core_path.push("core.fl");


    let mut contents = String::new();

    contents.push_str(&paths::read_file(&core_path)[..]);

    let file_contents = Box::leak(contents.into_boxed_str());
    let core_path_ref = Box::leak(core_path.into_boxed_path());

    let logger = Rc::new(RefCell::new(logger::Logger::new(true))); // The logger for the core is always verbose in the event of an error

    logger.borrow_mut().add_file(core_path_ref, file_contents);

    let typechecker = helpers::error_or_other(
        typecheck::TypeCheckModule::new(core_path_ref, file_contents, Rc::clone(&logger), false),
        logger,
    );

    typechecker.get_symbols()
}
