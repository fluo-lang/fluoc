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

pub fn generate_symbtab<'a>(
    logger: Rc<RefCell<logger::Logger<'a>>>,
) -> Result<ast_typecheck::TypeCheckSymbTab<'a>, Vec<logger::Error<'a>>> {
    let mut core_path = match current_exe() {
        Ok(val) => val,
        Err(e) => paths::file_error(e, "fluo core lib"),
    };
    core_path.pop();
    core_path.pop();
    core_path.pop();

    core_path.push("src");
    core_path.push("core");

    let files: Vec<_> = match fs::read_dir(&core_path) {
        Ok(val) => val,
        Err(e) => paths::file_error(e, core_path.display()),
    }
    .filter(|x| {
        x.as_ref()
            .unwrap()
            .path()
            .extension()
            .unwrap_or(OsStr::new(""))
            .to_str()
            .unwrap_or("")
            == "fl"
    })
    .map(|x| x.unwrap().path())
    .collect();

    let mut contents = String::new();

    for filename in files {
        contents.push_str(&paths::read_file(&filename)[..]);
    }

    let file_contents = Box::leak(contents.into_boxed_str());
    let core_path_ref = Box::leak(core_path.into_boxed_path());

    logger.borrow_mut().add_file(core_path_ref, file_contents);

    let typechecker = helpers::error_or_other(
        typecheck::TypeCheckModule::new(core_path_ref, file_contents, Rc::clone(&logger), false),
        logger,
    );

    typechecker.get_symbols()
}
