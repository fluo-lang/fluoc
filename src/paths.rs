use crate::logger::buffer_writer::color;

use std::fs;
use std::path;
use std::process;

pub fn file_error(message: impl std::fmt::Display, filename: impl std::fmt::Display) -> ! {
    eprintln!(
        "{}{}Fluo: Fatal Error{}{}: {}: {}{}",
        color::RED,
        color::BOLD,
        color::RESET,
        color::BOLD,
        message,
        filename,
        color::RESET
    );
    process::exit(1);
}

pub fn canonicalize_file<'a>(file_path: &'a path::Path) -> path::PathBuf {
    match file_path.canonicalize() {
        Ok(val) => val,
        Err(e) => file_error(e, file_path.display()),
    }
}

pub fn get_parent(file_path: path::PathBuf) -> path::PathBuf {
    match (&file_path).parent() {
        Some(val) => val.to_path_buf(),
        None => file_error("Unable to get parent", file_path.display()),
    }
}

pub fn path_to_str<'a>(file_path: &'a path::Path) -> &'a str {
    match file_path.to_str() {
        Some(val) => val,
        None => file_error("Path is not valid UTF-8", file_path.display()),
    }
}

pub fn read_file(filename: &path::Path) -> String {
    let f = fs::read_to_string(filename);
    match f {
        Ok(file_cont) => file_cont,
        Err(e) => file_error(e, filename.display()),
    }
}

pub fn read_file_leak(filename: &path::Path) -> &'static str {
    Box::leak(read_file(filename).into_boxed_str())
}

pub fn process_str(filename_base: &str) -> path::PathBuf {
    let file_unexpanded = &canonicalize_file(path::Path::new(filename_base));

    let expanded_path = match shellexpand::full(path_to_str(file_unexpanded)) {
        Ok(str_path) => str_path.into_owned(),
        Err(e) => file_error(e, file_unexpanded.display()),
    };

    path::PathBuf::from(expanded_path)
}
