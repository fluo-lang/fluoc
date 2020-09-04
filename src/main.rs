#![feature(backtrace)]
#![feature(bindings_after_at)]

#![warn(rust_2018_idioms)]

#[macro_use]
extern crate lazy_static;

#[macro_use]
pub mod sourcemap;

pub mod codegen;
pub mod helpers;
pub mod lexer;
pub mod logger;
//pub mod mangle;
pub mod master;
pub mod parser;
pub mod paths;
pub mod segmentation;
pub mod tags;
pub mod typecheck;
pub mod mir;

#[macro_use]
extern crate clap;

use clap::App;
use inkwell::context::Context;

use logger::{Color, Font};

use std::backtrace;
use std::panic;
use std::path;
use std::process;
use std::time::Instant;

fn main() {
    let master_start = Instant::now();
    panic::set_hook(Box::new(|value| {
        let bt = backtrace::Backtrace::force_capture();
        eprintln!(
            "{}\n{}.\n{}This is likely a problem with the fluo compiler and not your code. Please report the issue to the fluo github: https://github.com/fluo-lang/fluoc{}",
            bt,
            value,
            Color::Red,
            Font::Reset
        );
    }));
    let yaml = load_yaml!("cli.yml");
    let matches = App::from_yaml(yaml).get_matches();

    if matches.is_present("version") {
        println!(
            "{}You are using fluo version 0.0.1{}",
            Color::Blue,
            Font::Reset
        );
        process::exit(0);
    }

    let filename = paths::process_str(matches.value_of("entry").unwrap());

    let context = Context::create();

    let read_file_start = Instant::now();
    let mut contents = paths::read_file(filename.as_path());

    // Load prelude
    let mut prelude_path: path::PathBuf = helpers::CORE_LOC.to_owned();
    prelude_path.pop();
    prelude_path.pop();
    prelude_path.push("prelude.fl");

    let prelude = paths::read_file(&prelude_path);
    // Append prelude into contents
    contents += &prelude[..];

    let mut master = master::Master::new(&context, matches.is_present("verbose"));
    master.logger.borrow().log_verbose(&|| {
        format!(
            "{}: Read file",
            helpers::display_duration(read_file_start.elapsed())
        )
    }); // Lazily run it so no impact on performance

    master.generate_file(
        filename,
        contents,
        path::Path::new("out.ll"),
        path::Path::new("out.o"),
    );

    master.logger.borrow().log(format!(
        "{}: All Done",
        helpers::display_duration(master_start.elapsed())
    ));
}
