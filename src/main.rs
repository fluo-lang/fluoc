#![feature(backtrace)]
#![warn(rust_2018_idioms)]
pub mod codegen;
pub mod helpers;
pub mod lexer;
pub mod logger;
pub mod master;
pub mod parser;
pub mod typecheck;
pub mod paths;
pub mod core;

#[macro_use]
extern crate clap;

use clap::App;
use inkwell::context::Context;

use logger::buffer_writer::{Color, Font};

use std::backtrace;
use std::panic;
use std::path;
use std::time::Instant;

fn main() {
    let master_start = Instant::now();
    panic::set_hook(Box::new(|value| {
        let bt = backtrace::Backtrace::force_capture();
        eprintln!(
            "{}\n{}.\n{}This is likely a problem with the fluo compiler and not your code. Please report the issue to the fluo github: https://github.com/fluo-lang/fluo{}", 
            bt, 
            value, 
            Color::RED.to_string(), 
            Font::RESET.to_string()
        );
    }));
    let yaml = load_yaml!("cli.yml");
    let matches = App::from_yaml(yaml).get_matches();

    let filename = paths::process_str(matches.value_of("entry").unwrap());

    let context = Context::create();
    
    let read_file_start = Instant::now();
    let contents = paths::read_file(filename.as_path());
    let mut master = master::Master::new(&context, matches.is_present("verbose"));
    master.logger.borrow().log_verbose(&|| format!("{}: Read file", helpers::display_duration(read_file_start.elapsed()))); // Lazily run it so no impact on performance

    master.generate_file(
        filename.as_path(),
        &contents[..],
        path::Path::new(matches.value_of("output").unwrap_or("out.o")),
    );

    master.logger.borrow().log(format!("{}: All Done", helpers::display_duration(master_start.elapsed())));
}
