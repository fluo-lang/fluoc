#![feature(backtrace)]
pub mod codegen;
pub mod helpers;
pub mod lexer;
pub mod logger;
pub mod master;
pub mod parser;
pub mod typecheck;

#[macro_use]
extern crate clap;
use clap::App;
use inkwell::context::Context;

use logger::buffer_writer::{Color, Font};

use std::backtrace;
use std::panic;

fn main() {
    panic::set_hook(Box::new(|value| {
        let bt = backtrace::Backtrace::force_capture();
        println!("{}\n{}.\n{}This is likely a problem with the fluo compiler and not your code. Please report the issue to the fluo github.{}", bt, value, Color::RED.to_string(), Font::RESET.to_string());
    }));
    let yaml = load_yaml!("cli.yml");
    let matches = App::from_yaml(yaml).get_matches();

    let filename = matches.value_of("entry").unwrap();
    let context = Context::create();

    let contents = helpers::read_file(filename);

    let mut master = master::Master::new(&context);
    master.add_file(filename, &contents[..]);
}
