pub mod helpers;
pub mod lexer;
pub mod logger;
pub mod parser;
pub mod codegen;
pub mod master;

#[macro_use]
extern crate clap;
use clap::App;
use inkwell::context::Context;

fn main() {
    let yaml = load_yaml!("cli.yml");
    let matches = App::from_yaml(yaml).get_matches();

    let filename = matches.value_of("entry").unwrap().to_string();
    let context = Context::create();

    let mut master = master::Master::new(&context);
    master.add_file(filename);
}
