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

fn main() {
    let yaml = load_yaml!("cli.yml");
    let matches = App::from_yaml(yaml).get_matches();

    let filename = matches.value_of("entry").unwrap();
    let context = Context::create();

    let contents = helpers::read_file(filename);

    let mut master = master::Master::new(&context);
    master.add_file(filename, &contents[..]);
}
