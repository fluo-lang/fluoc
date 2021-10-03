use std::fs::read_to_string;

mod common;
mod frontend;

fn main() {
    let file = read_to_string("tests/io/print.fl").unwrap();
    println!("{:?}", file);
}
