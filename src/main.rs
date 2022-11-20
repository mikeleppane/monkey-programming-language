extern crate core;

mod ast;
mod lexer;
mod parser;
mod repl;
mod token;

use std::env;

use repl::repl::*;

fn main() {
    println!(
        "Hello {}! This is the Monkey programming language!",
        env::var("USER").unwrap()
    );
    println!("Feel free to type in commands:");
    start();
}
