extern crate core;

mod ast;
mod lexer;
mod parser;
mod repl;
mod tokens;
use std::env;

use repl::*;

fn main() {
    println!(
        "Hello {}! This is the Monkey programming language!",
        env::var("USER").expect("'USER' environment variable not defined!")
    );
    println!("Feel free to type in commands:");
    start();
}
