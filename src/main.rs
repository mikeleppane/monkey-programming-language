extern crate core;

mod cli;
mod lexing;
mod parsing;
mod syntax_tree;
mod token;

use std::env;

use cli::repl::*;

fn main() {
    println!(
        "Hello {}! This is the Monkey programming language!",
        env::var("USER").unwrap()
    );
    println!("Feel free to type in commands:");
    start();
}
