use crate::lexing::lexer::*;
use crate::token::tokens::Token;
use std::io;
use std::io::prelude::*;

const PROMPT: &str = ">> ";
const EXIT: &str = "exit";

pub fn start() {
    let stdin = io::stdin();
    print!("{}", PROMPT);
    io::stdout().flush().unwrap();
    for line_result in stdin.lock().lines() {
        let line = line_result.unwrap();
        let line_trimmed = line.trim();
        if line_trimmed == EXIT {
            return;
        }
        let mut lexer = Lexer::new(line_trimmed);
        loop {
            let tok = lexer.next_token();
            if let Token::Empty = tok {
                break;
            }
            println!("{:#?}", tok);
        }
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
    }
}
