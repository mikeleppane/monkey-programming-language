#![allow(dead_code)]
#![allow(non_camel_case_types)]

use std::fmt::Debug;

pub const ILLEGAL: &str = "ILLEGAL";
pub const EOF: &str = "EOF";
pub const IDENT: &str = "IDENT";
pub const INT: &str = "INT";
pub const ASSIGN: &str = "=";
pub const PLUS: &str = "+";
pub const COMMA: &str = ",";
pub const SEMICOLON: &str = ";";
pub const LPAREN: &str = "(";
pub const RPAREN: &str = ")";
pub const LBRACE: &str = "{";
pub const RBRACE: &str = "}";
pub const FUNCTION: &str = "FUNCTION";
pub const LET: &str = "let";
pub const EMPTY: &str = "";
pub const MINUS: &str = "-";
pub const BANG: &str = "!";
pub const ASTERISK: &str = "*";
pub const SLASH: &str = "/";
pub const LT: &str = "<";
pub const GT: &str = ">";
pub const EQ: &str = "==";
pub const NOT_EQ: &str = "!=";
pub const TRUE: &str = "true";
pub const FALSE: &str = "false";
pub const IF: &str = "if";
pub const ELSE: &str = "else";
pub const RETURN: &str = "return";

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    Assign,
    Semicolon,
    Illegal(String),
    Eof,
    Ident(String),
    Int(String),
    Plus,
    Comma,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Function,
    Let,
    Empty,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LT,
    GT,
    True,
    False,
    IF,
    Else,
    Return,
    EQ,
    NOT_EQ,
}

impl Token {
    pub(crate) fn lookup_ident(ident: &str) -> Token {
        match ident {
            "fn" => Token::Function,
            "let" => Token::Let,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::IF,
            "else" => Token::Else,
            "return" => Token::Return,
            _ => Token::Ident(ident.to_string()),
        }
    }

    pub(crate) fn literal(&self) -> &str {
        match self {
            Token::Assign => ASSIGN,
            Token::Semicolon => SEMICOLON,
            Token::Illegal(value) => value,
            Token::Eof => EOF,
            Token::Ident(value) => value,
            Token::Int(value) => value,
            Token::Plus => PLUS,
            Token::Minus => MINUS,
            Token::Comma => COMMA,
            Token::Lparen => LPAREN,
            Token::Rparen => RPAREN,
            Token::Lbrace => LBRACE,
            Token::Rbrace => RBRACE,
            Token::Function => FUNCTION,
            Token::Let => LET,
            Token::Empty => EMPTY,
            Token::True => TRUE,
            Token::False => FALSE,
            Token::Bang => BANG,
            Token::Asterisk => ASTERISK,
            Token::Slash => SLASH,
            Token::LT => LT,
            Token::GT => GT,
            Token::EQ => EQ,
            Token::NOT_EQ => NOT_EQ,
            Token::IF => IF,
            Token::Else => ELSE,
            Token::Return => RETURN,
        }
    }

    pub(crate) fn matches(&self, token: &Token) -> bool {
        match self {
            Token::Ident(_) => matches!(token, Token::Ident(_)),
            _ => token == self,
        }
    }
}
