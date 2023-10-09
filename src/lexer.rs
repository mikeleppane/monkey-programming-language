use crate::tokens::*;

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    ch: String,
}

fn is_letter(ch: &str) -> bool {
    ("a"..="z").contains(&ch) || ("A"..="Z").contains(&ch) || ch == "_"
}

fn is_digit(ch: &str) -> bool {
    ("0"..="9").contains(&ch)
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: String::new(),
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = String::new();
        } else {
            self.ch = self
                .input
                .chars()
                .nth(self.read_position)
                .unwrap_or_else(|| {
                    panic!("Cannot read character from position {}", self.read_position)
                })
                .to_string()
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        let mut tok = Token::Empty;
        self.skip_whitespace();
        match self.ch.as_str() {
            ASSIGN => {
                if self.peek_char() == "=" {
                    self.read_char();
                    tok = Token::EQ;
                } else {
                    tok = Token::Assign;
                }
            }
            PLUS => {
                tok = Token::Plus;
            }
            MINUS => {
                tok = Token::Minus;
            }
            SLASH => {
                tok = Token::Slash;
            }
            ASTERISK => {
                tok = Token::Asterisk;
            }
            BANG => {
                if self.peek_char() == "=" {
                    self.read_char();
                    tok = Token::NOT_EQ;
                } else {
                    tok = Token::Bang;
                }
            }
            LT => {
                tok = Token::LT;
            }
            GT => {
                tok = Token::GT;
            }
            SEMICOLON => {
                tok = Token::Semicolon;
            }
            LPAREN => {
                tok = Token::Lparen;
            }
            RPAREN => {
                tok = Token::Rparen;
            }
            COMMA => {
                tok = Token::Comma;
            }
            LBRACE => {
                tok = Token::Lbrace;
            }
            RBRACE => {
                tok = Token::Rbrace;
            }
            EMPTY => {}
            _ => {
                if is_letter(&self.ch) {
                    return Token::lookup_ident(&self.read_identifier());
                } else if is_digit(&self.ch) {
                    return Token::Int(self.read_number().to_string());
                } else {
                    return Token::Illegal(self.ch.to_string());
                }
            }
        }
        self.read_char();
        tok
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter(self.ch.as_str()) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn skip_whitespace(&mut self) {
        fn is_whitespace(ch: &str) -> bool {
            ch == " " || ch == "\t" || ch == "\n" || ch == "\r"
        }
        while is_whitespace(&self.ch) {
            self.read_char()
        }
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while is_digit(&self.ch) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn peek_char(&self) -> &str {
        if self.read_position >= self.input.len() {
            ""
        } else {
            &self.input[self.read_position..self.read_position + 1]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::assert_eq;

    #[test]
    fn test_next_token_with_basic_token_types() {
        let test_input = "=+(){},;-/*<>";
        let inputs = vec![
            (Token::Assign),
            (Token::Plus),
            (Token::Lparen),
            (Token::Rparen),
            (Token::Lbrace),
            (Token::Rbrace),
            (Token::Comma),
            (Token::Semicolon),
            (Token::Minus),
            (Token::Slash),
            (Token::Asterisk),
            (Token::LT),
            (Token::GT),
        ];
        let mut lexer = Lexer::new(test_input);
        for input_token in inputs.iter() {
            let tok = lexer.next_token();
            assert_eq!(tok, *input_token);
        }
    }
    #[test]
    fn test_next_token_with_real_test_code() {
        let test_input = "let five = 5;\
        let ten = 10;\
        \
        let add = fn(x, y) {\
          x + y;\
        }; \
        \
        let result = add(five, ten);\
        !-/*5;\
        5 < 10 > 5; \
        \
        if (5 < 10) {\
            return true;\
        } else {\
            return false;\
        }\
        \
        10 == 10;\
        10 != 9;\
        let add = fn(x, y) { x + y; };
        ";
        let inputs = vec![
            (Token::Let),
            (Token::Ident("five".to_string())),
            (Token::Assign),
            (Token::Int("5".to_string())),
            (Token::Semicolon),
            (Token::Let),
            (Token::Ident("ten".to_string())),
            (Token::Assign),
            (Token::Int("10".to_string())),
            (Token::Semicolon),
            (Token::Let),
            (Token::Ident("add".to_string())),
            (Token::Assign),
            (Token::Function),
            (Token::Lparen),
            (Token::Ident("x".to_string())),
            (Token::Comma),
            (Token::Ident("y".to_string())),
            (Token::Rparen),
            (Token::Lbrace),
            (Token::Ident("x".to_string())),
            (Token::Plus),
            (Token::Ident("y".to_string())),
            (Token::Semicolon),
            (Token::Rbrace),
            (Token::Semicolon),
            (Token::Let),
            (Token::Ident("result".to_string())),
            (Token::Assign),
            (Token::Ident("add".to_string())),
            (Token::Lparen),
            (Token::Ident("five".to_string())),
            (Token::Comma),
            (Token::Ident("ten".to_string())),
            (Token::Rparen),
            (Token::Semicolon),
            (Token::Bang),
            (Token::Minus),
            (Token::Slash),
            (Token::Asterisk),
            (Token::Int("5".to_string())),
            (Token::Semicolon),
            (Token::Int("5".to_string())),
            (Token::LT),
            (Token::Int("10".to_string())),
            (Token::GT),
            (Token::Int("5".to_string())),
            (Token::Semicolon),
            (Token::IF),
            (Token::Lparen),
            (Token::Int("5".to_string())),
            (Token::LT),
            (Token::Int("10".to_string())),
            (Token::Rparen),
            (Token::Lbrace),
            (Token::Return),
            (Token::True),
            (Token::Semicolon),
            (Token::Rbrace),
            (Token::Else),
            (Token::Lbrace),
            (Token::Return),
            (Token::False),
            (Token::Semicolon),
            (Token::Rbrace),
            (Token::Int("10".to_string())),
            (Token::EQ),
            (Token::Int("10".to_string())),
            (Token::Semicolon),
            (Token::Int("10".to_string())),
            (Token::NOT_EQ),
            (Token::Int("9".to_string())),
            (Token::Semicolon),
            (Token::Let),
            (Token::Ident("add".to_string())),
            (Token::Assign),
            (Token::Function),
            (Token::Lparen),
            (Token::Ident("x".to_string())),
            (Token::Comma),
            (Token::Ident("y".to_string())),
            (Token::Rparen),
            (Token::Lbrace),
            (Token::Ident("x".to_string())),
            (Token::Plus),
            (Token::Ident("y".to_string())),
            (Token::Semicolon),
            (Token::Rbrace),
            (Token::Semicolon),
        ];
        let mut lexer = Lexer::new(test_input);
        for input_tok in inputs.iter() {
            let tok = lexer.next_token();
            assert_eq!(tok, *input_tok);
        }
    }
}
