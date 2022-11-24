use crate::token::token::*;

pub struct Lexer {
    input: String,
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

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer {
            input: input.to_string(),
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
        let mut tok = Token::Empty("".to_string());
        self.skip_whitespace();
        match self.ch.as_str() {
            ASSIGN => {
                if self.peek_char() == "=" {
                    self.read_char();
                    tok = Token::EQ("==".to_string());
                } else {
                    tok = Token::Assign("=".to_string());
                }
            }
            PLUS => {
                tok = Token::Plus("+".to_string());
            }
            MINUS => {
                tok = Token::Minus("-".to_string());
            }
            SLASH => {
                tok = Token::Slash("/".to_string());
            }
            ASTERISK => {
                tok = Token::Asterisk(ASTERISK.to_string());
            }
            BANG => {
                if self.peek_char() == "=" {
                    self.read_char();
                    tok = Token::NOT_EQ(NOT_EQ.to_string());
                } else {
                    tok = Token::Bang(BANG.to_string());
                }
            }
            LT => {
                tok = Token::LT(LT.to_string());
            }
            GT => {
                tok = Token::GT(GT.to_string());
            }
            SEMICOLON => {
                tok = Token::Semicolon(SEMICOLON.to_string());
            }
            LPAREN => {
                tok = Token::Lparen(LPAREN.to_string());
            }
            RPAREN => {
                tok = Token::Rparen(RPAREN.to_string());
            }
            COMMA => {
                tok = Token::Comma(COMMA.to_string());
            }
            LBRACE => {
                tok = Token::Lbrace(LBRACE.to_string());
            }
            RBRACE => {
                tok = Token::Rbrace(RBRACE.to_string());
            }
            EMPTY => {}
            _ => {
                if is_letter(&self.ch) {
                    return Token::lookup_ident(&self.read_identifier());
                } else if is_digit(&self.ch) {
                    return Token::Int(self.read_number());
                } else {
                    return Token::Illegal(self.ch.clone());
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
            (Token::Assign("=".to_string())),
            (Token::Plus("+".to_string())),
            (Token::Lparen("(".to_string())),
            (Token::Rparen(")".to_string())),
            (Token::Lbrace("{".to_string())),
            (Token::Rbrace("}".to_string())),
            (Token::Comma(",".to_string())),
            (Token::Semicolon(";".to_string())),
            (Token::Minus("-".to_string())),
            (Token::Slash("/".to_string())),
            (Token::Asterisk("*".to_string())),
            (Token::LT("<".to_string())),
            (Token::GT(">".to_string())),
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
        ";
        let inputs = vec![
            (Token::Let("let".to_string())),
            (Token::Ident("five".to_string())),
            (Token::Assign("=".to_string())),
            (Token::Int("5".to_string())),
            (Token::Semicolon(";".to_string())),
            (Token::Let("let".to_string())),
            (Token::Ident("ten".to_string())),
            (Token::Assign("=".to_string())),
            (Token::Int("10".to_string())),
            (Token::Semicolon(";".to_string())),
            (Token::Let("let".to_string())),
            (Token::Ident("add".to_string())),
            (Token::Assign("=".to_string())),
            (Token::Function("fn".to_string())),
            (Token::Lparen("(".to_string())),
            (Token::Ident("x".to_string())),
            (Token::Comma(",".to_string())),
            (Token::Ident("y".to_string())),
            (Token::Rparen(")".to_string())),
            (Token::Lbrace("{".to_string())),
            (Token::Ident("x".to_string())),
            (Token::Plus("+".to_string())),
            (Token::Ident("y".to_string())),
            (Token::Semicolon(";".to_string())),
            (Token::Rbrace("}".to_string())),
            (Token::Semicolon(";".to_string())),
            (Token::Let("let".to_string())),
            (Token::Ident("result".to_string())),
            (Token::Assign("=".to_string())),
            (Token::Ident("add".to_string())),
            (Token::Lparen("(".to_string())),
            (Token::Ident("five".to_string())),
            (Token::Comma(",".to_string())),
            (Token::Ident("ten".to_string())),
            (Token::Rparen(")".to_string())),
            (Token::Semicolon(";".to_string())),
            (Token::Bang("!".to_string())),
            (Token::Minus("-".to_string())),
            (Token::Slash("/".to_string())),
            (Token::Asterisk("*".to_string())),
            (Token::Int("5".to_string())),
            (Token::Semicolon(";".to_string())),
            (Token::Int("5".to_string())),
            (Token::LT("<".to_string())),
            (Token::Int("10".to_string())),
            (Token::GT(">".to_string())),
            (Token::Int("5".to_string())),
            (Token::Semicolon(";".to_string())),
            (Token::IF("if".to_string())),
            (Token::Lparen("(".to_string())),
            (Token::Int("5".to_string())),
            (Token::LT("<".to_string())),
            (Token::Int("10".to_string())),
            (Token::Rparen(")".to_string())),
            (Token::Lbrace("{".to_string())),
            (Token::Return("return".to_string())),
            (Token::True("true".to_string())),
            (Token::Semicolon(";".to_string())),
            (Token::Rbrace("}".to_string())),
            (Token::Else("else".to_string())),
            (Token::Lbrace("{".to_string())),
            (Token::Return("return".to_string())),
            (Token::False("false".to_string())),
            (Token::Semicolon(";".to_string())),
            (Token::Rbrace("}".to_string())),
            (Token::Int("10".to_string())),
            (Token::EQ("==".to_string())),
            (Token::Int("10".to_string())),
            (Token::Semicolon(";".to_string())),
            (Token::Int("10".to_string())),
            (Token::NOT_EQ("!=".to_string())),
            (Token::Int("9".to_string())),
            (Token::Semicolon(";".to_string())),
            (Token::Empty("".to_string())),
        ];
        let mut lexer = Lexer::new(test_input);
        for input_tok in inputs.iter() {
            let tok = lexer.next_token();
            assert_eq!(tok, *input_tok);
        }
    }
}
