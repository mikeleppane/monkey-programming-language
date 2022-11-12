use crate::ast::ast::*;
use crate::lexer::lexer::*;
use crate::token::token::TokenType::{EMPTY, IDENT};
use crate::token::token::*;

struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: Token::new(),
            peek_token: Token::new(),
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };
        loop {
            let statement = self.parse_statement();
            if let Some(x) = statement {
                program.statements.push(x)
            }
            self.next_token();
            dbg!(&self.current_token);
            if self.current_token.r#type == EMPTY {
                break;
            }
        }
        program
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.current_token.r#type {
            TokenType::LET => {
                if let Some(x) = self.parse_let_statement() {
                    return Some(Box::new(x));
                }
                return None;
            }
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let mut statement = LetStatement::new(self.current_token.clone());

        if !self.expect_peek(IDENT) {
            return None;
        }
        statement.name = Identifier::new(
            self.current_token.clone(),
            self.current_token.literal.clone(),
        );

        if !self.expect_peek(TokenType::ASSIGN) {
            return None;
        }

        if !self.current_token_matches(TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(statement)
    }

    fn current_token_matches(&self, token: TokenType) -> bool {
        self.current_token.r#type == token
    }

    fn peek_token_matches(&self, token: TokenType) -> bool {
        self.peek_token.r#type == token
    }

    fn expect_peek(&mut self, token: TokenType) -> bool {
        match self.peek_token_matches(token) {
            true => {
                self.next_token();
                true
            }
            false => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::assert_eq;

    fn is_let_statement_ok(statement: &dyn Statement, name: &str) -> bool {
        if statement.token_literal() != "let" {
            eprintln!(
                "Token literal is not 'let'! Got {}",
                statement.token_literal().as_str()
            );
            return false;
        }
        let identifier = statement.identifier().unwrap();
        if identifier.value != name {
            eprintln!(
                "Identifier's value is not {}! Got {}",
                name, identifier.value
            );
            return false;
        }
        if identifier.token_literal() != name {
            eprintln!(
                "Identifier's token literal is not {}! Got {}",
                name,
                identifier.token_literal()
            );
            return false;
        }
        true
    }

    #[test]
    fn test_let_statements() {
        let test_input = "let x = 5;\
        let y = 10;\
        let foobar = 838383;\
        ";
        let inputs = vec!["x", "y", "foobar"];
        let lexer = Lexer::new(test_input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(
            program.statements.len(),
            3,
            "There should three program statements"
        );
        for (index, identifier) in inputs.iter().enumerate() {
            let statement = &program.statements[index];
            assert!(is_let_statement_ok(statement.as_ref(), identifier))
        }
    }
}
