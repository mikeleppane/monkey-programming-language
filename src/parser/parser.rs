use crate::ast::ast::ExpressionStatement;
use crate::ast::ast::*;
use crate::lexer::lexer::*;
use crate::token::token::TokenType::{Empty, Ident, Semicolon};
use crate::token::token::*;
use std::str::FromStr;

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, PartialOrd)]
pub enum Precedences {
    Lowest = 1,
    Equals = 2,
    LessGreater = 3,
    Sum = 4,
    Product = 5,
    Prefix = 6,
    Call = 7,
}

struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: Token::new(),
            peek_token: Token::new(),
            errors: Vec::new(),
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
        while !self.current_token_matches(&Empty) {
            let statement = self.parse_statement();
            if let Some(x) = statement {
                program.statements.push(x)
            }
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.current_token.r#type {
            TokenType::Let => {
                if let Some(x) = self.parse_let_statement() {
                    return Some(Box::new(x));
                }
                None
            }
            TokenType::Return => {
                if let Some(x) = self.parse_return_statement() {
                    return Some(Box::new(x));
                }
                None
            }
            _ => Some(self.parse_expression_statement()),
        }
    }

    fn parse_expression_statement(&mut self) -> Box<dyn Statement> {
        let statement = ExpressionStatement::new(
            self.current_token.clone(),
            self.parse_expression(Precedences::Lowest),
        );
        if self.peek_token_matches(&Semicolon) {
            self.next_token()
        }
        Box::new(statement)
    }

    fn parse_expression(&mut self, _precedence: Precedences) -> Option<Box<dyn Expression>> {
        let expression = self.parse_prefix(&self.current_token.r#type.clone());
        match expression {
            Some(exp) => Some(exp),
            None => {
                self.errors.push(format!(
                    "No prefix function for {:?} found",
                    self.current_token.r#type
                ));
                None
            }
        }
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let mut statement = LetStatement::new(self.current_token.clone());

        if !self.expect_peek(&Ident) {
            return None;
        }
        statement.name = Identifier::new(
            self.current_token.clone(),
            self.current_token.literal.clone(),
        );

        if !self.expect_peek(&TokenType::Assign) {
            return None;
        }

        if !self.current_token_matches(&TokenType::Semicolon) {
            self.next_token();
        }
        Some(statement)
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        let statement = ReturnStatement::new(self.current_token.clone());
        self.next_token();

        while !self.current_token_matches(&Semicolon) {
            self.next_token();
        }
        Some(statement)
    }

    fn current_token_matches(&self, token: &TokenType) -> bool {
        self.current_token.r#type == *token
    }

    fn peek_token_matches(&self, token: &TokenType) -> bool {
        self.peek_token.r#type == *token
    }

    fn expect_peek(&mut self, token: &TokenType) -> bool {
        match self.peek_token_matches(token) {
            true => {
                self.next_token();
                true
            }
            false => {
                self.peek_error(token);
                false
            }
        }
    }

    fn peek_error(&mut self, token: &TokenType) {
        let message = format!(
            "Expected next token to be {:?}. Got {:?} instead.",
            token, self.peek_token.r#type
        );
        self.errors.push(message)
    }

    fn errors(&self) -> &Vec<String> {
        self.errors.as_ref()
    }

    fn parse_prefix(&mut self, token: &TokenType) -> Option<Box<dyn Expression>> {
        match token {
            Ident => Some(self.parse_identifier()),
            TokenType::Int => self.parse_integer_literal(),
            TokenType::Bang => self.parse_prefix_expression(),
            TokenType::Minus => self.parse_prefix_expression(),
            _ => None,
        }
    }

    fn parse_identifier(&self) -> Box<dyn Expression> {
        Box::new(Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        })
    }

    fn parse_integer_literal(&mut self) -> Option<Box<dyn Expression>> {
        let value = i64::from_str(self.current_token.literal.as_str());
        match value {
            Ok(value) => Some(Box::new(IntegerLiteral::new(
                self.current_token.clone(),
                value,
            ))),
            Err(_value) => {
                self.errors.push(format!(
                    "Could not parse {} as integer",
                    self.current_token.literal.as_str()
                ));
                None
            }
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Box<dyn Expression>> {
        let mut expression = PrefixExpression::new(
            self.current_token.clone(),
            self.current_token.literal.as_str(),
            None,
        );
        self.next_token();
        expression.right = self.parse_expression(Precedences::Prefix);
        Some(Box::new(expression))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::assert_eq;
    use std::collections::HashMap;

    fn is_let_statement_ok(statement: &dyn Statement, name: &str) -> bool {
        if statement.token_literal() != "let" {
            eprintln!(
                "Token literal is not 'let'! Got {}",
                statement.token_literal()
            );
            return false;
        }
        let let_statement = statement
            .as_any()
            .downcast_ref::<LetStatement>()
            .expect("Expecting LetStament");
        if let_statement.name.value != name {
            eprintln!(
                "Identifier's value is not {}! Got {}",
                name, let_statement.name.value
            );
            return false;
        }
        if let_statement.name.token_literal() != name {
            eprintln!(
                "Identifier's value is not {}! Got {}",
                name,
                let_statement.name.token_literal()
            );
            return false;
        }
        true
    }

    fn check_parser_errors(parser: &Parser) {
        if parser.errors.is_empty() {
            return;
        }
        eprintln!("Parser has errors: {:?}", parser.errors.len());
        for error in parser.errors().iter() {
            eprintln!("Parser error: {:?}", error);
        }
    }

    fn check_integer_literal(expression: &Option<Box<dyn Expression>>, value: i64) {
        let integer_literal = expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<IntegerLiteral>()
            .expect("Was not IntegerLiteral");
        assert_eq!(integer_literal.value, value);
        assert_eq!(integer_literal.token_literal(), i64::to_string(&value))
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
        let mut program = parser.parse_program();
        program.statements = program
            .statements
            .into_iter()
            .filter(|s| s.get_type() == AstType::Let)
            .collect::<Vec<Box<dyn Statement>>>();
        assert_eq!(
            program.statements.len(),
            3,
            "There should three program statements"
        );
        for (index, identifier) in inputs.iter().enumerate() {
            let statement = &program.statements[index];
            if statement.get_type() == AstType::Let {
                assert!(is_let_statement_ok(statement.as_ref(), identifier))
            }
        }
    }

    #[test]
    fn test_check_errors() {
        let test_input = "let x 5;\
        let = 10;\
        let 838383;\
        ";
        let lexer = Lexer::new(test_input);
        let mut parser = Parser::new(lexer);
        parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(parser.errors().len(), 4);
    }

    #[test]
    fn test_return_statements() {
        let test_input = "return 5;\
        return 10;\
        return 993322;\
        ";
        let lexer = Lexer::new(test_input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(parser.errors().len(), 0);
        assert_eq!(
            program.statements.len(),
            3,
            "There should be exactly three program statements"
        );
        for statement in program.statements {
            assert_eq!(statement.token_literal(), "return")
        }
    }

    #[test]
    fn test_identifier_expression() {
        let test_input = "foobar;";
        let lexer = Lexer::new(test_input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(parser.errors().len(), 0);
        assert_eq!(
            program.statements.len(),
            1,
            "There should be exactly one program statements"
        );
        let statement = &program.statements[0];
        assert_eq!(statement.get_type(), AstType::ExprStatement);
        assert_eq!(statement.token_literal(), "foobar");
    }

    #[test]
    fn test_integer_literal_expression() {
        let test_input = "5;";
        let lexer = Lexer::new(test_input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(parser.errors().len(), 0);
        assert_eq!(
            program.statements.len(),
            1,
            "There should be exactly one program statements"
        );
        let statement = &program.statements[0];
        assert_eq!(statement.get_type(), AstType::ExprStatement);
        assert_eq!(statement.token_literal(), "5");
        let integer_literal = statement
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .expect("Was not ExpressionStatemetn")
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<IntegerLiteral>()
            .expect("Was not ExpressionStatement");
        assert_eq!(integer_literal.value, 5)
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let test_inputs = vec![
            HashMap::from([("input", "!5"), ("operator", "!"), ("int_value", "5")]),
            HashMap::from([("input", "-15"), ("operator", "-"), ("int_value", "15")]),
        ];
        for map in test_inputs {
            let lexer = Lexer::new(map.get("input").unwrap());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);
            assert_eq!(parser.errors().len(), 0);
            assert_eq!(
                program.statements.len(),
                1,
                "There should be exactly one program statements"
            );
            let statement = &program.statements[0];
            assert_eq!(statement.get_type(), AstType::ExprStatement);
            let prefix_expression = statement
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .expect("Was not ExpressionStatemetn")
                .expression
                .as_ref()
                .unwrap()
                .as_any()
                .downcast_ref::<PrefixExpression>()
                .expect("Was not PrefixExpression");
            assert_eq!(prefix_expression.operator, *map.get("operator").unwrap());
            check_integer_literal(
                &prefix_expression.right,
                i64::from_str(map.get("int_value").unwrap()).unwrap(),
            )
        }
    }
}
