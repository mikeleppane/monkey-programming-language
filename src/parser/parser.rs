use crate::ast::ast::ExpressionStatement;
use crate::ast::ast::*;
use crate::lexer::lexer::*;
use crate::parser::parser::Precedences::{Equals, LessGreater, Lowest, Product, Sum};
use crate::token::token::*;
use std::str::FromStr;

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Clone)]
pub enum Precedences {
    Lowest = 1,
    Equals = 2,
    LessGreater = 3,
    Sum = 4,
    Product = 5,
    Prefix = 6,
    Call = 7,
}

impl Precedences {
    fn get_precedence(token: &Token) -> Precedences {
        match token {
            Token::EQ(_) => Equals,
            Token::NOT_EQ(_) => Equals,
            Token::LT(_) => LessGreater,
            Token::GT(_) => LessGreater,
            Token::Plus(_) => Sum,
            Token::Minus(_) => Sum,
            Token::Slash(_) => Product,
            Token::Asterisk(_) => Product,
            _ => Lowest,
        }
    }
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
            current_token: Token::from_str("").unwrap(),
            peek_token: Token::from_str("").unwrap(),
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
        while !self.current_token_matches("") {
            let statement = self.parse_statement();
            if let Some(x) = statement {
                program.statements.push(x)
            }
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.current_token {
            Token::Let(_) => {
                if let Some(x) = self.parse_let_statement() {
                    return Some(Box::new(x));
                }
                None
            }
            Token::Return(_) => {
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
        if self.peek_token_matches(&Token::from_str(";").unwrap()) {
            self.next_token()
        }
        Box::new(statement)
    }

    fn parse_expression(&mut self, precedence: Precedences) -> Option<Box<dyn Expression>> {
        let mut left_prefix = self.parse_prefix(&self.current_token.clone());
        if let None = &left_prefix {
            self.errors.push(format!(
                "No prefix function for {:?} found",
                self.current_token.literal()
            ));
            return None;
        }
        while !self.peek_token_matches(&Token::from_str(";").unwrap())
            && precedence < Precedences::get_precedence(&self.peek_token)
        {
            if !self.check_infixes(&self.peek_token) {
                return left_prefix;
            }
            self.next_token();
            left_prefix = self.parse_infix(&self.current_token.clone(), left_prefix.unwrap());
        }
        left_prefix
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let mut statement = LetStatement::new(self.current_token.clone());

        if !self.expect_peek(&Token::Ident("".to_string())) {
            return None;
        }
        statement.name = Identifier::new(
            self.current_token.clone(),
            self.current_token.literal().to_string(),
        );

        if !self.expect_peek(&Token::Assign("=".to_string())) {
            return None;
        }

        while !self.current_token_matches(SEMICOLON) {
            self.next_token();
        }
        Some(statement)
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        let statement = ReturnStatement::new(self.current_token.clone());
        self.next_token();

        while !self.current_token_matches(SEMICOLON) {
            self.next_token();
        }
        Some(statement)
    }

    fn current_token_matches(&self, token_literal: &str) -> bool {
        let token = Token::from_str(token_literal);
        match token {
            Ok(found_token) => self.current_token.matches(&found_token),
            Err(_) => false,
        }
    }

    fn peek_token_matches(&self, token: &Token) -> bool {
        self.peek_token.matches(token)
        // let token = Token::from_str(token_literal);
        // match token {
        //     Ok(found_token) => {
        //         let _token = &self.peek_token;
        //         matches!(found_token, _token)
        //     }
        //     Err(_) => false,
        // }
    }

    fn expect_peek(&mut self, token: &Token) -> bool {
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

    fn peek_error(&mut self, token: &Token) {
        let message = format!(
            "Expected next token to be {:?}. Got {:?} instead.",
            token,
            self.peek_token.literal()
        );
        self.errors.push(message)
    }

    fn errors(&self) -> &Vec<String> {
        self.errors.as_ref()
    }

    fn parse_prefix(&mut self, token: &Token) -> Option<Box<dyn Expression>> {
        match token {
            Token::Ident(_) => Some(self.parse_identifier()),
            Token::Int(_) => self.parse_integer_literal(),
            Token::Bang(_) => self.parse_prefix_expression(),
            Token::Minus(_) => self.parse_prefix_expression(),
            _ => None,
        }
    }

    fn parse_identifier(&self) -> Box<dyn Expression> {
        Box::new(Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal().to_string(),
        })
    }

    fn parse_integer_literal(&mut self) -> Option<Box<dyn Expression>> {
        let value = i64::from_str(self.current_token.literal());
        match value {
            Ok(value) => Some(Box::new(IntegerLiteral::new(
                self.current_token.clone(),
                value,
            ))),
            Err(_value) => {
                self.errors.push(format!(
                    "Could not parse {} as integer",
                    self.current_token.literal()
                ));
                None
            }
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Box<dyn Expression>> {
        let mut expression = PrefixExpression::new(
            self.current_token.clone(),
            self.current_token.literal(),
            None,
        );
        self.next_token();
        expression.right = self.parse_expression(Precedences::Prefix);
        Some(Box::new(expression))
    }

    fn parse_infix_expression(&mut self, left: Box<dyn Expression>) -> Option<Box<dyn Expression>> {
        let mut expression = InfixExpression::new(
            self.current_token.clone(),
            left,
            self.current_token.literal(),
            None,
        );
        self.next_token();
        let precedence = Precedences::get_precedence(&self.current_token);
        expression.right = self.parse_expression(precedence.clone());
        Some(Box::new(expression))
    }

    fn parse_infix(
        &mut self,
        token: &Token,
        expression: Box<dyn Expression>,
    ) -> Option<Box<dyn Expression>> {
        match token {
            Token::Plus(_) => self.parse_infix_expression(expression),
            Token::Minus(_) => self.parse_infix_expression(expression),
            Token::Slash(_) => self.parse_infix_expression(expression),
            Token::Asterisk(_) => self.parse_infix_expression(expression),
            Token::EQ(_) => self.parse_infix_expression(expression),
            Token::NOT_EQ(_) => self.parse_infix_expression(expression),
            Token::LT(_) => self.parse_infix_expression(expression),
            Token::GT(_) => self.parse_infix_expression(expression),
            _ => None,
        }
    }

    fn check_infixes(&self, token: &Token) -> bool {
        matches!(
            token,
            Token::Plus(_)
                | Token::Minus(_)
                | Token::Slash(_)
                | Token::Asterisk(_)
                | Token::EQ(_)
                | Token::NOT_EQ(_)
                | Token::LT(_)
                | Token::GT(_)
        )
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
        check_parser_errors(&parser);
        assert_eq!(parser.errors().len(), 0);
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

    #[test]
    fn test_parsing_infix_expressions() {
        let test_inputs = vec![
            HashMap::from([
                ("input", "5+5;"),
                ("left_value", "5"),
                ("operator", "+"),
                ("right_value", "5"),
            ]),
            HashMap::from([
                ("input", "5 - 5;"),
                ("left_value", "5"),
                ("operator", "-"),
                ("right_value", "5"),
            ]),
            HashMap::from([
                ("input", "5 * 5;"),
                ("left_value", "5"),
                ("operator", "*"),
                ("right_value", "5"),
            ]),
            HashMap::from([
                ("input", "5 / 5;"),
                ("left_value", "5"),
                ("operator", "/"),
                ("right_value", "5"),
            ]),
            HashMap::from([
                ("input", "5 > 5;"),
                ("left_value", "5"),
                ("operator", ">"),
                ("right_value", "5"),
            ]),
            HashMap::from([
                ("input", "5 < 5;"),
                ("left_value", "5"),
                ("operator", "<"),
                ("right_value", "5"),
            ]),
            HashMap::from([
                ("input", "5 == 5;"),
                ("left_value", "5"),
                ("operator", "=="),
                ("right_value", "5"),
            ]),
            HashMap::from([
                ("input", "5 != 5;"),
                ("left_value", "5"),
                ("operator", "!="),
                ("right_value", "5"),
            ]),
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
            let infix_expression = statement
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .expect("Was not ExpressionStatement")
                .expression
                .as_ref()
                .unwrap()
                .as_any()
                .downcast_ref::<InfixExpression>()
                .expect("Was not InfixExpression");
            dbg!(infix_expression.to_string());
            assert_eq!(infix_expression.operator, *map.get("operator").unwrap());
            let il = infix_expression
                .left
                .as_ref()
                .as_any()
                .downcast_ref::<IntegerLiteral>()
                .expect("Was not IntegerLiteral");
            assert_eq!(
                il.value,
                i64::from_str(map.get("right_value").unwrap()).unwrap()
            );
            check_integer_literal(
                &infix_expression.right,
                i64::from_str(map.get("right_value").unwrap()).unwrap(),
            );
        }
    }
}
