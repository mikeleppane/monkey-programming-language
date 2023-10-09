use crate::ast::*;
use crate::lexer::Lexer;
use crate::parser::Precedences::{Equals, LessGreater, Lowest, Product, Sum};
use crate::tokens::*;
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
    #[allow(dead_code)]
    fn get_precedence(token: &Token) -> Precedences {
        match token {
            Token::EQ => Equals,
            Token::NOT_EQ => Equals,
            Token::LT => LessGreater,
            Token::GT => LessGreater,
            Token::Plus => Sum,
            Token::Minus => Sum,
            Token::Slash => Product,
            Token::Asterisk => Product,
            _ => Lowest,
        }
    }
}

#[allow(dead_code)]
struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    #[allow(dead_code)]
    fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: Token::Empty,
            peek_token: Token::Empty,
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
        while !matches!(self.current_token, Token::Empty) {
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
            Token::Let => {
                if let Some(x) = self.parse_let_statement() {
                    return Some(Box::new(x));
                }
                None
            }
            Token::Return => {
                if let Some(x) = self.parse_return_statement() {
                    return Some(Box::new(x));
                }
                None
            }
            _ => Some(self.parse_expression_statement()),
        }
    }

    fn parse_expression_statement(&mut self) -> Box<ExpressionStatement> {
        let statement = ExpressionStatement::new(
            self.current_token.clone(),
            self.parse_expression(Precedences::Lowest),
        );
        if matches!(self.peek_token, Token::Semicolon) {
            self.next_token()
        }
        Box::new(statement)
    }

    fn parse_expression(&mut self, precedence: Precedences) -> Option<Box<dyn Expression>> {
        let mut left_prefix = self.parse_prefix(&self.current_token.clone());
        if left_prefix.is_none() {
            self.errors.push(format!(
                "No prefix function for {:?} found",
                self.current_token.literal()
            ));
            return None;
        }
        while !matches!(self.peek_token, Token::Semicolon)
            && precedence < Precedences::get_precedence(&self.peek_token)
        {
            if !self.check_infixes(&self.peek_token) {
                return left_prefix;
            }
            self.next_token();
            left_prefix = self.parse_infix(
                &self.current_token.clone(),
                left_prefix
                    .unwrap_or_else(|| panic!("parse_expression error: prefix cannot be None!")),
            );
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

        if !self.expect_peek(&Token::Assign) {
            return None;
        }

        while !matches!(self.current_token, Token::Semicolon) {
            self.next_token();
        }
        Some(statement)
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        let statement = ReturnStatement::new(self.current_token.clone());
        self.next_token();

        while !matches!(self.current_token, Token::Semicolon) {
            self.next_token();
        }
        Some(statement)
    }

    /* fn current_token_matches(&self, token_literal: &str) -> bool {
           let token = Token::from_str(token_literal);
           match token {
               Ok(found_token) => self.current_token.matches(&found_token),
               Err(_) => false,
           }
       }

       fn peek_token_matches(&self, token: &Token) -> bool {
           self.peek_token.matches(token)
       }
    */
    fn expect_peek(&mut self, token: &Token) -> bool {
        match self.peek_token.matches(token) {
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
            Token::Bang => self.parse_prefix_expression(),
            Token::Minus => self.parse_prefix_expression(),
            Token::True => self.parse_boolean(),
            Token::False => self.parse_boolean(),
            Token::Lparen => self.parse_grouped_expression(),
            _ => None,
        }
    }

    fn parse_grouped_expression(&mut self) -> Option<Box<dyn Expression>> {
        self.next_token();
        let expr = self.parse_expression(Precedences::Lowest);
        if !self.expect_peek(&Token::Rparen) {
            return None;
        }
        expr
    }

    fn parse_boolean(&self) -> Option<Box<dyn Expression>> {
        Some(Box::new(Boolean::new(
            self.current_token.clone(),
            matches!(self.current_token, Token::True),
        )))
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
            Some(left),
            self.current_token.literal(),
            None,
        );
        let precedence = Precedences::get_precedence(&self.current_token);
        self.next_token();
        expression.right = self.parse_expression(precedence);
        Some(Box::new(expression))
    }

    fn parse_infix(
        &mut self,
        token: &Token,
        expression: Box<dyn Expression>,
    ) -> Option<Box<dyn Expression>> {
        match token {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::EQ
            | Token::NOT_EQ
            | Token::LT
            | Token::GT => self.parse_infix_expression(expression),
            _ => None,
        }
    }

    fn check_infixes(&self, token: &Token) -> bool {
        matches!(
            token,
            Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::EQ
                | Token::NOT_EQ
                | Token::LT
                | Token::GT
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::assert_eq;

    fn check_parser_errors(parser: &Parser) {
        if parser.errors.is_empty() {
            return;
        }
        eprintln!("Parser has errors: {:?}", parser.errors.len());
        for error in parser.errors().iter() {
            eprintln!("Parser error: {:?}", error);
        }
        panic!("Parser errors found!")
    }

    fn check_let_statement(statement: &dyn Statement, name: &str) {
        assert_eq!(
            statement.token_literal(),
            "let",
            "Token literal is not 'let'! Got {}",
            statement.token_literal()
        );
        let let_stmt = match statement.as_any().downcast_ref::<LetStatement>() {
            Some(stmt) => stmt,
            None => panic!("statement is not LetStatement"),
        };

        assert_eq!(
            let_stmt.name.value, name,
            "Identifier's value is not {}! Got {}",
            name, let_stmt.name.value
        );

        assert_eq!(
            let_stmt.name.token_literal(),
            name,
            "Identifier's value is not {}! Got {}",
            name,
            let_stmt.name.token_literal()
        );
    }

    fn check_return_statement(statement: &dyn Statement) {
        assert_eq!(
            statement.token_literal(),
            "return",
            "Token literal is not 'return'! Got {}",
            statement.token_literal()
        );
        match statement.as_any().downcast_ref::<ReturnStatement>() {
            Some(stmt) => stmt,
            None => panic!("statement is not ReturnStatement"),
        };
    }

    fn check_identifier_expression(ident: &ExpressionStatement, value: &str) {
        let ident = match ident
            .expression
            .as_ref()
            .expect("Expecting expression")
            .as_any()
            .downcast_ref::<Identifier>()
        {
            Some(ident) => ident,
            None => panic!("expression is not Identifier"),
        };
        assert_eq!(
            ident.value, value,
            "Ident value is not {}! Got {}",
            value, ident.value
        );
        assert_eq!(
            ident.token_literal(),
            value,
            "Ident token literal is not {}! Got {}",
            value,
            ident.value
        );
    }

    fn check_boolean_literal(expr: &Option<Box<dyn Expression>>, value: bool) {
        let boolean = match expr
            .as_ref()
            .expect("Expecting expression")
            .as_any()
            .downcast_ref::<Boolean>()
        {
            Some(boolean) => boolean,
            None => panic!("expression is not Boolean"),
        };
        assert_eq!(
            boolean.value, value,
            "Boolean value is not {}! Got {}",
            value, boolean.value
        );
        assert_eq!(
            boolean.token_literal(),
            value.to_string(),
            "Boolean token literal is not {}! Got {}",
            value,
            boolean.value
        );
    }

    fn check_integer_literal(expr: &Option<Box<dyn Expression>>, value: i64) {
        let integer = match expr
            .as_ref()
            .expect("Expecting expression")
            .as_any()
            .downcast_ref::<IntegerLiteral>()
        {
            Some(integer) => integer,
            None => panic!("expression is not IntegerLiteral"),
        };
        assert_eq!(
            integer.value, value,
            "IntegerLiteral value is not {}! Got {}",
            value, integer.value
        );
        assert_eq!(
            integer.token_literal(),
            value.to_string(),
            "Ident token literal is not {}! Got {}",
            value,
            integer.token_literal()
        );
    }

    #[test]
    fn test_let_statements() {
        let test_input = "let x = 5;\
        let y = 10;\
        let foobar = 838383;\
        ";
        let inputs = ["x", "y", "foobar"];
        let lexer = Lexer::new(test_input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(
            program.statements.len(),
            3,
            "There should be three program statements"
        );
        check_parser_errors(&parser);
        for (index, identifier) in inputs.iter().enumerate() {
            let statement = &program.statements[index];
            check_let_statement(statement.as_ref(), identifier)
        }
    }

    #[test]
    #[should_panic]
    fn test_let_statements_should_panic_when_parser_founds_errors() {
        let test_input = "let x 5;\
        let y = 10;\
        let foobar = 838383;\
        ";
        let inputs = ["x", "y", "foobar"];
        let lexer = Lexer::new(test_input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(
            program.statements.len(),
            3,
            "There should be three program statements"
        );
        check_parser_errors(&parser);
        for (index, identifier) in inputs.iter().enumerate() {
            let statement = &program.statements[index];
            check_let_statement(statement.as_ref(), identifier)
        }
    }

    #[test]
    fn test_identifier_expression() {
        let test_input = "foobar;";
        let lexer = Lexer::new(test_input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(
            program.statements.len(),
            1,
            "Program has not enough statements. Got {}",
            program.statements.len()
        );
        check_parser_errors(&parser);
        for statement in &program.statements {
            match statement.as_any().downcast_ref::<ExpressionStatement>() {
                Some(expr) => check_identifier_expression(expr, "foobar"),
                None => panic!("statement is not ExpressionStatement"),
            };
        }
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
        assert_eq!(
            program.statements.len(),
            3,
            "There should be three program statements"
        );
        check_parser_errors(&parser);
        for statement in &program.statements {
            check_return_statement(statement.as_ref());
        }
    }

    #[test]
    fn test_integer_literal_expressions() {
        let test_input = "5;";
        let lexer = Lexer::new(test_input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(
            program.statements.len(),
            1,
            "Program has not enough statements. Got {}",
            program.statements.len()
        );
        check_parser_errors(&parser);
        for statement in &program.statements {
            match statement.as_any().downcast_ref::<ExpressionStatement>() {
                Some(expr) => {
                    check_integer_literal(&expr.expression, 5);
                }
                None => panic!("statement is not ExpressionStatement"),
            };
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        struct PrefixTest {
            input: String,
            operator: String,
            integer_value: i64,
        }
        impl PrefixTest {
            fn new(input: String, operator: String, integer_value: i64) -> Self {
                Self {
                    input,
                    operator,
                    integer_value,
                }
            }
        }

        let test_inputs = vec![
            PrefixTest::new(String::from("!5;"), String::from("!"), 5),
            PrefixTest::new(String::from("-15;"), String::from("-"), 15),
        ];
        for test in &test_inputs {
            let lexer = Lexer::new(&test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);
            assert_eq!(
                program.statements.len(),
                1,
                "There should be exactly one program statements"
            );
            for statement in &program.statements {
                match statement.as_any().downcast_ref::<ExpressionStatement>() {
                    Some(expr) => {
                        match expr
                            .expression
                            .as_ref()
                            .expect("Expecting expression")
                            .as_any()
                            .downcast_ref::<PrefixExpression>()
                        {
                            Some(expr) => {
                                assert_eq!(expr.operator, test.operator);
                                check_integer_literal(&expr.right, test.integer_value);
                            }
                            None => panic!("expression is not PrefixExpression"),
                        };
                    }
                    None => panic!("statement is not ExpressionStatement"),
                };
            }
        }
    }

    #[test]
    fn test_parsing_prefix_expressions_with_boolean() {
        struct PrefixTest {
            input: String,
            operator: String,
            boolean: bool,
        }
        impl PrefixTest {
            fn new(input: String, operator: String, boolean: bool) -> Self {
                Self {
                    input,
                    operator,
                    boolean,
                }
            }
        }

        let test_inputs = vec![
            PrefixTest::new(String::from("!true;"), String::from("!"), true),
            PrefixTest::new(String::from("!false;"), String::from("!"), false),
        ];
        for test in &test_inputs {
            let lexer = Lexer::new(&test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);
            assert_eq!(
                program.statements.len(),
                1,
                "There should be exactly one program statements"
            );
            for statement in &program.statements {
                match statement.as_any().downcast_ref::<ExpressionStatement>() {
                    Some(expr) => {
                        match expr
                            .expression
                            .as_ref()
                            .expect("Expecting expression")
                            .as_any()
                            .downcast_ref::<PrefixExpression>()
                        {
                            Some(expr) => {
                                assert_eq!(expr.operator, test.operator);
                                check_boolean_literal(&expr.right, test.boolean);
                            }
                            None => panic!("expression is not PrefixExpression"),
                        };
                    }
                    None => panic!("statement is not ExpressionStatement"),
                };
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        struct InfixTest {
            input: String,
            left_value: i64,
            operator: String,
            right_value: i64,
        }
        impl InfixTest {
            fn new(input: String, left_value: i64, operator: String, right_value: i64) -> Self {
                Self {
                    input,
                    left_value,
                    operator,
                    right_value,
                }
            }
        }

        let test_inputs = vec![
            InfixTest::new(String::from("5 + 5;"), 5, String::from("+"), 5),
            InfixTest::new(String::from("5 - 5;"), 5, String::from("-"), 5),
            InfixTest::new(String::from("5 * 5;"), 5, String::from("*"), 5),
            InfixTest::new(String::from("5 / 5;"), 5, String::from("/"), 5),
            InfixTest::new(String::from("5 > 5;"), 5, String::from(">"), 5),
            InfixTest::new(String::from("5 < 5;"), 5, String::from("<"), 5),
            InfixTest::new(String::from("5 == 5;"), 5, String::from("=="), 5),
            InfixTest::new(String::from("5 != 5;"), 5, String::from("!="), 5),
        ];
        for test in &test_inputs {
            let lexer = Lexer::new(&test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);
            assert_eq!(
                program.statements.len(),
                1,
                "There should be exactly one program statements"
            );
            for statement in &program.statements {
                match statement.as_any().downcast_ref::<ExpressionStatement>() {
                    Some(expr) => {
                        match expr
                            .expression
                            .as_ref()
                            .expect("Expecting expression")
                            .as_any()
                            .downcast_ref::<InfixExpression>()
                        {
                            Some(expr) => {
                                assert_eq!(expr.operator, test.operator);
                                check_integer_literal(&expr.left, test.left_value);
                                check_integer_literal(&expr.right, test.right_value);
                            }
                            None => panic!("expression is not InfixExpression"),
                        };
                    }
                    None => panic!("statement is not ExpressionStatement"),
                };
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions_with_boolean() {
        struct InfixTest {
            input: String,
            left_value: bool,
            operator: String,
            right_value: bool,
        }
        impl InfixTest {
            fn new(input: String, left_value: bool, operator: String, right_value: bool) -> Self {
                Self {
                    input,
                    left_value,
                    operator,
                    right_value,
                }
            }
        }

        let test_inputs = vec![
            InfixTest::new(
                String::from("true == true;"),
                true,
                String::from("=="),
                true,
            ),
            InfixTest::new(
                String::from("true != false;"),
                true,
                String::from("!="),
                false,
            ),
            InfixTest::new(
                String::from("false == false;"),
                false,
                String::from("=="),
                false,
            ),
        ];
        for test in &test_inputs {
            let lexer = Lexer::new(&test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);
            assert_eq!(
                program.statements.len(),
                1,
                "There should be exactly one program statements"
            );
            for statement in &program.statements {
                match statement.as_any().downcast_ref::<ExpressionStatement>() {
                    Some(expr) => {
                        match expr
                            .expression
                            .as_ref()
                            .expect("Expecting expression")
                            .as_any()
                            .downcast_ref::<InfixExpression>()
                        {
                            Some(expr) => {
                                assert_eq!(expr.operator, test.operator);
                                check_boolean_literal(&expr.left, test.left_value);
                                check_boolean_literal(&expr.right, test.right_value);
                            }
                            None => panic!("expression is not InfixExpression"),
                        };
                    }
                    None => panic!("statement is not ExpressionStatement"),
                };
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        struct Test {
            input: String,
            expected: String,
        }
        impl Test {
            fn new(input: String, expected: String) -> Self {
                Self { input, expected }
            }
        }
        let test_inputs = vec![
            Test::new(String::from("-a * b"), String::from("((-a) * b)")),
            Test::new(String::from("!-a"), String::from("(!(-a))")),
            Test::new(String::from("a + b + c"), String::from("((a + b) + c)")),
            Test::new(String::from("a + b - c"), String::from("((a + b) - c)")),
            Test::new(String::from("a * b * c"), String::from("((a * b) * c)")),
            Test::new(String::from("a * b / c"), String::from("((a * b) / c)")),
            Test::new(String::from("a + b / c"), String::from("(a + (b / c))")),
            Test::new(
                String::from("a + b * c + d / e - f"),
                String::from("(((a + (b * c)) + (d / e)) - f)"),
            ),
            Test::new(
                String::from("3 + 4; -5 * 5"),
                String::from("(3 + 4)((-5) * 5)"),
            ),
            Test::new(
                String::from("5 > 4 == 3 < 4"),
                String::from("((5 > 4) == (3 < 4))"),
            ),
            Test::new(
                String::from("5 < 4 != 3 > 4"),
                String::from("((5 < 4) != (3 > 4))"),
            ),
            Test::new(
                String::from("3 + 4 * 5 == 3 * 1 + 4 * 5"),
                String::from("((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            ),
            Test::new(String::from("true"), String::from("true")),
            Test::new(String::from("false"), String::from("false")),
            Test::new(
                String::from("3 > 5 == false"),
                String::from("((3 > 5) == false)"),
            ),
            Test::new(
                String::from("3 < 5 == true"),
                String::from("((3 < 5) == true)"),
            ),
            Test::new(
                String::from("1 + (2 + 3) + 4"),
                String::from("((1 + (2 + 3)) + 4)"),
            ),
            Test::new(String::from("(5 + 5) * 2"), String::from("((5 + 5) * 2)")),
            Test::new(String::from("2 / (5 + 5)"), String::from("(2 / (5 + 5))")),
            Test::new(String::from("-(5 + 5)"), String::from("(-(5 + 5))")),
            Test::new(
                String::from("!(true == true)"),
                String::from("(!(true == true))"),
            ),
        ];
        for test in test_inputs {
            let lexer = Lexer::new(&test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);
            assert_eq!(program.to_string(), test.expected)
        }
    }
}
