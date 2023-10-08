use crate::token::tokens::*;
use std::{any::Any, fmt};

pub trait Node {
    fn token_literal(&self) -> &str;
    fn get_name(&self) -> &'static str;
    fn as_any(&self) -> &dyn Any;
}

pub trait Statement: Node {
    fn statement_node(&self);
    fn to_string(&self) -> String;
}

pub trait Expression: Node {
    fn expression_node(&self);
    fn to_string(&self) -> String;
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

#[allow(dead_code)]
impl Program {
    fn token_literal(&self) -> &str {
        if !self.statements.is_empty() {
            self.statements[0].token_literal()
        } else {
            ""
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(|s| { s.to_string() })
                .collect::<Vec<String>>()
                .join("")
                .trim()
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

#[allow(dead_code)]
impl Identifier {
    pub fn new(token: Token, value: String) -> Self {
        Self { token, value }
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }

    fn get_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}

    fn to_string(&self) -> String {
        self.value.clone()
    }
}

pub struct LetStatement {
    token: Token,
    pub name: Identifier,
    pub value: Option<Box<dyn Expression>>,
}

impl LetStatement {
    #[allow(dead_code)]
    pub fn new(token: Token) -> Self {
        Self {
            token,
            name: Identifier::new(Token::Empty, "".to_string() /* std::string::String */),
            value: None,
        }
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }

    fn get_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}

    fn to_string(&self) -> String {
        let mut string = "".to_string();
        string.push_str(self.token_literal());
        string.push(' ');
        string.push_str(self.name.to_string().as_str());
        string.push_str(" = ");
        if let Some(x) = &self.value {
            string.push_str(x.to_string().as_str());
        };
        string.push(';');
        string
    }
}

pub struct ReturnStatement {
    token: Token,
    return_value: Option<Box<dyn Expression>>,
}

impl ReturnStatement {
    #[allow(dead_code)]
    pub fn new(token: Token) -> Self {
        Self {
            token,
            return_value: None,
        }
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }

    fn get_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}
    fn to_string(&self) -> String {
        let mut string = "".to_string();
        string.push_str(self.token_literal());
        string.push(' ');
        if let Some(x) = &self.return_value {
            string.push_str(x.to_string().as_ref());
        };
        string.push(';');
        string
    }
}

pub struct ExpressionStatement {
    token: Token,
    pub expression: Option<Box<dyn Expression>>,
}

#[allow(dead_code)]
impl ExpressionStatement {
    pub fn new(token: Token, expression: Option<Box<dyn Expression>>) -> Self {
        Self { token, expression }
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }

    fn get_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {
        todo!()
    }
    fn to_string(&self) -> String {
        let mut string = "".to_string();
        match self.expression.as_ref() {
            None => string,
            Some(expr) => {
                string.push_str(expr.to_string().as_str());
                string
            }
        }
    }
}

pub struct IntegerLiteral {
    token: Token,
    pub value: i64,
}

impl IntegerLiteral {
    #[allow(dead_code)]
    pub fn new(token: Token, value: i64) -> Self {
        Self { token, value }
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }

    fn get_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) {
        todo!()
    }

    fn to_string(&self) -> String {
        self.token.literal().to_string()
    }
}

pub struct PrefixExpression {
    token: Token,
    pub operator: String,
    pub(crate) right: Option<Box<dyn Expression>>,
}

impl PrefixExpression {
    #[allow(dead_code)]
    pub fn new(token: Token, operator: &str, right: Option<Box<dyn Expression>>) -> Self {
        Self {
            token,
            operator: operator.to_string(),
            right,
        }
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }

    fn get_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for PrefixExpression {
    fn expression_node(&self) {
        todo!()
    }

    fn to_string(&self) -> String {
        let right = if let Some(x) = &self.right {
            x.to_string()
        } else {
            "".to_owned()
        };
        format!("{}{}{}{}", "(", self.operator.as_str(), right, ")")
    }
}

pub struct InfixExpression {
    token: Token,
    pub left: Option<Box<dyn Expression>>,
    pub operator: String,
    pub right: Option<Box<dyn Expression>>,
}

impl InfixExpression {
    #[allow(dead_code)]
    pub fn new(
        token: Token,
        left: Option<Box<dyn Expression>>,
        operator: &str,
        right: Option<Box<dyn Expression>>,
    ) -> Self {
        Self {
            token,
            left,
            operator: operator.to_string(),
            right,
        }
    }
}

impl Node for InfixExpression {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }

    fn get_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for InfixExpression {
    fn expression_node(&self) {
        todo!()
    }

    fn to_string(&self) -> String {
        let right = if let Some(x) = &self.right {
            x.to_string()
        } else {
            "".to_owned()
        };
        let left = if let Some(x) = &self.left {
            x.to_string()
        } else {
            "".to_owned()
        };
        format!(
            "{}{} {} {}{}",
            "(",
            left,
            self.operator.as_str(),
            right,
            ")"
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::assert_eq;
    use std::str::FromStr;

    #[test]
    fn test_to_string() {
        let program = Program {
            statements: vec![Box::new(LetStatement {
                token: Token::Let,
                name: Identifier {
                    token: Token::Ident("myVar".to_string()),
                    value: "myVar".to_string(),
                },
                value: Some(Box::new(Identifier {
                    token: Token::Ident("anotherVar".to_string()),
                    value: "anotherVar".to_string(),
                })),
            })],
        };
        assert_eq!(program.to_string().as_str(), "let myVar = anotherVar;")
    }
}
