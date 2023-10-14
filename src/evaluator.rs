use crate::ast::*;
use crate::object::*;

pub fn eval_program(program: &Program) -> Box<dyn Object> {
    let mut result: Box<dyn Object> = Box::new(Null);
    for statement in &program.statements {
        result = eval(statement.as_ref());
    }
    result
}

pub fn eval(node: &dyn Statement) -> Box<dyn Object> {
    match node.as_any().downcast_ref::<ExpressionStatement>() {
        Some(expr) => {
            let expr = expr.expression.as_ref().unwrap().as_ref();
            eval_expression(expr)
        }
        None => unimplemented!(),
    }
}

/* pub fn eval_statement() -> Box<dyn Object> {
    unimplemented!()
} */

pub fn eval_expression(node: &dyn Expression) -> Box<dyn Object> {
    match node.as_any().downcast_ref::<IntegerLiteral>() {
        Some(expr) => Box::new(Integer { value: expr.value }),
        None => unimplemented!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    fn test_eval(input: &str) -> Box<dyn Object> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        eval_program(&program)
    }

    fn check_integer_object(obj: Box<dyn Object>, expected: i64) {
        match obj.as_any().downcast_ref::<Integer>() {
            Some(integer) => assert_eq!(integer.value, expected),
            None => panic!("object is not Integer. got={}", obj.type_name().to_string()),
        }
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![("5", 5), ("10", 10)];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            check_integer_object(evaluated, expected)
        }
    }
}
