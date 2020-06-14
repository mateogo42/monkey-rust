use crate::eval::Object;
use crate::parser::{ Statement, Program, Parser, Expression };
use crate::lexer::{Lexer, Token};

pub fn parse_ast(statements: &[Statement]) -> Option<Object> {
    None
}

pub fn eval_program(program: Program) -> Option<Object> {
    let mut result = None;
    for stmt in program.statements {
        result = eval_statement(stmt);
    }

    result
}

pub fn eval_statement(stmt: Statement) -> Option<Object> {
    match stmt {
        Statement::Expr(expression) => eval_expression(expression),
        _ => None
    }
}

pub fn eval_expression(expr: Expression) -> Option<Object> {
    match expr {
        Expression::Int(value) => Some(Object::Int(value)),
        Expression::Bool(value) => Some(Object::Bool(value)),
        Expression::Prefix(operator, value) => {
            let right = eval_expression(*value).unwrap();
            eval_prefix_expression(operator, right)
        },
        Expression::Infix(left_expr, operator, right_expr) => {
            let left = eval_expression(*left_expr).unwrap();
            let right = eval_expression(*right_expr).unwrap();

            eval_infix_expression(operator, left, right)
        },
        _ => None
    }
}

pub fn eval_infix_expression(operator: Token, left: Object, right: Object) -> Option<Object> {
    match (left, right) {
        (Object::Int(left_value), Object::Int(right_value)) => eval_integer_infix_expression(operator, left_value, right_value),
        _ => None
    }
}

fn eval_integer_infix_expression(operator: Token, left: i32, right: i32) -> Option<Object> {
    match operator {
        Token::ADD => Some(Object::Int(left + right)),
        Token::MINUS => Some(Object::Int(left - right)),
        Token::ASTERISK => Some(Object::Int(left * right)),
        Token::SLASH => Some(Object::Int(left / right)),
        _ => None
    }
}

pub fn eval_prefix_expression(operator: Token, right: Object) -> Option<Object> {
    match operator {
        Token::BANG => eval_bang_operator_expression(right),
        Token::MINUS => eval_minus_prefix_operator_expression(right),
        _ => None
    }
}

pub fn eval_bang_operator_expression(right: Object) -> Option<Object> {
    match right {
        Object::Bool(false) => Some(Object::Bool(true)),
        _ => Some(Object::Bool(false)),
    }
}

pub fn eval_minus_prefix_operator_expression(right: Object) -> Option<Object> {
    match right {
        Object::Int(value) => Some(Object::Int(-value)),
        _ => None
    }
}

#[cfg(test)]
mod test {
    use super::*;
    
    #[test]
    fn test_eval_int_expression() {
        let input = "
            5;
            10;
            -5;
            -10;
            5 + 5 + 5 + 5 - 10;
            3 * (3 * 3) + 10;
            (5 + 10 * 2 + 15 / 3) * 2 + -10;
        ".to_string();
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let evalued: Vec<Object> = program.statements.into_iter().map(|stmt| eval_statement(stmt).unwrap()).collect();
        let expected = vec![
            Object::Int(5),
            Object::Int(10),
            Object::Int(-5),
            Object::Int(-10),
            Object::Int(10),
            Object::Int(37),
            Object::Int(50)
        ];

        for i in 0..evalued.len() {
            assert_eq!(evalued[i], expected[i]);
        }
    }

    fn test_eval_bool_expression() {
        let input = "
            true;
            false;
        ".to_string();
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let evalued: Vec<Object> = program.statements.into_iter().map(|stmt| eval_statement(stmt).unwrap()).collect();
        let expected = vec![
            Object::Bool(true),
            Object::Bool(false)
        ];

        for i in 0..evalued.len() {
            assert_eq!(evalued[i], expected[i]);
        }
    }

    #[test]
    fn test_bang_operator() {
        let input = "
            !true;
            !false;
            !5;
            !!true;
            !!false;
            !!5
        ".to_string();

        let expected = vec![
            Object::Bool(false),
            Object::Bool(true),
            Object::Bool(false),
            Object::Bool(true),
            Object::Bool(false),
            Object::Bool(true),
        ];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let evalued: Vec<Object> = program.statements.into_iter().map(|stmt| eval_statement(stmt).unwrap()).collect();

        for i in 0..evalued.len() {
            assert_eq!(evalued[i], expected[i]);
        }
    }
}
