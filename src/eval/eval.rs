use crate::eval::{Object, Environment};
use crate::parser::{ Statement, Program, Parser, Expression };
use crate::lexer::{Lexer, Token};

type EvalResult<T> = Result<T, String>;

pub fn eval_statements(statements: Vec<Statement>, environment: &mut Environment) -> EvalResult<Object> {
    let mut result = Object::Null;
    for stmt in statements {
        if let Statement::Return(expr) = stmt {
            return eval_expression(expr, environment) 
        }
        result = eval_single_statement(stmt, environment)?;
    }

    Ok(result)
}

pub fn eval_single_statement(stmt: Statement, environment: &mut Environment) -> EvalResult<Object> {
    match stmt {
        Statement::Expr(expression) => eval_expression(expression, environment),
        Statement::Block(statements) => eval_statements(statements, environment),
        Statement::Return(expression) => eval_expression(expression, environment),
        Statement::Let(Token::IDENT(ident), expression) => eval_let_statement(ident, expression, environment),
        _ => Err("ERROR: Invalid syntax".to_string())
    }
}


pub fn eval_let_statement(ident: String, expression: Expression, environment: &mut Environment) -> EvalResult<Object> {
    let value = eval_expression(expression, environment)?;
    environment.set(ident, value);
    Ok(Object::Null)
}

pub fn eval_expression(expr: Expression, environment: &mut Environment) -> EvalResult<Object> {
    match expr {
        Expression::Int(value) => Ok(Object::Int(value)),
        Expression::Bool(value) => Ok(Object::Bool(value)),
        Expression::Str(value) => Ok(Object::Str(value)),
        Expression::Prefix(operator, value) => {
            let right = eval_expression(*value, environment)?;
            eval_prefix_expression(operator, right)
        },
        Expression::Infix(left_expr, operator, right_expr) => {
            let left = eval_expression(*left_expr, environment)?;
            let right = eval_expression(*right_expr, environment)?;
            
            eval_infix_expression(operator, left, right)
        },
        Expression::If(cond, cons, alt) => {
            let condition = eval_expression(*cond, environment)?;
            if is_truthy(condition) {
                return eval_single_statement(*cons, environment)
            } else if alt != None {
                return eval_single_statement(*alt.unwrap(), environment);
            }
            Ok(Object::Null)
        },
        Expression::Ident(ident) => environment.get(ident.clone()).ok_or_else(|| format!("ERROR: {} is not defined", ident)),
        Expression::Function(parameters, body) => Ok(Object::Function(parameters, body)),
        Expression::Call(ident, parameters) => {
            let body = eval_expression(*ident, environment)?;
            let args: Vec<Object> = parameters.into_iter().map(|param| eval_expression(param, environment).unwrap()).collect();
            
            apply_function(body, args, environment.clone())
        }
        _ => Err("ERROR: Invalid Expression".to_string())
    }
}

fn apply_function(function: Object, args: Vec<Object>, env: Environment) -> EvalResult<Object> {
    match function {
        Object::Function(parameters, body) => {
            let mut extended_env = extend_environment(parameters, args, env);
            eval_single_statement(*body, &mut extended_env)
        },
        Object::Builtin(func) => func(args),
        _ => Err(format!("ERROR: Identifier {:?} not defined", function))
    }
}

fn extend_environment(parameters: Vec<Token>, args: Vec<Object>, outer: Environment) -> Environment {
    let mut env = Environment::new_enclosed_by(outer);

    for i in 0..parameters.len() {
        if let Token::IDENT(name) = &parameters[i] {
            env.set(name.to_string(), args[i].clone());
        }
    }

    env
}



fn is_truthy(condition: Object) -> bool {
    match condition {
        Object::Bool(false) | Object::Null => false,
        _ => true
    }
}

pub fn eval_infix_expression(operator: Token, left: Object, right: Object) -> EvalResult<Object> {
    match (left, right) {
        (Object::Int(left_value), Object::Int(right_value)) => eval_integer_infix_expression(operator, left_value, right_value),
        (Object::Bool(left_value), Object::Bool(right_value)) => eval_boolean_infix_expression(operator, left_value, right_value),
        (Object::Str(left_value), Object::Str(right_value)) => eval_string_infix_expression(operator, left_value, right_value),
        _ => Err("ERROR: Invalid operation".to_string())
    }
}

fn eval_string_infix_expression(operator: Token, left: String, right: String) -> EvalResult<Object> {
    match operator {
        Token::ADD => {
            let mut owned_left = left.to_owned();
            let owned_right = right.to_owned();
            owned_left.push_str(&owned_right);
            return Ok(Object::Str(owned_left))
        },
        _ => Err("ERROR: Invalid operation for type string".to_string())
    }
}

fn eval_boolean_infix_expression(operator: Token, left: bool, right: bool) -> EvalResult<Object> {
    
    match operator {
        Token::EQ => Ok(Object::Bool(left == right)),
        Token::NOT_EQ => Ok(Object::Bool(left != right)),
        _ => Err("ERROR: Invalid operation for type bool".to_string())
    }
}

fn eval_integer_infix_expression(operator: Token, left: i32, right: i32) -> EvalResult<Object> {
    match operator {
        Token::ADD => Ok(Object::Int(left + right)),
        Token::MINUS => Ok(Object::Int(left - right)),
        Token::ASTERISK => Ok(Object::Int(left * right)),
        Token::SLASH => Ok(Object::Int(left / right)),
        Token::LT => Ok(Object::Bool(left < right)),
        Token::LT_EQ => Ok(Object::Bool(left <= right)),
        Token::GT => Ok(Object::Bool(left > right)),
        Token::GT_EQ => Ok(Object::Bool(left >= right)),
        Token::EQ => Ok(Object::Bool(left == right)),
        Token::NOT_EQ => Ok(Object::Bool(left != right)),
        _ => Err("ERROR: invalid operation for type int".to_string())
    }
}

pub fn eval_prefix_expression(operator: Token, right: Object) -> EvalResult<Object> {
    match operator {
        Token::BANG => eval_bang_operator_expression(right),
        Token::MINUS => eval_minus_prefix_operator_expression(right),
        _ => Err("ERROR: ".to_string())
    }
}

pub fn eval_bang_operator_expression(right: Object) -> EvalResult<Object> {
    match right {
        Object::Bool(false) => Ok(Object::Bool(true)),
        _ => Ok(Object::Bool(false)),
    }
}

pub fn eval_minus_prefix_operator_expression(right: Object) -> EvalResult<Object> {
    match right {
        Object::Int(value) => Ok(Object::Int(-value)),
        _ => Err("ERROR: ".to_string())
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
        let mut environment = Environment::new();
        let evalued: Vec<Object> = program.statements.into_iter().map(|stmt| eval_single_statement(stmt, &mut environment).unwrap()).collect();
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
            1 < 2;
            1 > 2;
            1 == 1;
            1 != 1;
            (1 < 2) == true;
            (1 < 2) == false;
        ".to_string();
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let mut environment = Environment::new();
        let evalued: Vec<Object> = program.statements.into_iter().map(|stmt| eval_single_statement(stmt, &mut environment).unwrap()).collect();
        let expected = vec![
            Object::Bool(true),
            Object::Bool(false),
            Object::Bool(true),
            Object::Bool(false),
            Object::Bool(true),
            Object::Bool(false),
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
        let mut environment = Environment::new();
        let evalued: Vec<Object> = program.statements.into_iter().map(|stmt| eval_single_statement(stmt, &mut environment).unwrap()).collect();

        for i in 0..evalued.len() {
            assert_eq!(evalued[i], expected[i]);
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let input = "
            if (true) {10};
            if (false) {10};
            if (1 > 2) { 10 } else { 20 };
            if (1 < 2) { 10 } else { 20 };
            
        ".to_string();

        let expected = vec![
            Object::Int(10),
            Object::Null,
            Object::Int(20),
            Object::Int(10)
        ];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let mut environment = Environment::new();
        let evalued: Vec<Object> = program.statements.into_iter().map(|stmt| eval_single_statement(stmt, &mut environment).unwrap()).collect();
        for i in 0..evalued.len() {
            assert_eq!(evalued[i], expected[i]);
        }
    }

    #[test]
    fn test_return_statements() {
        let test_cases = vec![
            ("return 10;".to_string(), Object::Int(10)),
            ("return 10; 9;".to_string(), Object::Int(10)),
            ("return 2 * 5; 9;".to_string(), Object::Int(10)),
            ("9; return 2 * 5; 9;".to_string(), Object::Int(10)),
        ];

        for test in test_cases {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            let mut environment = Environment::new();
            assert_eq!(eval_statements(program.statements, &mut environment).unwrap(), test.1);
        }
    }

    #[test]
    fn test_let_statements() {
        let test_cases = vec![
            ("let a = 5; a;".to_string(), Object::Int(5)),
            ("let a = 5 * 5; a;".to_string(), Object::Int(25)),
            ("let a = 5; let b = a; b;".to_string(), Object::Int(5)),
            ("let a = 5; let b = a; let c = a + b + 5; c;".to_string(), Object::Int(15))
        ];

        for test in test_cases {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            let mut environment = Environment::new();
            assert_eq!(eval_statements(program.statements, &mut environment).unwrap(), test.1);
        }
    }

    #[test]
    fn test_function_object() {
        let test_cases = vec![
            ("fn (x) { x + 2; };".to_string(), Object::Function(
                vec![Token::IDENT("x".to_string())], 
                Box::new(
                    Statement::Block(vec![
                        Statement::Expr(Expression::Infix(
                            Box::new(Expression::Ident("x".to_string())),
                            Token::ADD,
                            Box::new(Expression::Int(2))
                        ))
                    ])
                ),
                Box::new(Environment::new())
            ))
        ];

        for test in test_cases {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            let mut environment = Environment::new();
            assert_eq!(eval_statements(program.statements, &mut environment).unwrap(), test.1);
        }
    }

    #[test]
    fn test_function_application() {
        let test_cases = vec![
            ("let identity = fn(x) { x; }; identity(5);".to_string(), Object::Int(5)),
            ("let identity = fn(x) { return x; }; identity(5);".to_string(), Object::Int(5)),
            ("let double = fn(x) { x * 2; }; double(5);".to_string(), Object::Int(10)),
            ("let add = fn(x, y) { x + y; }; add(5, 5);".to_string(), Object::Int(10)),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));".to_string(), Object::Int(20)),
            ("fn(x) { x; }(5)".to_string(), Object::Int(5))
        ];

        for test in test_cases {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            let mut environment = Environment::new();
            assert_eq!(eval_statements(program.statements, &mut environment).unwrap(), test.1);
        }
    }
    
    #[test]
    fn test_string_literal() {
        let input = "\"Hello World\"".to_string();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let mut environment = Environment::new();
        let evaluated = eval_statements(program.statements, &mut environment).unwrap();
        assert_eq!(evaluated, Object::Str("Hello World".to_string()));
    }

    #[test]
    fn test_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"World!\"".to_string();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let mut environment = Environment::new();
        let evaluated = eval_statements(program.statements, &mut environment).unwrap();
        assert_eq!(evaluated, Object::Str("Hello World!".to_string()))
    }

    #[test]
    fn test_builtin_functions() {
        let test_cases = vec![
            ("len(\"\")".to_string(), Object::Int(0)),
            ("len(\"four\")".to_string(), Object::Int(4)),
            ("len(\"hello world\")".to_string(), Object::Int(11)),
        ];

        for test in test_cases {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            let mut environment = Environment::new();
            assert_eq!(eval_statements(program.statements, &mut environment).unwrap(), test.1);
        }
    }
}
