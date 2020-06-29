use crate::lexer::{Lexer, Token};
use crate::parser::{Program, Statement, Expression};

#[derive(PartialOrd, PartialEq)]
pub enum Precedence {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL
}

impl Precedence {
    fn get_precedence(token: &Token) -> Self {
        match token {
            Token::EQ | Token::NOT_EQ => Precedence::EQUALS,
            Token::LT | Token::LT_EQ | Token::GT | Token::GT_EQ => Precedence::LESSGREATER,
            Token::ADD | Token::MINUS => Precedence::SUM,
            Token::ASTERISK | Token::SLASH => Precedence:: PRODUCT,
            Token::LPAREN => Precedence::CALL,
            _ => Precedence::LOWEST
        }        
    }
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer: lexer,
            cur_token: None,
            peek_token: None,
            errors: Vec::new(),
        };
        
        parser.next_token();
        parser.next_token();
        
        parser
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.take();
        self.peek_token = self.lexer.next_token();
    }

    
    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();
        
        while let Some(token) = &self.cur_token {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);               
            };
            
            self.next_token();
            
        }
        
        program
    }
    
    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Some(Token::LET) => self.parse_let_statement(),
            Some(Token::RETURN) => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }
    
    fn parse_let_statement(&mut self) -> Option<Statement> {
        if let Some(Token::IDENT(ident)) = &self.peek_token {
            let token = Token::IDENT(ident.to_string());
            self.next_token();
            if let Some(Token::ASSIGN) = self.peek_token {
                self.next_token();
                self.next_token();
                let expr = self.parse_expression(Precedence::LOWEST).unwrap();
                if self.peek_token == Some(Token::SEMICOLON) {
                    self.next_token();
                }

                return Some(Statement::Let(token, expr))       
            }
        } else {
            let msg = format!("Expected next token to be {}, got {:?} instead", "Option<Identifier>", self.peek_token);
            self.errors.push(msg);
        }
        None
    }
    
    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();
        
        let return_value = self.parse_expression(Precedence::LOWEST).unwrap();

        if self.peek_token == Some(Token::SEMICOLON) {
            self.next_token();
        } 
        
        Some(Statement::Return(return_value))
    }
    
    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let stmt = Statement::Expr(self.parse_expression(Precedence::LOWEST).unwrap());
        
        if let Some(Token::SEMICOLON) = &self.peek_token {
            self.next_token();
        }
        Some(stmt)
    }
    
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        if let Some(mut left) = self.parse_prefix() {
            while self.peek_token != Some(Token::SEMICOLON) && precedence < Precedence::get_precedence(self.peek_token.as_ref().unwrap_or(&Token::EOF)) {
                left = match self.parse_infix(left) {
                    Some(infix) => infix,
                    None => return None
                }
            }

            return Some(left)
        }
        None
    }
    
    fn parse_prefix(&mut self) -> Option<Expression> {
        match self.cur_token.as_ref() {
            Some(Token::IDENT(ident)) => Some(Expression::Ident(ident.to_string())),
            Some(Token::INT(int)) => Some(Expression::Int(*int)),
            Some(Token::MINUS) | Some(Token::BANG) => self.parse_prefix_expression(),
            Some(Token::TRUE) => Some(Expression::Bool(true)), 
            Some(Token::FALSE) => Some(Expression::Bool(false)),
            Some(Token::LPAREN) => self.parse_grouped_expression(),
            Some(Token::IF) => self.parse_if_expression(),
            Some(Token::FUNCTION) => self.parse_function_expression(),
            Some(Token::STRING(string)) => Some(Expression::Str(string.to_string())),
            _ => None

        }
    }

    fn parse_function_expression(&mut self) -> Option<Expression> {
       if self.peek_token != Some(Token::LPAREN) {
            return None
       }
       self.next_token();

       let parameters = self.parse_function_parameters().unwrap();

       if self.peek_token != Some(Token::LBRACE) {
            return None
       }
       self.next_token(); 
       let body = self.parse_block_statement().unwrap();

       return Some(Expression::Function(parameters, Box::new(body)))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Token>> {
        let mut identifiers = Vec::new();

        if self.peek_token == Some(Token::RPAREN) {
            self.next_token();
            return Some(identifiers)
        }

        self.next_token();
        identifiers.push(self.cur_token.take().unwrap());
        while self.peek_token == Some(Token::COMMA) {
            self.next_token();
            self.next_token();

            identifiers.push(self.cur_token.take().unwrap());
        }

        if self.peek_token != Some(Token::RPAREN) {
            return None 
        }
        self.next_token();

        Some(identifiers)
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if self.peek_token != Some(Token::LPAREN) {
            return None   
        }

        self.next_token();
        let condition = self.parse_expression(Precedence::LOWEST).unwrap();
        if self.peek_token != Some(Token::LBRACE) {
            return None
        }
        self.next_token();

        let consequence = self.parse_block_statement().unwrap();
        if self.peek_token == Some(Token::ELSE) {
            self.next_token();

            if self.peek_token != Some(Token::LBRACE) {
                return None
            }
            self.next_token();
            let alternative = self.parse_block_statement().unwrap();

            return Some(Expression::If(Box::new(condition), Box::new(consequence), Some(Box::new(alternative))))
        }
        Some(Expression::If(Box::new(condition), Box::new(consequence), None))
    }

    fn parse_block_statement(&mut self) -> Option<Statement> {
        let mut statements = Vec::new();
        self.next_token();
        while self.cur_token != Some(Token::RBRACE) && self.cur_token != None {
           if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
           }
           self.next_token();
        }

        Some(Statement::Block(statements))
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let exp = self.parse_expression(Precedence::LOWEST);   

        if self.peek_token != Some(Token::RPAREN) {
            return None
        }

        self.next_token();
        exp
    }

    fn parse_infix(&mut self, left: Expression) -> Option<Expression> {
        match self.peek_token.as_ref() {
            Some(Token::ADD) | Some(Token::MINUS) | Some(Token::SLASH) | Some(Token::ASTERISK) | 
            Some(Token::EQ) | Some(Token::NOT_EQ) | Some(Token::LT) | Some(Token::LT_EQ) | 
            Some(Token::GT) | Some(Token::GT_EQ) => self.parse_infix_expression(left),
            Some(Token::LPAREN) => self.parse_call_expression(left),
            _ => None
        }
    }

    fn parse_call_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();
        if let Some(args) = self.parse_call_arguments() {
            return Some(Expression::Call(Box::new(left), args))
        }

        None
        
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Expression>> {
        let mut args = Vec::new();

        if self.peek_token == Some(Token::RPAREN) {
            self.next_token();
            self.next_token();
            return Some(args)
        }

        self.next_token();
        args.push(self.parse_expression(Precedence::LOWEST).unwrap());
        while self.peek_token == Some(Token::COMMA) {
            self.next_token();
            self.next_token();

            args.push(self.parse_expression(Precedence::LOWEST).unwrap());
        }
        
        if self.peek_token != Some(Token::RPAREN) {
            return None
        }

        self.next_token();

        Some(args)

    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();
        let operator = self.cur_token.take().unwrap();
        let precedence = Precedence::get_precedence(&operator);
        self.next_token();
        let right = self.parse_expression(precedence).unwrap_or(Expression::Null);

        Some(Expression::Infix(Box::new(left), operator, Box::new(right)))

    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let cur_token = self.cur_token.take().unwrap();

        self.next_token();

        let right_token = self.parse_expression(Precedence::PREFIX).unwrap();

        Some(Expression::Prefix(cur_token, Box::new(right_token)))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = "
            let x = 5;
            let y = 10;
            let foobar = y;
        ".to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 3);

        let expected = vec![
            Statement::Let(Token::IDENT("x".to_string()), Expression::Int(5)),
            Statement::Let(Token::IDENT("y".to_string()), Expression::Int(10)),
            Statement::Let(Token::IDENT("foobar".to_string()), Expression::Ident("y".to_string())),
        ];

        for i in 0..expected.len() {
            assert_eq!(program.statements[i], expected[i]);
        }
    }

    #[test]
    fn test_return_statement() {
        let input = "
            return 5;
            return 10;
            return y;
        ".to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 3);

        let expected = vec![
            Statement::Return(Expression::Int(5)),
            Statement::Return(Expression::Int(10)),
            Statement::Return(Expression::Ident("y".to_string()))
        ];

        for i in 0..expected.len() {
            assert_eq!(program.statements[i], expected[i]);
        }
    }
    
    #[test]
    fn test_identifier_expression() {
        let input = "foobar".to_string();

        let program = parse_program(input);

        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.statements[0], Statement::Expr(Expression::Ident("foobar".to_string())));
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;".to_string();
        let program = parse_program(input);
        
        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.statements[0], Statement::Expr(Expression::Int(5)));
    }

    #[test]
    fn test_boolean_expression() {
        let input = "
            true;
            false;
        ".to_string();

        let program = parse_program(input);

        let expected = vec![
            Statement::Expr(Expression::Bool(true)),
            Statement::Expr(Expression::Bool(false)),
            // Statement::Let(Token::IDENT("foobar".to_string()), Expression::Bool(true)),
            // Statement::Let(Token::IDENT("barfoo".to_string()), Expression::Bool(false)),
        ];

        for i in 0..expected.len() {
            assert_eq!(expected[i], program.statements[i]);
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let input = "
            !5;
            -15;
        ".to_string();

        let program = parse_program(input);

        let expected = vec![
            Statement::Expr(Expression::Prefix(Token::BANG, Box::new(Expression::Int(5)))),
            Statement::Expr(Expression::Prefix(Token::MINUS, Box::new(Expression::Int(15))) ),
        ];

        assert_eq!(program.statements.len(), 2);

        for i in 0..expected.len() {
            assert_eq!(expected[i], program.statements[i]);
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let input = "
            5 + 5;
            5 - 5;
            5 * 5;
            5 / 5;
            5 > 5;
            5 < 5;
            5 >= 5;
            5 <= 5;
            5 == 5;
            5 != 5;
        ".to_string();

        let program = parse_program(input);

        let expected = vec![
            Statement::Expr(Expression::Infix(Box::new(Expression::Int(5)), Token::ADD, Box::new(Expression::Int(5)))),
            Statement::Expr(Expression::Infix(Box::new(Expression::Int(5)), Token::MINUS, Box::new(Expression::Int(5)))),
            Statement::Expr(Expression::Infix(Box::new(Expression::Int(5)), Token::ASTERISK, Box::new(Expression::Int(5)))),
            Statement::Expr(Expression::Infix(Box::new(Expression::Int(5)), Token::SLASH, Box::new(Expression::Int(5)))),
            Statement::Expr(Expression::Infix(Box::new(Expression::Int(5)), Token::GT, Box::new(Expression::Int(5)))),
            Statement::Expr(Expression::Infix(Box::new(Expression::Int(5)), Token::LT, Box::new(Expression::Int(5)))),
            Statement::Expr(Expression::Infix(Box::new(Expression::Int(5)), Token::GT_EQ, Box::new(Expression::Int(5)))),
            Statement::Expr(Expression::Infix(Box::new(Expression::Int(5)), Token::LT_EQ, Box::new(Expression::Int(5)))),
            Statement::Expr(Expression::Infix(Box::new(Expression::Int(5)), Token::EQ, Box::new(Expression::Int(5)))),
            Statement::Expr(Expression::Infix(Box::new(Expression::Int(5)), Token::NOT_EQ, Box::new(Expression::Int(5)))),
        ];

        assert_eq!(program.statements.len(), 10);

        for i in 0..expected.len() {
            assert_eq!(expected[i], program.statements[i]);
        }
    }

    #[test]
    fn test_operator_precendence_parsing() {
        let input = "
            -a * b;
            (a + b) * c;
            a + b / c;
            a + b * c + d / e - f;
            5 > 4 == 3 < 4;
        ".to_string();

        let program = parse_program(input);

        let expected = vec![
            //-a * b
            Statement::Expr(Expression::Infix(
                Box::new(Expression::Prefix(Token::MINUS, Box::new(Expression::Ident("a".to_string())))), 
                Token::ASTERISK,
                Box::new(Expression::Ident("b".to_string())),
            )),
            //(a + b) * c;
            Statement::Expr(Expression::Infix(
                Box::new(Expression::Infix(
                    Box::new(Expression::Ident("a".to_string())),
                    Token::ADD,
                    Box::new(Expression::Ident("b".to_string()))
                )),
                Token::ASTERISK,
                Box::new(Expression::Ident("c".to_string()))                
            )),
            //a + b / c;
            Statement::Expr(Expression::Infix(
                Box::new(Expression::Ident("a".to_string())), 
                Token::ADD,
                Box::new(Expression::Infix(
                    Box::new(Expression::Ident("b".to_string())),
                    Token::SLASH,
                    Box::new(Expression::Ident("c".to_string()))
                )),
            )),
            //a + b * c + d / e - f;
            Statement::Expr(Expression::Infix(
                Box::new(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Ident("a".to_string())),
                        Token::ADD,
                        Box::new(Expression::Infix(
                            Box::new(Expression::Ident("b".to_string())),
                            Token::ASTERISK,
                            Box::new(Expression::Ident("c".to_string()))
                        ))
                    )),
                    Token::ADD,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Ident("d".to_string())),
                        Token::SLASH,
                        Box::new(Expression::Ident("e".to_string()))
                    ))
                )),
                Token::MINUS,
                Box::new(Expression::Ident("f".to_string()))

            )),
            //5 > 4 == 3 < 4;
            Statement::Expr(Expression::Infix(
                Box::new(Expression::Infix(
                    Box::new(Expression::Int(5)),
                    Token::GT,
                    Box::new(Expression::Int(4))
                )),
                Token::EQ,
                Box::new(Expression::Infix(
                    Box::new(Expression::Int(3)),
                    Token::LT,
                    Box::new(Expression::Int(4))
                )),
            ))


        ];

        for i in 0..expected.len() {
            assert_eq!(expected[i], program.statements[i]);
        }
        
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }".to_string();
        
        let program = parse_program(input);

        let expected = Statement::Expr(
            Expression::If(
                Box::new(Expression::Infix(
                    Box::new(Expression::Ident("x".to_string())),
                    Token::LT,
                    Box::new(Expression::Ident("y".to_string()))
                )),
                Box::new(Statement::Block(vec![
                    Statement::Expr(Expression::Ident("x".to_string()))
                ])),
                None
        ));
        
        assert_eq!(program.statements.len(), 1);
        assert_eq!(expected, program.statements[0]);
        
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) {x} else {y}".to_string();

        let program = parse_program(input);

        let expected = Statement::Expr(
           Expression::If(
                Box::new(Expression::Infix(
                    Box::new(Expression::Ident("x".to_string())),
                    Token::LT,
                    Box::new(Expression::Ident("y".to_string()))
                )),
                Box::new(Statement::Block(vec![
                    Statement::Expr(Expression::Ident("x".to_string()))
                ])),
                Some(Box::new(Statement::Block(vec![
                    Statement::Expr(Expression::Ident("y".to_string()))
                ])))
        ));

        assert_eq!(program.statements.len(), 1);
        assert_eq!(expected, program.statements[0]);
    }

    #[test]
    fn test_function_literal_expression() {
        let input = "
            fn() {};
            fn(x) {};
            fn(x, y) {};

        ".to_string();

        let program = parse_program(input);
        
        let expected = vec![
            Statement::Expr(
            Expression::Function(
                vec![],
                Box::new(Statement::Block(
                    vec![]
                )
            ))),
            Statement::Expr(Expression::Function(
                vec![Token::IDENT("x".to_string())],
                Box::new(Statement::Block(
                    vec![]
                )
            ))),
            Statement::Expr(Expression::Function(
                vec![Token::IDENT("x".to_string()), Token::IDENT("y".to_string())],
                Box::new(Statement::Block(
                    vec![]
                )
            )))
        ];

        assert_eq!(program.statements.len(), 3);
        for i in 0..program.statements.len() {
            assert_eq!(program.statements[i], expected[i]);
        }

    }

    #[test]
    fn test_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5)".to_string();
        let program = parse_program(input);

        let expected = vec![
            Statement::Expr(Expression::Call(
                Box::new(Expression::Ident("add".to_string())),
                vec![
                    Expression::Int(1),
                    Expression::Infix(Box::new(Expression::Int(2)), Token::ASTERISK, Box::new(Expression::Int(3))),
                    Expression::Infix(Box::new(Expression::Int(4)), Token::ADD, Box::new(Expression::Int(5)))
                ]
            ))
        ];

        for i in 0..program.statements.len() {
            assert_eq!(program.statements[i], expected[i]);
        }

    }

    #[test]
    fn test_call_expression2() {
        let input = "fn(x) { x; }(5)".to_string();
        let program = parse_program(input);

        let expected = vec![
            Statement::Expr(Expression::Call(
                Box::new(Expression::Function(
                    vec![Token::IDENT("x".to_string())],
                    Box::new(Statement::Block(vec![Statement::Expr(Expression::Ident("x".to_string())),]))
                )),
                vec![
                    Expression::Int(5),
                ]
            ))
        ];

        for i in 0..program.statements.len() {
            assert_eq!(program.statements[i], expected[i]);
        }

    }

    #[test]
    fn test_string_literal_expression() {
        let input = "\"Hello World\"".to_string();

        let program = parse_program(input);

        let expected = Statement::Expr(Expression::Str("Hello World".to_string()));

        assert_eq!(program.statements[0], expected);

    }

    fn parse_program(input: String) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        parser.parse_program()
    }

    
}
