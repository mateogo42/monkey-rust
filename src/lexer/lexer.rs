use crate::lexer::Token;

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input: input,
            position: 0,
            read_position: 0,
            ch: '\0'
        };
        lexer.read_char();
        lexer
    }

    pub fn check_eq(&mut self, true_token: Token, false_token: Token) -> Option<Token> {
        if self.peek_char() == '=' {
            self.read_char();
            Some(true_token)
        } else {
            Some(false_token)
        }
    }

    pub fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            return '\0'
        }

        self.input.chars().nth(self.read_position).unwrap()
    }

    pub fn read_number(&mut self) -> Token {
        let position = self.position;
        while self.ch.is_numeric() {
            self.read_char();
        }

        let number: i32 = self.input[position..self.position].parse().unwrap();

        Token::INT(number)
    }

    pub fn read_identifier(&mut self) -> Token {
        let position = self.position;
        while self.ch.is_alphabetic() || self.ch == '_' {
            self.read_char();
        }
        
        let ident = &self.input[position..self.position];
        match ident {
            "let" => Token::LET,
            "fn" => Token::FUNCTION,
            "if" => Token::IF,
            "else" => Token::ELSE,
            "return" => Token::RETURN,
            "true" => Token::TRUE,
            "false" => Token::FALSE,
            _ => Token::IDENT(ident.to_string())
        }
    }

    pub fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.read_position).unwrap();
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Option<Token> {
        while self.ch.is_whitespace() {
            self.read_char();
        }

        let token = match self.ch {
            '+' => Some(Token::ADD),
            '(' => Some(Token::LPAREN),
            ')' => Some(Token::RPAREN),
            '{' => Some(Token::LBRACE),
            '}' => Some(Token::RBRACE),
            ',' => Some(Token::COMMA),
            ';' => Some(Token::SEMICOLON),
            '-' => Some(Token::MINUS),
            '/' => Some(Token::SLASH),
            '*' => Some(Token::ASTERISK),
            '\0' => None,
            '=' => self.check_eq(Token::EQ, Token::ASSIGN),
            '!' => self.check_eq(Token::NOT_EQ, Token::BANG),
            '<' => self.check_eq(Token::LT_EQ, Token::LT),
            '>' => self.check_eq(Token::GT_EQ, Token::GT),
            _ => {
                if self.ch.is_alphabetic() || self.ch == '_' {
                    return  Some(self.read_identifier())
                } else if self.ch.is_numeric() {
                    return Some(self.read_number())
                } else {
                    Some(Token::ILLEGAL)
                }
            }
        };

        self.read_char();
        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = String::from("
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);

            if (5 < 10) {
                return true;
                } else {
                return false;
            }
            
            10 == 10;
            10 != 9;

            ");

        let expected_results = vec![
            Token::LET,
            Token::IDENT("five".to_string()),
            Token::ASSIGN,
            Token::INT(5),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("ten".to_string()),
            Token::ASSIGN,
            Token::INT(10),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("add".to_string()),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT("x".to_string()),
            Token::COMMA,
            Token::IDENT("y".to_string()),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT("x".to_string()),
            Token::ADD,
            Token::IDENT("y".to_string()),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("result".to_string()),
            Token::ASSIGN,
            Token::IDENT("add".to_string()),
            Token::LPAREN,
            Token::IDENT("five".to_string()),
            Token::COMMA,
            Token::IDENT("ten".to_string()),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::IF,
            Token::LPAREN,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::RPAREN,
            Token::LBRACE,
            Token::RETURN,
            Token::TRUE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::ELSE,
            Token::LBRACE,
            Token::RETURN,
            Token::FALSE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::INT(10),
            Token::EQ,
            Token::INT(10),
            Token::SEMICOLON,
            Token::INT(10),
            Token::NOT_EQ,
            Token::INT(9),
            Token::SEMICOLON,            
        ];

        let mut lexer = Lexer::new(input);

        for token in expected_results {
            let result = lexer.next_token().unwrap();
            assert_eq!(result, token);
        }
    }
}