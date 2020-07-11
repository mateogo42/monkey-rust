use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
    Expr(Expression),
    Block(Vec<Statement>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Int(i32),
    Str(String),
    Ident(String),
    Bool(bool),
    Prefix(Token, Box<Expression>),
    Infix(Box<Expression>, Token, Box<Expression>),
    If(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    Function(Vec<Identifier>, Box<Statement>),
    Call(Box<Expression>, Vec<Expression>),
    Null

}

pub struct Program {
    pub statements: Vec<Statement>,
    pub errors: Vec<String>
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
            errors: Vec::new()
        }
    }
}

pub type Identifier = Token;
