

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Token {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT(String),
    INT(i32),
    STRING(String),
    // Operators
    ASSIGN,
    ADD,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    LT,
    GT,
    LT_EQ,
    GT_EQ,

    EQ,
    NOT_EQ,

    // Delimiters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN
}