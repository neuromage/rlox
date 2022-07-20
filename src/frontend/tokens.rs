use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub column: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let placeholder: String;
        let ch = match &self.token_type {
            TokenType::LeftParen => "(",
            TokenType::RightParen => ")",
            TokenType::LeftBrace => "{",
            TokenType::RightBrace => "}",
            TokenType::True => "true",
            TokenType::False => "false",
            TokenType::And => "and",
            TokenType::Or => "or",

            TokenType::Bang => "!",
            TokenType::BangEqual => "!=",
            TokenType::LessThan => "<",
            TokenType::LessThanEqual => "<=",
            TokenType::GreaterThan => "<",
            TokenType::GreaterThanEqual => ">=",
            TokenType::Equal => "=",
            TokenType::EqualEqual => "==",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Star => "*",
            TokenType::Slash => "/",
            TokenType::Comma => ",",
            TokenType::Dot => ".",
            TokenType::Return => "return",
            TokenType::If => "if",
            TokenType::Else => "else",
            TokenType::While => "while",
            TokenType::For => "for",
            TokenType::Fun => "fun",
            TokenType::Var => "vae",
            TokenType::Class => "class",
            TokenType::This => "this",
            TokenType::Super => "super",
            TokenType::Identifier(v) => &v,
            TokenType::String(v) => &v,
            TokenType::Int(v) => {
                placeholder = v.to_string();
                &placeholder
            }
            TokenType::Float(v) => {
                placeholder = v.to_string();
                &placeholder
            }
            TokenType::Print => "print",
            TokenType::Nil => "nil",
            TokenType::Eof => "EOF",
        };
        write!(f, "`{}` (line: {}, column: {})", ch, self.line, self.column)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    // Conditionals
    True,
    False,
    And,
    Or,

    // Operators
    Bang,
    BangEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    EqualEqual,
    Plus,
    Minus,
    Star,
    Slash,
    Comma,
    Dot,

    // Control Flow
    Return,
    If,
    Else,
    While,
    For,

    // Keywords
    Fun,
    Var,
    Class,
    This,
    Super,

    Identifier(String),

    // Literals.
    String(String),
    Int(i64),
    Float(f64),

    // Built in functions.
    Print,

    Nil,
    Eof,
}
