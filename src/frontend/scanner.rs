use lazy_static::lazy_static;
use miette::{Diagnostic, SourceSpan};
use std::collections::HashMap;
use std::fmt::Display;
use std::iter::Peekable;
use thiserror::Error;

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
    Semicolon,

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
    Number(f64),

    // Built in functions.
    Print,

    Nil,
    Eof,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub column: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lexeme: &str = match self.token_type {
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
            TokenType::GreaterThan => ">",
            TokenType::GreaterThanEqual => ">=",
            TokenType::Equal => "=",
            TokenType::EqualEqual => "==",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Star => "*",
            TokenType::Slash => "/",
            TokenType::Comma => ",",
            TokenType::Dot => ".",
            TokenType::Semicolon => ";",

            TokenType::Return => "return",
            TokenType::If => "if",
            TokenType::Else => "else",
            TokenType::While => "while",
            TokenType::For => "for",

            TokenType::Fun => "fun",
            TokenType::Var => "var",
            TokenType::Class => "class",
            TokenType::This => "this",
            TokenType::Super => "super",

            TokenType::Identifier(ref s) => s,
            TokenType::String(ref s) => s,
            TokenType::Number(ref n) => &n.to_string(),

            TokenType::Print => "print",

            TokenType::Nil => "nil",
            TokenType::Eof => "EOF",
        };
        write!(
            f,
            "`{}` (type: {:?} line: {}, column: {})",
            lexeme, self.token_type, self.line, self.column
        )
    }
}

#[derive(Debug, Error, PartialEq, Diagnostic)]
pub enum ScannerError {
    #[error("invalid char '{ch:}' at line:{line:} col:{column:}'")]
    InvalidChar {
        ch: char,
        line: usize,
        column: usize,

        #[source_code]
        src: String,
        #[label("not a valid Lox character")]
        span: SourceSpan,
    },

    #[error("unterminated string literal which started at line:{line:} col:{column:}")]
    UnterminatedStringLiteral {
        line: usize,
        column: usize,

        #[source_code]
        src: String,
        #[label("string literal began here")]
        start_span: SourceSpan,
        #[label("spans these lines")]
        lines_span: SourceSpan,
    },
}

lazy_static! {
    static ref RESERVED: HashMap<&'static str, TokenType> = {
        HashMap::from([
            ("if", TokenType::If),
            ("else", TokenType::Else),
            ("while", TokenType::While),
            ("for", TokenType::For),
            ("print", TokenType::Print),
            ("nil", TokenType::Nil),
            ("return", TokenType::Return),
            ("fun", TokenType::Fun),
            ("var", TokenType::Var),
            ("class", TokenType::Class),
            ("super", TokenType::Super),
            ("this", TokenType::This),
            ("true", TokenType::True),
            ("false", TokenType::False),
            ("and", TokenType::And),
            ("or", TokenType::Or),
        ])
    };
}

#[derive(Default, Clone, Copy, Debug, PartialEq)]
struct Position {
    byte: usize,
    line: usize,
    column: usize,
}

#[derive(Clone)]
struct SourceCodeIter<I: Iterator<Item = char>> {
    iter: Peekable<I>,
    current: Position,
}

impl<I> SourceCodeIter<I>
where
    I: Iterator<Item = char>,
{
    fn new(iter: I) -> Self {
        Self {
            iter: iter.peekable(),
            current: Position {
                byte: 0,
                line: 1,
                column: 1,
            },
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }
}

impl<I> Iterator for SourceCodeIter<I>
where
    I: Iterator<Item = char>,
{
    type Item = (char, Position);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ch) = self.iter.next() {
            let pos = self.current;
            self.current.byte += ch.len_utf8();
            self.current.column += 1;
            if ch == '\n' {
                self.current.line += 1;
                self.current.column = 1;
            }
            return Some((ch, pos));
        }
        None
    }
}

trait AsSourceCode: Iterator<Item = char> {
    fn into_source_code(self) -> SourceCodeIter<Self>
    where
        Self: Iterator<Item = char>,
        Self: Sized,
    {
        SourceCodeIter::new(self)
    }
}

impl<I> AsSourceCode for I where I: Iterator<Item = char> {}

pub fn scan(source_code: &str) -> Result<Vec<Token>, ScannerError> {
    let mut tokens = Vec::<Token>::new();

    let mut iter = source_code.chars().into_source_code();

    while let Some((ch, pos)) = iter.next() {
        match ch {
            ' ' | '\t' | '\r' | '\n' => {
                continue;
            }
            '(' => {
                tokens.push(Token {
                    token_type: TokenType::LeftParen,
                    line: pos.line,
                    column: pos.column,
                });
            }
            ')' => {
                tokens.push(Token {
                    token_type: TokenType::RightParen,
                    line: pos.line,
                    column: pos.column,
                });
            }
            '{' => {
                tokens.push(Token {
                    token_type: TokenType::LeftBrace,
                    line: pos.line,
                    column: pos.column,
                });
            }
            '}' => {
                tokens.push(Token {
                    token_type: TokenType::RightBrace,
                    line: pos.line,
                    column: pos.column,
                });
            }
            '+' => {
                tokens.push(Token {
                    token_type: TokenType::Plus,
                    line: pos.line,
                    column: pos.column,
                });
            }
            '-' => {
                tokens.push(Token {
                    token_type: TokenType::Minus,
                    line: pos.line,
                    column: pos.column,
                });
            }
            '*' => {
                tokens.push(Token {
                    token_type: TokenType::Star,
                    line: pos.line,
                    column: pos.column,
                });
            }
            ',' => {
                tokens.push(Token {
                    token_type: TokenType::Comma,
                    line: pos.line,
                    column: pos.column,
                });
            }
            ';' => {
                tokens.push(Token {
                    token_type: TokenType::Semicolon,
                    line: pos.line,
                    column: pos.column,
                });
            }

            '=' => {
                if let Some(c) = iter.peek() {
                    if *c == '=' {
                        tokens.push(Token {
                            token_type: TokenType::EqualEqual,
                            line: pos.line,
                            column: pos.column,
                        });
                        iter.next();
                        continue;
                    }
                }
                tokens.push(Token {
                    token_type: TokenType::Equal,
                    line: pos.line,
                    column: pos.column,
                });
            }
            '!' => {
                if let Some(c) = iter.peek() {
                    if *c == '=' {
                        tokens.push(Token {
                            token_type: TokenType::BangEqual,
                            line: pos.line,
                            column: pos.column,
                        });
                        iter.next();
                        continue;
                    }
                }
                tokens.push(Token {
                    token_type: TokenType::Bang,
                    line: pos.line,
                    column: pos.column,
                });
            }

            '<' => {
                if let Some(c) = iter.peek() {
                    if *c == '=' {
                        tokens.push(Token {
                            token_type: TokenType::LessThanEqual,
                            line: pos.line,
                            column: pos.column,
                        });
                        iter.next();
                        continue;
                    }
                }
                tokens.push(Token {
                    token_type: TokenType::LessThan,
                    line: pos.line,
                    column: pos.column,
                });
            }

            '>' => {
                if let Some(c) = iter.peek() {
                    if *c == '=' {
                        tokens.push(Token {
                            token_type: TokenType::GreaterThanEqual,
                            line: pos.line,
                            column: pos.column,
                        });
                        iter.next();
                        continue;
                    }
                }
                tokens.push(Token {
                    token_type: TokenType::GreaterThan,
                    line: pos.line,
                    column: pos.column,
                });
            }

            '/' => {
                if let Some(c) = iter.peek() {
                    if *c == '/' {
                        // This is a comment. Consume until EOF.
                        iter.next();
                        while let Some(c) = iter.peek() {
                            if *c != '\n' {
                                iter.next();
                                continue;
                            }
                            break;
                        }
                        continue;
                    }
                }
                tokens.push(Token {
                    token_type: TokenType::Slash,
                    line: pos.line,
                    column: pos.column,
                });
            }

            '"' => {
                let mut terminates = false;
                let mut lexeme = String::new();
                let line = pos.line;
                let column = pos.column;
                let start = pos.byte;
                while iter.peek().is_some() {
                    let (c, _) = iter.next().unwrap();
                    if c == '"' {
                        tokens.push(Token {
                            token_type: TokenType::String(lexeme),
                            line,
                            column,
                        });
                        terminates = true;
                        break;
                    } else {
                        lexeme.push(c);
                    }
                }
                if !terminates {
                    return Err(ScannerError::UnterminatedStringLiteral {
                        src: source_code.to_string(),
                        start_span: (start..start + 1).into(),
                        lines_span: (start..iter.current.byte).into(),
                        line,
                        column,
                    });
                }
            }

            ch @ '0'..='9' => {
                let mut lexeme: String = ch.into();
                let mut seen_decimal_point = false;
                let line = pos.line;
                let column = pos.column;
                while let Some(c) = iter.peek() {
                    if !c.is_ascii_digit() && *c != '.' {
                        break;
                    }
                    if *c == '.' {
                        if seen_decimal_point {
                            break;
                        }
                        seen_decimal_point = true;
                        // #TODO: Avoid need for cloning?
                        let mut peeker = iter.clone();
                        peeker.next();
                        let next_peeked = peeker.peek();
                        if next_peeked.is_none()
                            || next_peeked.is_some_and(|next| !next.is_ascii_digit())
                        {
                            break;
                        }
                    }

                    let (c, _) = iter.next().unwrap();
                    lexeme.push(c);
                }
                let num = lexeme.parse::<f64>().unwrap();
                tokens.push(Token {
                    token_type: TokenType::Number(num),
                    line,
                    column,
                });
            }

            // Look for Dot only after numbers. Maximal munch!
            '.' => {
                tokens.push(Token {
                    token_type: TokenType::Dot,
                    line: pos.line,
                    column: pos.column,
                });
            }

            ch @ ('a'..='z' | 'A'..='Z' | '_') => {
                let mut lexeme = String::from(ch);
                let line = pos.line;
                let column = pos.column;
                while let Some(c) = iter.peek() {
                    if c.is_ascii_alphanumeric() || *c == '_' {
                        lexeme.push(*c);
                        iter.next();
                        continue;
                    }
                    break;
                }
                if RESERVED.contains_key(lexeme.as_str()) {
                    tokens.push(Token {
                        token_type: RESERVED.get(lexeme.as_str()).unwrap().clone(),
                        line,
                        column,
                    })
                } else {
                    tokens.push(Token {
                        token_type: TokenType::Identifier(lexeme),
                        line,
                        column,
                    });
                }
            }

            _ => {
                return Err(ScannerError::InvalidChar {
                    ch,
                    line: pos.line,
                    column: pos.column,
                    src: source_code.to_string(),
                    span: (pos.byte, ch.len_utf8()).into(),
                });
            }
        }
    }
    tokens.push(Token {
        token_type: TokenType::Eof,
        line: iter.current.line,
        column: iter.current.column,
    });
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    use googletest::prelude::*;

    macro_rules! assert_scans_tokens {
        ( $source:expr, [$(($token_type:expr, $line:expr, $column:expr)),*$(,)?]) => {{
            let expected: Vec<Token> = vec![
                $(
                    Token{
                        token_type: $token_type,
                        line: $line,
                        column: $column
                    },
                )*
            ];

            let result = scan($source);
            expect_that!(result, ok(eq(&expected)));
        }};
    }

    #[gtest]
    fn parens_and_braces() {
        assert_scans_tokens!(
            "(  )\n( ( ) {   } } {",
            [
                (TokenType::LeftParen, 1, 1),
                (TokenType::RightParen, 1, 4),
                (TokenType::LeftParen, 2, 1),
                (TokenType::LeftParen, 2, 3),
                (TokenType::RightParen, 2, 5),
                (TokenType::LeftBrace, 2, 7),
                (TokenType::RightBrace, 2, 11),
                (TokenType::RightBrace, 2, 13),
                (TokenType::LeftBrace, 2, 15),
                (TokenType::Eof, 2, 16),
            ]
        );
    }

    #[gtest]
    fn operators() {
        assert_scans_tokens!(
            "+ ++",
            [
                (TokenType::Plus, 1, 1),
                (TokenType::Plus, 1, 3),
                (TokenType::Plus, 1, 4),
                (TokenType::Eof, 1, 5),
            ]
        );

        assert_scans_tokens!(
            "- --",
            [
                (TokenType::Minus, 1, 1),
                (TokenType::Minus, 1, 3),
                (TokenType::Minus, 1, 4),
                (TokenType::Eof, 1, 5),
            ]
        );

        assert_scans_tokens!(
            "* **",
            [
                (TokenType::Star, 1, 1),
                (TokenType::Star, 1, 3),
                (TokenType::Star, 1, 4),
                (TokenType::Eof, 1, 5),
            ]
        );

        assert_scans_tokens!(
            "= == = =",
            [
                (TokenType::Equal, 1, 1),
                (TokenType::EqualEqual, 1, 3),
                (TokenType::Equal, 1, 6),
                (TokenType::Equal, 1, 8),
                (TokenType::Eof, 1, 9),
            ]
        );

        assert_scans_tokens!(
            "! != ! = !",
            [
                (TokenType::Bang, 1, 1),
                (TokenType::BangEqual, 1, 3),
                (TokenType::Bang, 1, 6),
                (TokenType::Equal, 1, 8),
                (TokenType::Bang, 1, 10),
                (TokenType::Eof, 1, 11),
            ]
        );

        assert_scans_tokens!(
            "< <= <<",
            [
                (TokenType::LessThan, 1, 1),
                (TokenType::LessThanEqual, 1, 3),
                (TokenType::LessThan, 1, 6),
                (TokenType::LessThan, 1, 7),
                (TokenType::Eof, 1, 8),
            ]
        );

        assert_scans_tokens!(
            "> >= >>",
            [
                (TokenType::GreaterThan, 1, 1),
                (TokenType::GreaterThanEqual, 1, 3),
                (TokenType::GreaterThan, 1, 6),
                (TokenType::GreaterThan, 1, 7),
                (TokenType::Eof, 1, 8),
            ]
        );

        assert_scans_tokens!(
            ", ,,",
            [
                (TokenType::Comma, 1, 1),
                (TokenType::Comma, 1, 3),
                (TokenType::Comma, 1, 4),
                (TokenType::Eof, 1, 5),
            ]
        );

        assert_scans_tokens!(
            ". ..",
            [
                (TokenType::Dot, 1, 1),
                (TokenType::Dot, 1, 3),
                (TokenType::Dot, 1, 4),
                (TokenType::Eof, 1, 5),
            ]
        );
    }

    #[gtest]
    fn slash_and_comments() {
        assert_scans_tokens!(
            "/ /+ /-//",
            [
                (TokenType::Slash, 1, 1),
                (TokenType::Slash, 1, 3),
                (TokenType::Plus, 1, 4),
                (TokenType::Slash, 1, 6),
                (TokenType::Minus, 1, 7),
                (TokenType::Eof, 1, 10),
            ]
        );
        assert_scans_tokens!("// This is some comment", [(TokenType::Eof, 1, 24),]);
        assert_scans_tokens!("// This is some comment\n", [(TokenType::Eof, 2, 1),]);

        assert_scans_tokens!(
            "// This is some comment\nvar // Inline comment.",
            [(TokenType::Var, 2, 1), (TokenType::Eof, 2, 23),]
        );
    }

    #[gtest]
    fn every_keyword() {
        assert_scans_tokens!("if", [(TokenType::If, 1, 1), (TokenType::Eof, 1, 3)]);
        assert_scans_tokens!("else", [(TokenType::Else, 1, 1), (TokenType::Eof, 1, 5)]);
        assert_scans_tokens!("while", [(TokenType::While, 1, 1), (TokenType::Eof, 1, 6)]);
        assert_scans_tokens!("for", [(TokenType::For, 1, 1), (TokenType::Eof, 1, 4)]);
        assert_scans_tokens!("print", [(TokenType::Print, 1, 1), (TokenType::Eof, 1, 6)]);
        assert_scans_tokens!("nil", [(TokenType::Nil, 1, 1), (TokenType::Eof, 1, 4)]);
        assert_scans_tokens!(
            "return",
            [(TokenType::Return, 1, 1), (TokenType::Eof, 1, 7)]
        );
        assert_scans_tokens!("fun", [(TokenType::Fun, 1, 1), (TokenType::Eof, 1, 4)]);
        assert_scans_tokens!("var", [(TokenType::Var, 1, 1), (TokenType::Eof, 1, 4)]);
        assert_scans_tokens!("class", [(TokenType::Class, 1, 1), (TokenType::Eof, 1, 6)]);
        assert_scans_tokens!("super", [(TokenType::Super, 1, 1), (TokenType::Eof, 1, 6)]);
        assert_scans_tokens!("this", [(TokenType::This, 1, 1), (TokenType::Eof, 1, 5)]);
        assert_scans_tokens!("true", [(TokenType::True, 1, 1), (TokenType::Eof, 1, 5)]);
        assert_scans_tokens!("false", [(TokenType::False, 1, 1), (TokenType::Eof, 1, 6)]);
        assert_scans_tokens!("and", [(TokenType::And, 1, 1), (TokenType::Eof, 1, 4)]);
        assert_scans_tokens!("or", [(TokenType::Or, 1, 1), (TokenType::Eof, 1, 3)]);

        assert_scans_tokens!(
            "if else while true false return this print var for super class",
            [
                (TokenType::If, 1, 1),
                (TokenType::Else, 1, 4),
                (TokenType::While, 1, 9),
                (TokenType::True, 1, 15),
                (TokenType::False, 1, 20),
                (TokenType::Return, 1, 26),
                (TokenType::This, 1, 33),
                (TokenType::Print, 1, 38),
                (TokenType::Var, 1, 44),
                (TokenType::For, 1, 48),
                (TokenType::Super, 1, 52),
                (TokenType::Class, 1, 58),
                (TokenType::Eof, 1, 63)
            ]
        );
    }

    #[gtest]
    fn whitespace() {
        assert_scans_tokens!(
            "var\n\tvar\n var\r\nvar",
            [
                (TokenType::Var, 1, 1),
                (TokenType::Var, 2, 2),
                (TokenType::Var, 3, 2),
                (TokenType::Var, 4, 1),
                (TokenType::Eof, 4, 4),
            ]
        );
    }

    #[gtest]
    fn semicolon() {
        assert_scans_tokens!(
            "; ;;;",
            [
                (TokenType::Semicolon, 1, 1),
                (TokenType::Semicolon, 1, 3),
                (TokenType::Semicolon, 1, 4),
                (TokenType::Semicolon, 1, 5),
                (TokenType::Eof, 1, 6),
            ]
        );
    }

    #[gtest]
    fn identifiers() {
        assert_scans_tokens!(
            "some_function",
            [
                (TokenType::Identifier(String::from("some_function")), 1, 1),
                (TokenType::Eof, 1, 14)
            ]
        );
        assert_scans_tokens!(
            "fun some_function(",
            [
                (TokenType::Fun, 1, 1),
                (TokenType::Identifier(String::from("some_function")), 1, 5),
                (TokenType::LeftParen, 1, 18),
                (TokenType::Eof, 1, 19)
            ]
        );

        assert_scans_tokens!(
            ", . this.variable_",
            [
                (TokenType::Comma, 1, 1),
                (TokenType::Dot, 1, 3),
                (TokenType::This, 1, 5),
                (TokenType::Dot, 1, 9),
                (TokenType::Identifier(String::from("variable_")), 1, 10),
                (TokenType::Eof, 1, 19),
            ]
        );
    }

    #[gtest]
    fn string_literals() {
        assert_scans_tokens!(
            r#"var a = "This is a literal string""#,
            [
                (TokenType::Var, 1, 1),
                (TokenType::Identifier(String::from("a")), 1, 5),
                (TokenType::Equal, 1, 7),
                (
                    TokenType::String(String::from("This is a literal string")),
                    1,
                    9
                ),
                (TokenType::Eof, 1, 35),
            ]
        );

        assert_scans_tokens!(
            r#""This is a
multiline string""#,
            [
                (
                    TokenType::String(String::from("This is a\nmultiline string")),
                    1,
                    1
                ),
                (TokenType::Eof, 2, 18),
            ]
        );
    }

    #[gtest]
    fn invalid_identifiers() {
        let s = "hello    world\n   it's a new day!";
        expect_that!(
            scan(s),
            err(eq(&ScannerError::InvalidChar {
                ch: '\'',
                line: 2,
                column: 6,
                src: String::from(s),
                span: (20, 1).into(),
            }))
        );
        let s = "hello$  lox";
        expect_that!(
            scan(s),
            err(eq(&ScannerError::InvalidChar {
                ch: '$',
                line: 1,
                column: 6,
                src: String::from(s),
                span: (5, 1).into(),
            }))
        );

        let s = "hel_lo$  lox";
        expect_that!(
            scan(s),
            err(eq(&ScannerError::InvalidChar {
                ch: '$',
                line: 1,
                column: 7,
                src: String::from(s),
                span: (6, 1).into(),
            }))
        );

        let s = "hel _lo$  lox";
        expect_that!(
            scan(s),
            err(eq(&ScannerError::InvalidChar {
                ch: '$',
                line: 1,
                column: 8,
                src: String::from(s),
                span: (7, 1).into(),
            }))
        );

        let s = "hel\n_lo$  lox";
        expect_that!(
            scan(s),
            err(eq(&ScannerError::InvalidChar {
                ch: '$',
                line: 2,
                column: 4,
                src: String::from(s),
                span: (7, 1).into(),
            }))
        );

        let s = "_lo$  lox";
        expect_that!(
            scan(s),
            err(eq(&ScannerError::InvalidChar {
                ch: '$',
                line: 1,
                column: 4,
                src: String::from(s),
                span: (3, 1).into(),
            }))
        );
    }

    #[gtest]
    fn unterminated_string() {
        let s = r#""Unterminated!!"#;
        expect_that!(
            scan(s),
            err(eq(&ScannerError::UnterminatedStringLiteral {
                line: 1,
                column: 1,
                src: String::from(s),
                start_span: (0, 1).into(),
                lines_span: (0, 15).into(),
            }))
        );

        let s = r#""Multiline
            unterminated"#;
        expect_that!(
            scan(s),
            err(eq(&ScannerError::UnterminatedStringLiteral {
                line: 1,
                column: 1,
                src: String::from(s),
                start_span: (0, 1).into(),
                lines_span: (0, 35).into(),
            }))
        );
    }

    #[gtest]
    fn number_literals() {
        assert_scans_tokens!(
            "123",
            [(TokenType::Number(123.0), 1, 1), (TokenType::Eof, 1, 4),]
        );

        assert_scans_tokens!(
            "123.",
            [
                (TokenType::Number(123.0), 1, 1),
                (TokenType::Dot, 1, 4),
                (TokenType::Eof, 1, 5),
            ]
        );

        assert_scans_tokens!(
            ".123",
            [
                (TokenType::Dot, 1, 1),
                (TokenType::Number(123.0), 1, 2),
                (TokenType::Eof, 1, 5),
            ]
        );

        assert_scans_tokens!(
            ".123.",
            [
                (TokenType::Dot, 1, 1),
                (TokenType::Number(123.0), 1, 2),
                (TokenType::Dot, 1, 5),
                (TokenType::Eof, 1, 6),
            ]
        );

        assert_scans_tokens!(
            ".123. 0",
            [
                (TokenType::Dot, 1, 1),
                (TokenType::Number(123.0), 1, 2),
                (TokenType::Dot, 1, 5),
                (TokenType::Number(0.0), 1, 7),
                (TokenType::Eof, 1, 8),
            ]
        );

        assert_scans_tokens!(
            ".123.0",
            [
                (TokenType::Dot, 1, 1),
                (TokenType::Number(123.0), 1, 2),
                (TokenType::Eof, 1, 7),
            ]
        );

        assert_scans_tokens!(
            ".123.0.1",
            [
                (TokenType::Dot, 1, 1),
                (TokenType::Number(123.0), 1, 2),
                (TokenType::Dot, 1, 7),
                (TokenType::Number(1.0), 1, 8),
                (TokenType::Eof, 1, 9),
            ]
        );

        assert_scans_tokens!(
            "123\n456",
            [
                (TokenType::Number(123.0), 1, 1),
                (TokenType::Number(456.0), 2, 1),
                (TokenType::Eof, 2, 4),
            ]
        );

        assert_scans_tokens!(
            "123.0",
            [(TokenType::Number(123.0), 1, 1), (TokenType::Eof, 1, 6),]
        );
        assert_scans_tokens!(
            "1.23",
            [(TokenType::Number(1.23), 1, 1), (TokenType::Eof, 1, 5),]
        );
    }
}
