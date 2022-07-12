use phf::phf_map;
use std::iter::{Enumerate, Peekable};
use std::str::Chars;
use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum ScannerError {
    #[error("unexpected character '{invalid_character:}' while parsing identifier at line {line:}, column {column:}")]
    InvalidIdentifier {
        invalid_character: char,
        line: usize,
        column: usize,
    },
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

    Word(String),
}

static RESERVED: phf::Map<&'static str, TokenType> = phf_map! {
    "if" => TokenType::If,
    "else" => TokenType::Else,
    "while" => TokenType::While,
    "for" => TokenType::For,
    "print" => TokenType::Print,
    "nil" => TokenType::Nil,
    "return" => TokenType::Return,
    "fun" => TokenType::Fun,
    "var" => TokenType::Var,
    "class" => TokenType::Class,
    "super" => TokenType::Super,
    "this" => TokenType::This,
    "true" => TokenType::True,
    "false" => TokenType::False,
    "and" => TokenType::And,
    "or" => TokenType::Or,
};

#[derive(Debug, PartialEq)]
pub struct Token {
    token_type: TokenType,
    line: usize,
    column: usize,
}

pub fn scan(source_code: &str) -> Result<Vec<Token>, ScannerError> {
    let mut scanner = Scanner::new(&source_code);
    scanner.scan()?;
    Ok(scanner.tokens)
}

struct Scanner<'a> {
    source_code: &'a str,
    current_line: usize,
    current_column: usize,
    tokens: Vec<Token>,
}

impl<'a> Scanner<'a> {
    fn new(source_code: &'a str) -> Self {
        Scanner {
            source_code: source_code,
            current_line: 1,
            current_column: 0,
            tokens: Vec::new(),
        }
    }

    fn scan(&mut self) -> Result<(), ScannerError> {
        let mut iter = self.source_code.chars().enumerate().peekable();

        loop {
            if iter.peek() == None {
                break;
            }

            let (start_pos, ch) = iter.next().expect("Unexpected EOF");
            self.current_column += 1;
            let start_column = self.current_column;

            match ch {
                ' ' => continue,
                '\n' => {
                    self.current_line += 1;
                    self.current_column = 0;
                }

                '(' => self.add_token(TokenType::LeftParen, start_column),
                ')' => self.add_token(TokenType::RightParen, start_column),
                '{' => self.add_token(TokenType::LeftBrace, start_column),
                '}' => self.add_token(TokenType::RightBrace, start_column),

                '+' => self.add_token(TokenType::Plus, start_column),
                '-' => self.add_token(TokenType::Minus, start_column),
                '/' => self.add_token(TokenType::Slash, start_column),
                '*' => self.add_token(TokenType::Star, start_column),
                ',' => self.add_token(TokenType::Comma, start_column),
                '.' => self.add_token(TokenType::Dot, start_column),

                '=' => self.parse_one_or_two_char_operator(
                    &mut iter,
                    start_column,
                    TokenType::Equal,
                    '=',
                    TokenType::EqualEqual,
                ),

                '<' => self.parse_one_or_two_char_operator(
                    &mut iter,
                    start_column,
                    TokenType::LessThan,
                    '=',
                    TokenType::LessThanEqual,
                ),

                '>' => self.parse_one_or_two_char_operator(
                    &mut iter,
                    start_column,
                    TokenType::GreaterThan,
                    '=',
                    TokenType::GreaterThanEqual,
                ),

                '!' => self.parse_one_or_two_char_operator(
                    &mut iter,
                    start_column,
                    TokenType::Bang,
                    '=',
                    TokenType::BangEqual,
                ),

                start_char if start_char.is_alphabetic() => {
                    let mut current_pos: usize = start_pos;
                    while let Some((_, next_char)) = iter.peek() {
                        if !next_char.is_alphanumeric() && *next_char != '_' {
                            break;
                        }
                        (current_pos, _) = iter.next().unwrap();
                        self.current_column += 1;
                    }

                    let token = &self.source_code[start_pos..=current_pos];

                    if RESERVED.contains_key(token) {
                        self.add_token(RESERVED.get(token).unwrap().clone(), start_column);
                    } else {
                        self.add_token(
                            TokenType::Identifier(String::from(
                                &self.source_code[start_pos..=current_pos],
                            )),
                            start_column,
                        );
                    }
                }

                _ => {
                    return Err(ScannerError::InvalidIdentifier {
                        invalid_character: ch,
                        line: self.current_line,
                        column: self.current_column,
                    })
                }
            }
        }

        self.add_token(TokenType::Eof, self.current_column + 1);

        // println!("Got tokens:\n{:?}", self.tokens);
        Ok(())
    }

    fn add_token(&mut self, token_type: TokenType, column: usize) {
        self.tokens.push(Token {
            token_type,
            line: self.current_line,
            column: column,
        })
    }

    fn parse_one_or_two_char_operator(
        &mut self,
        iter: &mut Peekable<Enumerate<Chars>>,
        start_column: usize,
        first_token_type: TokenType,
        second_char: char,
        second_token_type: TokenType,
    ) {
        if let Some((_, next_char)) = iter.peek() {
            if *next_char == second_char {
                iter.next();
                self.current_column += 1;
                self.add_token(second_token_type, start_column);
            } else {
                self.add_token(first_token_type, start_column)
            }
        } else {
            self.add_token(first_token_type, start_column)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_scans_tokens {
        ( $source:expr, [$(($token_type:expr, $line:expr, $column:expr)),*$(,)?]) => {{
            let mut expected: Vec<Token> = Vec::new();
            $(
                expected.push(Token{
                    token_type: $token_type,
                    line: $line,
                    column: $column

                });
            )*

            let result = scan($source);
            assert!(result.is_ok(), "\nExpected OK scanning:\n'''\n{}\n'''\nInstead got error: {:?}", $source, result.unwrap_err());
            let got = result.unwrap();

            // // Skip Eof when comparing.
            // let got = &got[..got.len() - 1];

            assert_eq!(got, expected, "\nWhile scanning:\n'''\n{}\n'''\nExpected:\n{:?}\nInstead got\n: {:?}", $source, expected, got);
        }};
    }

    #[test]
    fn it_scans_parens_and_braces() {
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

    #[test]
    fn it_scans_operators() {
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
            "/ //",
            [
                (TokenType::Slash, 1, 1),
                (TokenType::Slash, 1, 3),
                (TokenType::Slash, 1, 4),
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

    #[test]
    fn it_scans_every_keyword() {
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
    }

    #[test]
    fn it_scans_identifiers() {
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

    fn assert_invalid_character(
        source: &str,
        invalid_char: char,
        at_line: usize,
        at_column: usize,
    ) {
        let mut scanner = Scanner::new(source);

        assert_eq!(
            scanner.scan().unwrap_err(),
            ScannerError::InvalidIdentifier {
                invalid_character: invalid_char,
                line: at_line,
                column: at_column,
            }
        );
    }

    #[test]
    fn invalid_identifiers() {
        assert_invalid_character("hello    world\n   it's a new day!", '\'', 2, 6);
        assert_invalid_character("hello$  lox", '$', 1, 6);
        assert_invalid_character("hel_lo$  lox", '$', 1, 7);
        assert_invalid_character("hel _lo$  lox", '_', 1, 5);
        assert_invalid_character("hel\n_lo$  lox", '_', 2, 1);
        assert_invalid_character("_lo$  lox", '_', 1, 1);
    }

    #[test]
    fn alphabetic() {
        assert!(!'!'.is_alphabetic());
        assert!(!'\''.is_alphabetic());
        assert!(!'\n'.is_alphabetic());
    }
}
