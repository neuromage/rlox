use super::{
    expr::{Expr, Operator, Value},
    tokens::{Token, TokenType},
};
use thiserror::Error;

#[derive(Debug)]
struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

#[derive(Debug, Error, PartialEq)]
pub enum ParseError {
    #[error("expected closing `)`, instead found {unexpected:}")]
    UnterminatedGrouping { unexpected: Token },
    #[error("expected expression, instead found {unexpected:}")]
    NotAnExpression { unexpected: Token },
}

type ParseResult = Result<Box<Expr>, ParseError>;

pub fn parse(tokens: Vec<Token>) -> ParseResult {
    let mut parser = Parser::new(tokens);
    parser.expression()
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    fn expression(&mut self) -> ParseResult {
        self.equality()
    }

    fn equality(&mut self) -> ParseResult {
        let mut expr = self.comparison()?;

        while self.matches(&vec![TokenType::EqualEqual, TokenType::BangEqual]) {
            let operator = Operator::from(&self.tokens[self.current].token_type);
            self.current += 1;

            expr = Box::new(Expr::Binary {
                left: expr,
                right: self.comparison()?,
                operator: operator,
            })
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> ParseResult {
        let mut expr = self.term()?;

        while self.matches(&vec![
            TokenType::LessThan,
            TokenType::LessThanEqual,
            TokenType::GreaterThan,
            TokenType::GreaterThanEqual,
        ]) {
            let operator = Operator::from(&self.tokens[self.current].token_type);
            self.current += 1;

            expr = Box::new(Expr::Binary {
                left: expr,
                right: self.term()?,
                operator: operator,
            })
        }
        Ok(expr)
    }

    fn term(&mut self) -> ParseResult {
        let mut expr = self.factor()?;

        while self.matches(&vec![TokenType::Plus, TokenType::Minus]) {
            let operator = Operator::from(&self.tokens[self.current].token_type);
            self.current += 1;

            expr = Box::new(Expr::Binary {
                left: expr,
                right: self.factor()?,
                operator: operator,
            })
        }
        Ok(expr)
    }

    fn factor(&mut self) -> ParseResult {
        let mut expr = self.unary()?;

        while self.matches(&vec![TokenType::Star, TokenType::Slash]) {
            let operator = Operator::from(&self.tokens[self.current].token_type);
            self.current += 1;

            expr = Box::new(Expr::Binary {
                left: expr,
                right: self.unary()?,
                operator: operator,
            })
        }
        Ok(expr)
    }

    fn unary(&mut self) -> ParseResult {
        if self.matches(&vec![TokenType::Bang, TokenType::Minus]) {
            let operator = Operator::from(&self.tokens[self.current].token_type);
            self.current += 1;

            let expr = self.unary()?;

            Ok(Box::new(Expr::Unary { expr, operator }))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> ParseResult {
        let expr = match &self.tokens[self.current].token_type {
            TokenType::False => {
                self.current += 1;
                Box::new(Expr::Literal(Value::Bool(false)))
            }
            TokenType::True => {
                self.current += 1;
                Box::new(Expr::Literal(Value::Bool(true)))
            }
            TokenType::Nil => {
                self.current += 1;
                Box::new(Expr::Literal(Value::Bool(true)))
            }
            TokenType::String(v) => {
                self.current += 1;
                Box::new(Expr::Literal(Value::String(v.clone())))
            }
            TokenType::Float(v) => {
                self.current += 1;
                Box::new(Expr::Literal(Value::Float(*v)))
            }
            TokenType::Int(v) => {
                self.current += 1;
                Box::new(Expr::Literal(Value::Int(*v)))
            }
            TokenType::LeftParen => {
                self.current += 1;
                let inner_expr = self.expression()?;
                if !self.match_and_consume(TokenType::RightParen) {
                    return Err(ParseError::UnterminatedGrouping {
                        unexpected: self.current_token(),
                    });
                }
                Box::new(Expr::Grouping(inner_expr))
            }
            _ => {
                return Err(ParseError::NotAnExpression {
                    unexpected: self.current_token(),
                })
            }
        };
        Ok(expr)
    }

    fn match_and_consume(&mut self, token_type: TokenType) -> bool {
        if self.tokens[self.current].token_type == token_type {
            self.current += 1;
            return true;
        }
        false
    }

    fn current_token(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn matches(&self, token_types: &[TokenType]) -> bool {
        let current_tt = &self.tokens[self.current].token_type;
        for tt in token_types {
            if current_tt == tt {
                return true;
            }
        }
        false
    }

    fn try_consume(&mut self, token_type: TokenType) -> bool {
        if self.tokens[self.current].token_type == token_type {
            self.current += 1;
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_parses_tokens {
        ( [$($token_type:expr),*$(,)?], $expected:expr ) => {
            let mut tokens: Vec<Token> = Vec::new();
            $(
                tokens.push(Token{token_type: $token_type, line: 0, column: 0});
            )*

            let result = parse(tokens.clone());
            assert!(result.is_ok(), "\nExpected OK parsing tokens:\n'''\n{:?}\n'''\nInstead got error: {:?}", tokens, result.unwrap_err());
            let got = format!("{}", result.unwrap());

            assert_eq!(got, $expected, "\nWhile parsing:\n'''\n{:?}\n'''\nExpected:\n{:?}\nInstead got:\n{:?}", tokens, $expected, got);


        };
    }

    macro_rules! assert_parse_error {
        ( [$($token_type:expr),*$(,)?], $expected_err:expr) => {{
            let mut tokens: Vec<Token> = Vec::new();
            $(
                tokens.push(Token{token_type: $token_type, line: 0, column: 0});
            )*

            let result = parse(tokens.clone());
            assert!(
                result.is_err(),
                "\nExpected error parsing tokens:\n'''\n{:?}\n'''\nInstead got OK Result containing:\n{:?}",
                tokens,
                result.unwrap()
            );

            let got_err = result.unwrap_err();
            assert_eq!(
                got_err, $expected_err,
                "\nWhile parsing:\n'''\n{:?}\n'''\nExpected error:\n{:?}\nInstead got error:\n{:?}",
                tokens, $expected_err, got_err
            );
        }};
    }

    #[test]
    fn parses_tokens() {
        assert_parses_tokens!(
            [
                TokenType::Int(10),
                TokenType::Plus,
                TokenType::Int(10),
                TokenType::Eof,
            ],
            "(+ 10 10)"
        );

        assert_parses_tokens!(
            [
                TokenType::LeftParen,
                TokenType::Int(10),
                TokenType::Plus,
                TokenType::Int(20),
                TokenType::RightParen,
                TokenType::Star,
                TokenType::Int(30),
                TokenType::Eof,
            ],
            "(* (group (+ 10 20)) 30)"
        );
    }

    #[test]
    fn empty_tokens() {
        assert_parse_error!(
            [TokenType::Eof],
            ParseError::NotAnExpression {
                unexpected: Token {
                    token_type: TokenType::Eof,
                    line: 0,
                    column: 0
                }
            }
        );
    }
}
