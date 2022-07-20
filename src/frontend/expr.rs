use std::fmt;

use super::tokens::TokenType;

// use super::tokens::{Token, TokenType};

#[derive(Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Bang,
    EqualEqual,
    BangEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

impl Operator {
    pub fn from(token_type: &TokenType) -> Self {
        match token_type {
            TokenType::Plus => Operator::Plus,
            TokenType::Minus => Operator::Minus,
            TokenType::Star => Operator::Multiply,
            TokenType::Slash => Operator::Divide,
            TokenType::Bang => Operator::Bang,
            TokenType::BangEqual => Operator::BangEqual,
            TokenType::LessThan => Operator::LessThan,
            TokenType::LessThanEqual => Operator::LessThanEqual,
            TokenType::GreaterThan => Operator::GreaterThan,
            TokenType::GreaterThanEqual => Operator::GreaterThanEqual,
            TokenType::EqualEqual => Operator::EqualEqual,
            _ => panic!("cannot convert non-operator type token {:?}", token_type),
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Multiply => write!(f, "*"),
            Operator::Divide => write!(f, "/"),
            Operator::Bang => write!(f, "!"),
            Operator::BangEqual => write!(f, "!="),
            Operator::EqualEqual => write!(f, "=="),
            Operator::LessThan => write!(f, "<"),
            Operator::LessThanEqual => write!(f, "<="),
            Operator::GreaterThan => write!(f, ">"),
            Operator::GreaterThanEqual => write!(f, ">="),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        operator: Operator,
    },
    Unary {
        operator: Operator,
        expr: Box<Expr>,
    },
    Grouping(Box<Expr>),
    Literal(Value),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary {
                left,
                right,
                operator,
            } => {
                write!(f, "({} {} {})", operator, left, right)
            }
            Expr::Literal(v) => write!(f, "{}", v),
            Expr::Grouping(exp) => write!(f, "(group {})", exp),
            Expr::Unary { operator, expr } => write!(f, "({} {})", operator, expr),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prints_binary() {
        let bin = Expr::Binary {
            left: Box::new(Expr::Literal(Value::Int(10))),
            right: Box::new(Expr::Literal(Value::Int(10))),
            operator: Operator::Plus,
        };
        assert_eq!(format! {"{}", bin}, "(+ 10 10)");

        let bin = Expr::Binary {
            left: Box::new(Expr::Literal(Value::Int(10))),
            right: Box::new(Expr::Literal(Value::Float(10.1))),
            operator: Operator::Minus,
        };
        assert_eq!(format! {"{}", bin}, "(- 10 10.1)");

        let bin = Expr::Binary {
            left: Box::new(Expr::Literal(Value::Int(10))),
            right: Box::new(Expr::Literal(Value::Float(10.1))),
            operator: Operator::Multiply,
        };
        assert_eq!(format! {"{}", bin}, "(* 10 10.1)");

        let bin = Expr::Binary {
            left: Box::new(Expr::Literal(Value::Int(10))),
            right: Box::new(Expr::Literal(Value::Float(10.1))),
            operator: Operator::Divide,
        };
        assert_eq!(format! {"{}", bin}, "(/ 10 10.1)");
    }

    #[test]
    fn prints_grouping() {
        let grp = Expr::Grouping(Box::new(Expr::Binary {
            left: Box::new(Expr::Grouping(Box::new(Expr::Binary {
                left: Box::new(Expr::Literal(Value::Int(10))),
                right: Box::new(Expr::Literal(Value::Float(10.1))),
                operator: Operator::Plus,
            }))),
            right: Box::new(Expr::Literal(Value::Int(20))),
            operator: Operator::Multiply,
        }));
        assert_eq!(format! {"{}", grp}, "(group (* (group (+ 10 10.1)) 20))");
    }

    #[test]
    fn prints_unary() {
        let unary = Expr::Unary {
            expr: Box::new(Expr::Literal(Value::Int(10))),
            operator: Operator::Minus,
        };
        assert_eq!(format! {"{}", unary}, "(- 10)");

        let unary = Expr::Unary {
            expr: Box::new(Expr::Literal(Value::Bool(false))),
            operator: Operator::Bang,
        };
        assert_eq!(format! {"{}", unary}, "(! false)");
    }

    #[test]
    fn prints_example() {
        let ex = Expr::Binary {
            left: Box::new(Expr::Unary {
                operator: Operator::Minus,
                expr: Box::new(Expr::Literal(Value::Int(123))),
            }),
            right: Box::new(Expr::Grouping(Box::new(Expr::Literal(Value::Float(45.67))))),
            operator: Operator::Multiply,
        };

        assert_eq!(format! {"{}", ex}, "(* (- 123) (group 45.67))");
    }
}
