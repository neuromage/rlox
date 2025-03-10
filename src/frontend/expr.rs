// use core::panic;
// use std::fmt;
// use thiserror::Error;

// use super::tokens::TokenType;

// #[derive(Debug, Error, PartialEq)]
// pub enum EvaluationError {
//     #[error("cannot apply operator `{operator:}` to non-number operand `{value:}`")]
//     NonNumberOperand { value: Value, operator: Operator },

//     #[error("not a float: `{value:}`")]
//     NotAFloat { value: Value },
// }

// #[derive(Debug, Error, PartialEq)]
// pub enum ValueError {
//     #[error("non-number `{value:}` (not an integer or a float)")]
//     NotANumber { value: Value },

//     #[error("non-integer `{value:}`")]
//     NotAnInteger { value: Value },

//     #[error("cannot apply operator `{operator:}` to `{value:}`: {reason:}")]
//     InvalidOperation {
//         value: Value,
//         operator: Operator,
//         reason: String,
//     },
// }

// #[derive(Debug, Clone, PartialEq)]
// pub enum Value {
//     Number(f64),
//     String(String),
//     Bool(bool),
//     Nil,
// }

// // use std::ops;

// // impl ops::Add<Value> for Value {
// //     type Output = EvaluationResult;

// //     fn add(self, rhs: Value) -> Self::Output {
// //         Value::check_number_operands(&self, &rhs, Operator::Plus)?;

// //         if self.is_float() || rhs.is_float() {
// //             Ok(Value::Float(
// //                 self.unwrap_number_as_float() + rhs.unwrap_number_as_float(),
// //             ))
// //         } else {
// //             Ok(Value::Int(self.unwrap_int() + rhs.unwrap_int()))
// //         }
// //     }
// // }

// // impl Value {
// //     fn apply_number_operator<F, T>(
// //         lhs: &Value,
// //         rhs: &Value,
// //         operator: &Operator,
// //     ) -> Result<Value, ValueError> {
// //         if lhs.is_float() || rhs.is_float() {
// //             let l = lhs.unwrap_as_float()?;
// //             let r = rhs.unwrap_as_float()?;

// //             Ok(Value::Float(operator_func(l, r)))
// //         } else {
// //             let l = lhs.unwrap_int()?;
// //             let r = rhs.unwrap_int()?;
// //             Ok(Value::Int(operator_func(l, r)))
// //         }
// //     }

// //     fn unwrap_number(&self) -> Result<f64, ValueError> {
// //         match self {
// //             Value::Number(v) => Ok(*v),
// //             _ => Err(ValueError::NotANumber {
// //                 value: self.clone(),
// //             }),
// //         }
// //     }

// //     // fn evaluate(&self, op: &Operator, other: &Value) -> EvaluationResult {
// //     //     match op {
// //     //         Operator::Plus | Operator::Minus | Operator::Multiply | Operator::Divide => {
// //     //             if !self.check_is_number() {
// //     //                 return Err(EvaluationError::NonNumberOperand {
// //     //                     value: self.clone(),
// //     //                     operator: op.clone(),
// //     //                 });
// //     //             }
// //     //             if !other.check_is_number() {
// //     //                 return Err(EvaluationError::NonNumberOperand {
// //     //                     value: other.clone(),
// //     //                     operator: op.clone(),
// //     //                 });
// //     //             }
// //     //         }
// //     //     }

// //     //     match op {
// //     //         Operator::Multiply => match (self, other) {
// //     //             (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left * right)),
// //     //             (Value::Float(left), Value::Int(right)) => Ok(Value::Float(left * *right as f64)),
// //     //             (Value::Int(left), Value::Float(right)) => Ok(Value::Float(*left as f64 * right)),
// //     //             (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left * right)),
// //     //             _ => Err(EvaluationError::NonNumberOperands {
// //     //                 left: self.clone(),
// //     //                 right: other.clone(),
// //     //                 operator: op.clone(),
// //     //             }),
// //     //         },
// //     //         _ => Err(EvaluationError::NonNumberOperands {
// //     //             left: self.clone(),
// //     //             right: other.clone(),
// //     //             operator: op.clone(),
// //     //         }),
// //     //     }
// //     // }
// // }

// impl fmt::Display for Value {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Value::Number(v) => write!(f, "{}", v),
//             Value::String(v) => write!(f, "{}", v),
//             Value::Bool(v) => write!(f, "{}", v),
//             Value::Nil => write!(f, "nil"),
//         }
//     }
// }

// #[derive(Debug, PartialEq, Clone)]
// pub enum Operator {
//     Plus,
//     Minus,
//     Multiply,
//     Divide,
//     Bang,
//     EqualEqual,
//     BangEqual,
//     LessThan,
//     LessThanEqual,
//     GreaterThan,
//     GreaterThanEqual,
// }

// impl Operator {
//     pub fn from(token_type: &TokenType) -> Self {
//         match token_type {
//             TokenType::Plus => Operator::Plus,
//             TokenType::Minus => Operator::Minus,
//             TokenType::Star => Operator::Multiply,
//             TokenType::Slash => Operator::Divide,
//             TokenType::Bang => Operator::Bang,
//             TokenType::BangEqual => Operator::BangEqual,
//             TokenType::LessThan => Operator::LessThan,
//             TokenType::LessThanEqual => Operator::LessThanEqual,
//             TokenType::GreaterThan => Operator::GreaterThan,
//             TokenType::GreaterThanEqual => Operator::GreaterThanEqual,
//             TokenType::EqualEqual => Operator::EqualEqual,
//             _ => panic!("cannot convert non-operator type token {:?}", token_type),
//         }
//     }
// }

// impl fmt::Display for Operator {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Operator::Plus => write!(f, "+"),
//             Operator::Minus => write!(f, "-"),
//             Operator::Multiply => write!(f, "*"),
//             Operator::Divide => write!(f, "/"),
//             Operator::Bang => write!(f, "!"),
//             Operator::BangEqual => write!(f, "!="),
//             Operator::EqualEqual => write!(f, "=="),
//             Operator::LessThan => write!(f, "<"),
//             Operator::LessThanEqual => write!(f, "<="),
//             Operator::GreaterThan => write!(f, ">"),
//             Operator::GreaterThanEqual => write!(f, ">="),
//         }
//     }
// }

// #[derive(Debug)]
// pub enum Expr {
//     Binary {
//         left: Box<Expr>,
//         right: Box<Expr>,
//         operator: Operator,
//     },
//     Unary {
//         operator: Operator,
//         expr: Box<Expr>,
//     },
//     Grouping(Box<Expr>),
//     Literal(Value),
// }

// type EvaluationResult = Result<Value, EvaluationError>;

// // impl Expr {
// //     fn evaluate(&self) -> EvaluationResult {
// //         match self {
// //             Expr::Literal(v) => Ok(v.clone()),
// //             Expr::Grouping(expr) => expr.evaluate(),
// //             Expr::Unary { operator, expr } => {
// //                 let value = expr.evaluate()?;
// //                 match operator {
// //                     Operator::Minus => value.evaluate(&Operator::Multiply, &Value::Int(-1)),
// //                     Operator::Bang => value.evaluate(&Operator::Multiply, &Value::Int(-1)),
// //                 }
// //             }
// //         }
// //     }
// // }

// impl fmt::Display for Expr {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Expr::Binary {
//                 left,
//                 right,
//                 operator,
//             } => {
//                 write!(f, "({} {} {})", operator, left, right)
//             }
//             Expr::Literal(v) => write!(f, "{}", v),
//             Expr::Grouping(exp) => write!(f, "(group {})", exp),
//             Expr::Unary { operator, expr } => write!(f, "({} {})", operator, expr),
//         }
//     }
// }

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn prints_binary() {
//         let bin = Expr::Binary {
//             left: Box::new(Expr::Literal(Value::Number(10.0))),
//             right: Box::new(Expr::Literal(Value::Number(10.0))),
//             operator: Operator::Plus,
//         };
//         assert_eq!(format! {"{}", bin}, "(+ 10 10)");

//         let bin = Expr::Binary {
//             left: Box::new(Expr::Literal(Value::Number(10.0))),
//             right: Box::new(Expr::Literal(Value::Number(10.1))),
//             operator: Operator::Minus,
//         };
//         assert_eq!(format! {"{}", bin}, "(- 10 10.1)");

//         let bin = Expr::Binary {
//             left: Box::new(Expr::Literal(Value::Number(10.0))),
//             right: Box::new(Expr::Literal(Value::Number(10.1))),
//             operator: Operator::Multiply,
//         };
//         assert_eq!(format! {"{}", bin}, "(* 10 10.1)");

//         let bin = Expr::Binary {
//             left: Box::new(Expr::Literal(Value::Number(10.0))),
//             right: Box::new(Expr::Literal(Value::Number(10.1))),
//             operator: Operator::Divide,
//         };
//         assert_eq!(format! {"{}", bin}, "(/ 10 10.1)");
//     }

//     #[test]
//     fn prints_grouping() {
//         let grp = Expr::Grouping(Box::new(Expr::Binary {
//             left: Box::new(Expr::Grouping(Box::new(Expr::Binary {
//                 left: Box::new(Expr::Literal(Value::Number(10.0))),
//                 right: Box::new(Expr::Literal(Value::Number(10.1))),
//                 operator: Operator::Plus,
//             }))),
//             right: Box::new(Expr::Literal(Value::Number(20.0))),
//             operator: Operator::Multiply,
//         }));
//         assert_eq!(format! {"{}", grp}, "(group (* (group (+ 10 10.1)) 20))");
//     }

//     #[test]
//     fn prints_unary() {
//         let unary = Expr::Unary {
//             expr: Box::new(Expr::Literal(Value::Number(10.0))),
//             operator: Operator::Minus,
//         };
//         assert_eq!(format! {"{}", unary}, "(- 10)");

//         let unary = Expr::Unary {
//             expr: Box::new(Expr::Literal(Value::Bool(false))),
//             operator: Operator::Bang,
//         };
//         assert_eq!(format! {"{}", unary}, "(! false)");
//     }

//     #[test]
//     fn prints_example() {
//         let ex = Expr::Binary {
//             left: Box::new(Expr::Unary {
//                 operator: Operator::Minus,
//                 expr: Box::new(Expr::Literal(Value::Number(123.0))),
//             }),
//             right: Box::new(Expr::Grouping(Box::new(Expr::Literal(Value::Number(
//                 45.67,
//             ))))),
//             operator: Operator::Multiply,
//         };

//         assert_eq!(format! {"{}", ex}, "(* (- 123) (group 45.67))");
//     }
// }
