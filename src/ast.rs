use crate::token::*;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Expression {
  Identifier(String),
  Number(f64),
}

impl fmt::Display for Expression {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Expression::Identifier(identifier) => write!(f, "Identifier({:?})", identifier),
      Expression::Number(number) => write!(f, "Number({:?})", number),
    }
  }
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
  pub token: Token,
  pub identifier: Token,
  //value: Expression,
}

impl fmt::Display for LetStatement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.identifier {
      Token::Identifier(identifier) => write!(f, "Token({:?})", identifier),
      _ => unreachable!(),
    }
  }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
  Let(LetStatement),
  Return(Expression),
  Expression(Expression),
}

impl fmt::Display for Statement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Statement::Let(statement) => statement.fmt(f),
      Statement::Return(statement) => statement.fmt(f),
      Statement::Expression(statement) => statement.fmt(f),
    }
  }
}

#[derive(Debug, PartialEq)]
pub enum Node {
  Statement(Statement),
}

#[derive(Debug)]
pub struct Program {
  pub statements: Vec<Statement>,
  pub errors: Vec<String>,
}
