use crate::token::*;

#[derive(Debug, PartialEq)]
pub struct LetStatement {
  pub token: Token,
  pub identifier: Token,
  //value: Expression,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
  Let(LetStatement),
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
