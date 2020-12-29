use crate::token::*;
use std::fmt;

#[derive(Debug)]
pub struct Program {
  pub errors: Vec<String>,
  pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for statement in &self.statements {
      statement.fmt(f)?;
    }

    Ok(())
  }
}

#[derive(Debug, PartialEq)]
pub struct PrefixExpression {
  pub operator: Token,
  pub operand: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct InfixExpression {
  pub left_operand: Box<Expression>,
  pub operator: Token,
  pub right_operand: Box<Expression>,
}
#[derive(Debug, PartialEq)]
pub struct IfExpression {
  pub condition: Box<Expression>,
  pub consequence: Box<Statement>,
  pub alternative: Option<Box<Statement>>,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
  Identifier(String),
  Number(f64),
  Prefix(PrefixExpression),
  Infix(InfixExpression),
  Boolean(bool),
  If(IfExpression),
}

impl fmt::Display for Expression {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Expression::Identifier(identifier) => write!(f, "{}", identifier),
      Expression::Number(number) => write!(f, "{}", number),
      Expression::Prefix(expression) => {
        write!(f, "({} {})", expression.operator, expression.operand)
      }
      Expression::Infix(expression) => write!(
        f,
        "({} {} {})",
        expression.left_operand, expression.operator, expression.right_operand
      ),
      Expression::Boolean(boolean) => write!(f, "{}", boolean),
      Expression::If(expression) => {
        write!(
          f,
          "(if ({}) {}",
          expression.condition, expression.consequence
        )?;

        if let Some(expression) = &expression.alternative {
          write!(f, " else {})", expression)
        } else {
          write!(f, ")")
        }
      }
    }
  }
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
  pub token: Token,
  pub identifier: Token,
  pub value: Expression,
}

impl fmt::Display for LetStatement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.identifier {
      Token::Identifier(identifier) => {
        write!(f, "let {} = ", identifier)?;
        self.value.fmt(f)
      }
      _ => unreachable!(),
    }
  }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
  Let(LetStatement),
  Return(Expression),
  Expression(Expression),
  Block(Vec<Statement>),
}

impl fmt::Display for Statement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Statement::Let(statement) => statement.fmt(f),
      Statement::Return(statement) => {
        write!(f, "return ")?;
        statement.fmt(f)
      }
      Statement::Expression(statement) => statement.fmt(f),
      Statement::Block(statements) => {
        write!(f, "{{ ")?;

        for statement in statements {
          statement.fmt(f)?;
        }

        write!(f, " }}")
      }
    }
  }
}
