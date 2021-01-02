use crate::token::*;
use std::fmt;

#[derive(Debug)]
pub struct Program {
  pub errors: Vec<String>,
  pub statements: Vec<Statement>,
}

impl Program {
  pub fn has_errors(&self) -> bool {
    !self.errors.is_empty()
  }
}

impl fmt::Display for Program {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for statement in &self.statements {
      statement.fmt(f)?;
    }

    Ok(())
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
  pub operator: Token,
  pub operand: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
  pub left_operand: Box<Expression>,
  pub operator: Token,
  pub right_operand: Box<Expression>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
  pub condition: Box<Expression>,
  pub consequence: Vec<Statement>,
  pub alternative: Option<Vec<Statement>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionExpression {
  pub parameters: Vec<String>,
  pub body: Vec<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
  pub function: Box<Expression>,
  pub arguments: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AcessExpression {
  pub object: Expression,
  pub key: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
  Identifier(String),
  Number(f64),
  String(String),
  Prefix(PrefixExpression),
  Infix(InfixExpression),
  Boolean(bool),
  If(IfExpression),
  Function(FunctionExpression),
  Call(CallExpression),
  Array(Vec<Expression>),
  Null,
  Access(Box<AcessExpression>),
}

impl fmt::Display for Expression {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Expression::Identifier(identifier) => write!(f, "{}", identifier),
      Expression::Number(number) => write!(f, "{}", number),
      Expression::String(string) => write!(f, "{}", string),
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
        write!(f, "(if ({}) ", expression.condition)?;

        write!(f, "{{ ")?;

        for statement in &expression.consequence {
          statement.fmt(f)?;
        }

        write!(f, " }}")?;

        if let Some(statements) = &expression.alternative {
          write!(f, " else {{ ")?;

          for statement in statements {
            statement.fmt(f)?;
          }

          write!(f, " }})")
        } else {
          write!(f, ")")
        }
      }
      Expression::Function(function) => {
        write!(f, "(fn(")?;

        for (index, parameter) in function.parameters.iter().enumerate() {
          if index > 0 {
            write!(f, ", {}", parameter)?;
          } else {
            write!(f, "{}", parameter)?;
          }
        }

        write!(f, ") ")?;

        write!(f, "{{ ")?;

        for statement in &function.body {
          statement.fmt(f)?;
        }

        write!(f, " }})")
      }
      Expression::Call(call) => {
        write!(f, "(")?;
        call.function.fmt(f)?;

        write!(f, "(")?;

        for (index, argument) in call.arguments.iter().enumerate() {
          argument.fmt(f)?;

          if index < call.arguments.len() - 1 {
            write!(f, ", ")?;
          }
        }

        write!(f, "))")?;

        Ok(())
      }
      Expression::Null => write!(f, "(null)"),
      Expression::Array(expressions) => {
        write!(f, "([")?;

        for (index, expression) in expressions.iter().enumerate() {
          expression.fmt(f)?;

          if index < expressions.len() - 1 {
            write!(f, ", ")?;
          }
        }

        write!(f, "])")
      }
      Expression::Access(expression) => {
        write!(f, "(")?;

        match expression.object {
          Expression::Array(_) => {
            expression.object.fmt(f)?;
            write!(f, "[{}]", expression.key)?;
          }

          _ => unreachable!(),
        }

        write!(f, ")")
      }
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
  Let(LetStatement),
  Return(Expression),
  Expression(Expression),
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
    }
  }
}
