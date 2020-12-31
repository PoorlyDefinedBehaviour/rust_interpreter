use crate::ast::{Expression, Program, Statement};
use crate::token::Token;

#[derive(Debug, PartialEq)]
pub enum Object {
  Number(f64),
  Boolean(bool),
  Null,
}

pub struct Interpreter {
  program: Program,
}

impl Interpreter {
  pub fn new(program: Program) -> Self {
    Interpreter { program }
  }

  pub fn evaluate(&self) -> Object {
    let mut result: Object = Object::Null;

    for statement in &self.program.statements {
      result = self.eval_statement(statement);
    }

    result
  }

  fn eval_statement(&self, statement: &Statement) -> Object {
    match statement {
      Statement::Expression(expression) => self.eval_expression(expression),
      _ => panic!("unexpected statement: {:?}", statement),
    }
  }

  fn eval_expression(&self, expression: &Expression) -> Object {
    match expression {
      Expression::Number(number) => Object::Number(*number),
      Expression::Boolean(t) => Object::Boolean(*t),
      Expression::Prefix(expression) => {
        let operand = self.eval_expression(&expression.operand);

        match &expression.operator {
          Token::Bang => self.not(self.to_boolean(operand)),
          Token::Minus => match &operand {
            Object::Number(number) => Object::Number(-number),
            object => panic!("operator - expected a number, got {:?}", object),
          },
          token => panic!(
            "unexpected token found in prefix operator position: {:?}",
            token
          ),
        }
      }
      _ => panic!("unexpected expression: {:?}", expression),
    }
  }

  fn to_boolean(&self, object: Object) -> Object {
    match object {
      Object::Boolean(_) => object,
      Object::Number(number) => Object::Boolean(number != 0.0),
      Object::Null => Object::Boolean(false),
    }
  }

  fn not(&self, object: Object) -> Object {
    match object {
      Object::Boolean(boolean) => Object::Boolean(!boolean),
      _ => unreachable!(),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::lexer::Lexer;
  use crate::parser::Parser;

  #[test]
  fn simple_literals() {
    let test_cases: Vec<(&str, Object)> = vec![
      ("2", Object::Number(2.0)),
      ("43283", Object::Number(43283.0)),
      ("0", Object::Number(0.0)),
      ("42184912841", Object::Number(42184912841.0)),
      (
        "412041204812942198312938912839123",
        Object::Number(412041204812942198312938912839123.0),
      ),
      ("941912921421", Object::Number(941912921421.0)),
      ("true", Object::Boolean(true)),
      ("false", Object::Boolean(false)),
      ("null", Object::Null),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();
      let interpreter = Interpreter::new(program);

      assert_eq!(interpreter.evaluate(), expected);
    }
  }

  #[test]
  fn prefix_expressions() {
    let test_cases: Vec<(&str, Object)> = vec![
      ("!true", Object::Boolean(false)),
      ("!false", Object::Boolean(true)),
      ("!5", Object::Boolean(false)),
      ("!!true", Object::Boolean(true)),
      ("!!false", Object::Boolean(false)),
      ("!!3", Object::Boolean(true)),
      ("-5", Object::Number(-5.0)),
      ("-10", Object::Number(-10.0)),
      ("-0", Object::Number(-0.0)),
      ("-0", Object::Number(0.0)),
      ("!-0", Object::Boolean(true)),
      ("!-42834283", Object::Boolean(false)),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();
      let interpreter = Interpreter::new(program);

      assert_eq!(interpreter.evaluate(), expected);
    }
  }
}
