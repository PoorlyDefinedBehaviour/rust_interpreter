use crate::ast::{Expression, Program, Statement};

#[derive(Debug, PartialEq)]
pub enum Object {
  Number(f64),
  Boolean(bool),
  Unit,
}

pub struct Interpreter {
  program: Program,
}

impl Interpreter {
  pub fn new(program: Program) -> Self {
    Interpreter { program }
  }

  pub fn evaluate(&self) -> Object {
    let mut result: Object = Object::Unit;

    for statement in &self.program.statements {
      result = self.eval(statement);
    }

    result
  }

  fn eval(&self, statement: &Statement) -> Object {
    match statement {
      Statement::Expression(Expression::Number(number)) => Object::Number(*number),
      Statement::Expression(Expression::Boolean(t)) => Object::Boolean(*t),
      _ => panic!("unexpected statement: {:?}", statement),
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
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();
      let interpreter = Interpreter::new(program);

      assert_eq!(interpreter.evaluate(), expected);
    }
  }
}
