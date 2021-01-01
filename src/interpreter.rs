use crate::ast::{Expression, Program, Statement};
use crate::token::Token;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
  Number(f64),
  Boolean(bool),
  Null,
  Return(Box<Object>),
  Identifier(String),
}

type InterpreterError = String;

impl fmt::Display for Object {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Object::Number(number) => write!(f, "{}", number),
      Object::Boolean(boolean) => write!(f, "{}", boolean),
      Object::Null => write!(f, "null"),
      Object::Return(object) => {
        write!(f, "return ")?;
        object.fmt(f)
      }
      Object::Identifier(identifier) => write!(f, "{}", identifier),
    }
  }
}

#[derive(Debug)]
pub struct Environment {
  scopes: Vec<HashMap<String, Object>>,
}

impl Environment {
  pub fn new() -> Self {
    Environment {
      scopes: vec![HashMap::new()],
    }
  }

  pub fn create_scope(&mut self) {
    self.scopes.push(HashMap::new());
  }

  pub fn destroy_current_scope(&mut self) {
    self.scopes.pop();
  }

  pub fn set_binding(&mut self, identifier: String, value: Object) -> Result<(), String> {
    match self.scopes.last_mut() {
      Some(scope) => {
        if scope.contains_key(&identifier) {
          return Err(format!(
            "identifier {} is already declared in this scope",
            identifier
          ));
        }

        scope.insert(identifier, value);

        Ok(())
      }
      None => Err(String::from("environment has no scopes")),
    }
  }

  pub fn get_binding(&self, identifier: &String) -> Option<&Object> {
    self.scopes.last().and_then(|scope| scope.get(identifier))
  }
}

pub struct Interpreter {
  enviroment: Environment,
}

impl Interpreter {
  pub fn new() -> Self {
    Interpreter {
      enviroment: Environment::new(),
    }
  }

  pub fn evaluate(&mut self, program: &Program) -> Result<Object, InterpreterError> {
    match self.eval_statements(&program.statements) {
      Ok(Object::Return(object)) => Ok(*object),
      result => result,
    }
  }

  fn eval_statement(&mut self, statement: &Statement) -> Result<Object, InterpreterError> {
    match statement {
      Statement::Expression(expression) => Ok(self.eval_expression(expression)?),
      Statement::Block(statements) => self.eval_statements(statements),
      Statement::Return(expression) => {
        let object = self.eval_expression(expression)?;
        Ok(Object::Return(Box::new(object)))
      }
      Statement::Let(statement) => {
        let identifier = statement.identifier.to_string();
        let value = self.eval_expression(&statement.value)?;

        self.enviroment.set_binding(identifier, value.clone())?;

        Ok(value)
      }
    }
  }

  fn eval_statements(&mut self, statements: &[Statement]) -> Result<Object, InterpreterError> {
    let mut result: Object = Object::Null;

    for statement in statements {
      result = self.eval_statement(statement)?;

      if matches!(result, Object::Return(_)) {
        return Ok(result);
      }
    }

    Ok(result)
  }

  fn eval_expression(&mut self, expression: &Expression) -> Result<Object, InterpreterError> {
    match expression {
      Expression::Number(number) => Ok(Object::Number(*number)),
      Expression::Boolean(t) => Ok(Object::Boolean(*t)),
      Expression::Prefix(expression) => {
        let operand = self.eval_expression(&expression.operand)?;

        match (&expression.operator, operand) {
          (Token::Bang, operand) => Ok(self.not(self.to_boolean(operand))),
          (Token::Minus, Object::Number(number)) => Ok(Object::Number(-number)),
          (token, operand) => Err(format!(
            "unexpected prefix expression: {}{}",
            token, operand
          )),
        }
      }
      Expression::Infix(expression) => {
        let left_operand = self.eval_expression(&expression.left_operand)?;

        let right_operand = self.eval_expression(&expression.right_operand)?;

        match (left_operand, &expression.operator, right_operand) {
          (Object::Number(left), Token::Plus, Object::Number(right)) => {
            Ok(Object::Number(left + right))
          }
          (Object::Number(left), Token::Minus, Object::Number(right)) => {
            Ok(Object::Number(left - right))
          }
          (Object::Number(left), Token::Star, Object::Number(right)) => {
            Ok(Object::Number(left * right))
          }
          (Object::Number(left), Token::Slash, Object::Number(right)) => {
            Ok(Object::Number(left / right))
          }
          (Object::Number(left), Token::LessThan, Object::Number(right)) => {
            Ok(Object::Boolean(left < right))
          }
          (Object::Number(left), Token::LessThanOrEqual, Object::Number(right)) => {
            Ok(Object::Boolean(left <= right))
          }
          (Object::Number(left), Token::GreaterThan, Object::Number(right)) => {
            Ok(Object::Boolean(left > right))
          }
          (Object::Number(left), Token::GreaterThanOrEqual, Object::Number(right)) => {
            Ok(Object::Boolean(left >= right))
          }
          (left, Token::Equal, right) => Ok(Object::Boolean(left == right)),
          (left, Token::NotEqual, right) => Ok(Object::Boolean(left != right)),
          (left, token, right) => Err(format!(
            "unexpected infix expression: {} {} {}",
            left, token, right
          )),
        }
      }
      Expression::If(expression) => {
        let object = self.eval_expression(&expression.condition)?;
        let boolean = self.to_boolean(object);

        match boolean {
          Object::Boolean(true) => self.eval_statement(&expression.consequence),
          Object::Boolean(false) => match &expression.alternative {
            Some(alternative) => self.eval_statement(alternative),
            None => Ok(Object::Null),
          },
          _ => unreachable!(),
        }
      }
      Expression::Identifier(identifier) => match self.enviroment.get_binding(&identifier) {
        Some(object) => Ok(object.clone()),
        None => Err(format!("identifier not found: {}", identifier)),
      },
      _ => unreachable!(),
    }
  }

  fn to_boolean(&self, object: Object) -> Object {
    match object {
      Object::Boolean(_) => object,
      Object::Number(number) => Object::Boolean(number != 0.0),
      Object::Null => Object::Boolean(false),
      Object::Identifier(identifier) => match self.enviroment.get_binding(&identifier) {
        Some(object) => self.to_boolean(object.clone()),
        None => Object::Boolean(false),
      },
      Object::Return(_) => unreachable!(),
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
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(&program).unwrap(), expected);
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
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(&program).unwrap(), expected);
    }
  }

  #[test]
  fn prefix_expression_errors() {
    let test_cases: Vec<(&str, &str)> = vec![
      ("-true", "unexpected prefix expression: -true"),
      ("-false", "unexpected prefix expression: -false"),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(&program), Err(String::from(expected)));
    }
  }

  #[test]
  fn infix_expressions() {
    let test_cases: Vec<(&str, Object)> = vec![
      ("5 + 5 + 5 + 5 - 10", Object::Number(10.0)),
      ("2 * 2 * 2 * 2 * 2", Object::Number(32.0)),
      ("-50 + 100 - 50", Object::Number(0.0)),
      ("5 * 2 + 10", Object::Number(20.0)),
      ("20 + 2 * -10", Object::Number(0.0)),
      ("5 + 2 * 10", Object::Number(25.0)),
      ("20 + 2 * - 10", Object::Number(0.0)),
      ("50 / 2 * 2 + 10", Object::Number(60.0)),
      ("2 * (5 + 10)", Object::Number(30.0)),
      ("3 * (3 * 3) + 10", Object::Number(37.0)),
      ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Number(50.0)),
      ("1 < 2", Object::Boolean(true)),
      ("1 > 2", Object::Boolean(false)),
      ("1 < 1", Object::Boolean(false)),
      ("1 > 1", Object::Boolean(false)),
      ("1 == 1", Object::Boolean(true)),
      ("1 != 1", Object::Boolean(false)),
      ("1 == 2", Object::Boolean(false)),
      ("1 != 2", Object::Boolean(true)),
      ("-1 == (-1)", Object::Boolean(true)),
      ("1 == -1", Object::Boolean(false)),
      ("-(2 + 2) == 4", Object::Boolean(false)),
      ("-(2 + 2) == -4", Object::Boolean(true)),
      ("true == true", Object::Boolean(true)),
      ("true != true", Object::Boolean(false)),
      ("true == false", Object::Boolean(false)),
      ("false == true", Object::Boolean(false)),
      ("false != true", Object::Boolean(true)),
      ("(1 < 2) == true", Object::Boolean(true)),
      ("(1 < 2) == false", Object::Boolean(false)),
      ("(1 > 2) == true", Object::Boolean(false)),
      ("(1 > 2) == false", Object::Boolean(true)),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(&program).unwrap(), expected);
    }
  }

  #[test]
  fn infix_expression_errors() {
    let test_cases: Vec<(&str, &str)> = vec![
      ("true > false", "unexpected infix expression: true > false"),
      ("true < false", "unexpected infix expression: true < false"),
      ("false > true", "unexpected infix expression: false > true"),
      ("false < true", "unexpected infix expression: false < true"),
      (
        "false > false",
        "unexpected infix expression: false > false",
      ),
      (
        "false < false",
        "unexpected infix expression: false < false",
      ),
      ("true > true", "unexpected infix expression: true > true"),
      ("true < true", "unexpected infix expression: true < true"),
      (
        "false + false",
        "unexpected infix expression: false + false",
      ),
      ("false + true", "unexpected infix expression: false + true"),
      ("true + false", "unexpected infix expression: true + false"),
      ("true + true", "unexpected infix expression: true + true"),
      (
        "false - false",
        "unexpected infix expression: false - false",
      ),
      ("false - true", "unexpected infix expression: false - true"),
      ("true - true", "unexpected infix expression: true - true"),
      ("true - false", "unexpected infix expression: true - false"),
      (
        "false * false",
        "unexpected infix expression: false * false",
      ),
      ("false * true", "unexpected infix expression: false * true"),
      ("true * true", "unexpected infix expression: true * true"),
      ("true * false", "unexpected infix expression: true * false"),
      (
        "false / false",
        "unexpected infix expression: false / false",
      ),
      ("false / true", "unexpected infix expression: false / true"),
      ("true / true", "unexpected infix expression: true / true"),
      ("true / false", "unexpected infix expression: true / false"),
      (
        "false |> false",
        "unexpected infix expression: false |> false",
      ),
      (
        "false |> true",
        "unexpected infix expression: false |> true",
      ),
      ("true |> true", "unexpected infix expression: true |> true"),
      (
        "true |> false",
        "unexpected infix expression: true |> false",
      ),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(&program), Err(String::from(expected)));
    }
  }

  #[test]
  fn if_expressions() {
    let test_cases: Vec<(&str, Object)> = vec![
      ("if(true) { 10 }", Object::Number(10.0)),
      ("if (false) {10}", Object::Null),
      ("if(1){-3}", Object::Number(-3.0)),
      ("if (1 < 2) { 5 }", Object::Number(5.0)),
      ("if (1 > 2) { 10 }", Object::Null),
      ("if (1 > 2) { 10 } else { 20 }", Object::Number(20.0)),
      ("if(1 < 2) {10} else {20}", Object::Number(10.0)),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(&program).unwrap(), expected);
    }
  }

  #[test]
  fn return_statements() {
    let test_cases: Vec<(&str, Object)> = vec![
      ("return 10", Object::Number(10.0)),
      ("return 10 9", Object::Number(10.0)),
      ("return 2 * 5", Object::Number(10.0)),
      ("9 return 2 + 5", Object::Number(7.0)),
      (
        "if(10 > 1) {
          if(10 > 1) {
            return 42
          }

          return 32 
        }",
        Object::Number(42.0),
      ),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(&program).unwrap(), expected);
    }
  }

  #[test]
  fn bindings() {
    let test_cases: Vec<(&str, Object)> = vec![
      (
        "
        let a = 5
        a
       ",
        Object::Number(5.0),
      ),
      (
        "
        let a = 5 * 5
        a
       ",
        Object::Number(25.0),
      ),
      (
        "
        let a = 5
        let b = a
        b
        ",
        Object::Number(5.0),
      ),
      (
        "
        let a = 5
        let b = a
        let c = a + b + 5
        c
      ",
        Object::Number(15.0),
      ),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(&program).unwrap(), expected);
    }
  }

  #[test]
  fn binding_errors() {
    let test_cases: Vec<(&str, &str)> = vec![
      ("foobar", "identifier not found: foobar"),
      (
        "
        let a = 10
        let a = 20
        ",
        "identifier a is already declared in this scope",
      ),
      (
        "
        let a = 10
        let b = 20
        let a = 30
        ",
        "identifier a is already declared in this scope",
      ),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(&program), Err(String::from(expected)));
    }
  }
}
