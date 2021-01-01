use crate::ast::{CallExpression, Expression, Program, Statement};
use crate::token::Token;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionObject {
  pub parameters: Vec<String>,
  pub body: Vec<Statement>,
}

pub struct BuiltinFunction(fn(&Interpreter, Vec<Object>) -> Result<Object, InterpreterError>);

impl fmt::Debug for BuiltinFunction {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "fn(...) {{ /* internal */ }}")
  }
}

impl std::cmp::PartialEq for BuiltinFunction {
  fn eq(&self, _other: &Self) -> bool {
    false
  }
}

impl std::clone::Clone for BuiltinFunction {
  fn clone(&self) -> Self {
    BuiltinFunction(self.0)
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
  Number(f64),
  String(String),
  Boolean(bool),
  Null,
  Return(Box<Object>),
  Identifier(String),
  Function(FunctionObject),
  BuiltinFunction(BuiltinFunction),
}

type InterpreterError = String;

impl fmt::Display for Object {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Object::Number(number) => write!(f, "{}", number),
      Object::String(string) => write!(f, "{}", string),
      Object::Boolean(boolean) => write!(f, "{}", boolean),
      Object::Null => write!(f, "null"),
      Object::Return(object) => {
        write!(f, "return ")?;
        object.fmt(f)
      }
      Object::Identifier(identifier) => write!(f, "{}", identifier),
      Object::Function(function) => {
        write!(f, "(fn(")?;

        for (index, parameter) in function.parameters.iter().enumerate() {
          if index > 0 {
            write!(f, ", {}", parameter)?;
          } else {
            write!(f, "{}", parameter)?;
          }
        }

        write!(f, ") ")?;

        for statement in &function.body {
          statement.fmt(f)?;
        }

        write!(f, ")")
      }
      _ => unreachable!(),
    }
  }
}

pub struct Environment {
  scopes: Vec<HashMap<String, Object>>,
}

impl Default for Environment {
  fn default() -> Self {
    Environment::new()
  }
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

  pub fn get_binding(&self, identifier: &str) -> Option<&Object> {
    for scope in self.scopes.iter().rev() {
      if let Some(object) = scope.get(identifier) {
        return Some(object);
      }
    }

    None
  }
}

pub struct Interpreter {
  environment: Environment,
}

impl Default for Interpreter {
  fn default() -> Self {
    Interpreter::new()
  }
}

impl Interpreter {
  pub fn new() -> Self {
    let mut interpreter = Interpreter {
      environment: Environment::new(),
    };

    interpreter
      .environment
      .set_binding(
        String::from("len"),
        Object::BuiltinFunction(BuiltinFunction(Interpreter::len)),
      )
      .unwrap();

    interpreter
  }

  fn len(&self, arguments: Vec<Object>) -> Result<Object, InterpreterError> {
    if arguments.len() != 1 {
      return Err(format!(
        "len() expected one argument, got {}",
        arguments.len()
      ));
    }

    let argument = match arguments.first().unwrap() {
      Object::Identifier(identifier) => match self.environment.get_binding(identifier) {
        Some(object) => match object {
          Object::String(string) => string,
          object => return Err(format!("len() can't be used on {}", object)),
        },
        None => return Err(format!("identifier not found: {}", identifier)),
      },
      Object::String(string) => string,
      object => return Err(format!("len() can't be used on {}", object)),
    };

    Ok(Object::Number(argument.len() as f64))
  }

  pub fn evaluate(&mut self, program: Program) -> Result<Object, InterpreterError> {
    match self.eval_statements(program.statements) {
      Ok(Object::Return(object)) => Ok(*object),
      result => result,
    }
  }

  fn eval_statement(&mut self, statement: Statement) -> Result<Object, InterpreterError> {
    match statement {
      Statement::Expression(expression) => Ok(self.eval_expression(expression)?),
      Statement::Return(expression) => {
        let object = self.eval_expression(expression)?;
        Ok(Object::Return(Box::new(object)))
      }
      Statement::Let(statement) => {
        let identifier = statement.identifier.to_string();
        let value = self.eval_expression(statement.value)?;

        self.environment.set_binding(identifier, value.clone())?;

        Ok(value)
      }
    }
  }

  fn eval_statements(&mut self, statements: Vec<Statement>) -> Result<Object, InterpreterError> {
    let mut result: Object = Object::Null;

    for statement in statements {
      result = self.eval_statement(statement)?;

      if matches!(result, Object::Return(_)) {
        return Ok(result);
      }
    }

    Ok(result)
  }

  fn eval_expression(&mut self, expression: Expression) -> Result<Object, InterpreterError> {
    match expression {
      Expression::Number(number) => Ok(Object::Number(number)),
      Expression::String(string) => Ok(Object::String(string)),
      Expression::Boolean(t) => Ok(Object::Boolean(t)),
      Expression::Prefix(expression) => {
        let operand = self.eval_expression(*expression.operand)?;

        match (&expression.operator, &operand) {
          (Token::Bang, operand) => Ok(self.not(self.to_boolean(operand))),
          (Token::Minus, Object::Number(number)) => Ok(Object::Number(-number)),
          (token, operand) => Err(format!(
            "unexpected prefix expression: {}{}",
            token, operand
          )),
        }
      }
      Expression::Infix(expression) => {
        let left_operand = self.eval_expression(*expression.left_operand)?;

        let right_operand = self.eval_expression(*expression.right_operand)?;

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
        let object = self.eval_expression(*expression.condition)?;
        let boolean = self.to_boolean(&object);

        match boolean {
          Object::Boolean(true) => self.eval_statements(expression.consequence),
          Object::Boolean(false) => match expression.alternative {
            Some(alternative) => self.eval_statements(alternative),
            None => Ok(Object::Null),
          },
          _ => unreachable!(),
        }
      }
      Expression::Identifier(identifier) => match self.environment.get_binding(&identifier) {
        Some(object) => Ok(object.clone()),
        None => Err(format!("identifier not found: {}", identifier)),
      },
      Expression::Function(function) => Ok(Object::Function(FunctionObject {
        parameters: function.parameters,
        body: function.body,
      })),
      Expression::Call(expression) => {
        self.environment.create_scope();

        let return_value = self.evaluate_function_call(expression)?;

        self.environment.destroy_current_scope();

        Ok(return_value)
      }
      Expression::Null => Ok(Object::Null),
    }
  }

  fn evaluate_function_call(
    &mut self,
    expression: CallExpression,
  ) -> Result<Object, InterpreterError> {
    let mut arguments = Vec::new();

    for argument in &expression.arguments {
      arguments.push(self.eval_expression(argument.clone())?);
    }

    let (parameters, function_body) = match *expression.function {
      Expression::Function(function) => (function.parameters, function.body),
      Expression::Identifier(identifier) => match self.environment.get_binding(&identifier) {
        Some(Object::BuiltinFunction(BuiltinFunction(builtin_function))) => {
          return builtin_function(self, arguments)
        }
        Some(Object::Function(function)) => (function.parameters.clone(), function.body.clone()),
        _ => return Err(format!("identifier not found: {}", identifier)),
      },
      expression => return Err(format!("expression is not a function: {}", expression)),
    };

    if expression.arguments.len() != parameters.len() {
      return Err(format!(
        "expected {} arguments, got {}",
        parameters.len(),
        expression.arguments.len(),
      ));
    }

    for (parameter, argument) in parameters.iter().zip(arguments.iter()) {
      self
        .environment
        .set_binding(parameter.clone(), argument.clone())?;
    }

    Ok(self.eval_statements(function_body)?)
  }

  fn to_boolean(&self, object: &Object) -> Object {
    match object {
      Object::Boolean(boolean) => Object::Boolean(*boolean),
      Object::Number(number) => Object::Boolean(*number != 0.0),
      Object::Null => Object::Boolean(false),
      Object::Identifier(identifier) => match self.environment.get_binding(&identifier) {
        Some(object) => self.to_boolean(&object),
        None => Object::Boolean(false),
      },
      Object::Function(_) | Object::BuiltinFunction(_) => Object::Boolean(true),
      Object::String(string) => Object::Boolean(!string.is_empty()),
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
      (r#""123""#, Object::String(String::from("123"))),
      (r#""hello""#, Object::String(String::from("hello"))),
      (r#""a""#, Object::String(String::from("a"))),
      (
        r#""?24421@322r23lr2""#,
        Object::String(String::from("?24421@322r23lr2")),
      ),
      (
        r#""let hello = |> f()()()()()""#,
        Object::String(String::from("let hello = |> f()()()()()")),
      ),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex().unwrap());

      let program = parser.parse();
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(program).unwrap(), expected);
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
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex().unwrap());

      let program = parser.parse();
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(program).unwrap(), expected);
    }
  }

  #[test]
  fn prefix_expression_errors() {
    let test_cases: Vec<(&str, &str)> = vec![
      ("-true", "unexpected prefix expression: -true"),
      ("-false", "unexpected prefix expression: -false"),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex().unwrap());

      let program = parser.parse();
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(program), Err(String::from(expected)));
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
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex().unwrap());

      let program = parser.parse();
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(program).unwrap(), expected);
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
      ("false |> false", "expression is not a function: false"),
      ("false |> true", "expression is not a function: true"),
      ("true |> true", "expression is not a function: true"),
      ("true |> false", "expression is not a function: false"),
      ("true |> 10", "expression is not a function: 10"),
      ("true |> 32", "expression is not a function: 32"),
      ("true |> -1", "expression is not a function: (- 1)"),
      ("10 |> 0", "expression is not a function: 0"),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex().unwrap());

      let program = parser.parse();
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(program), Err(String::from(expected)));
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
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex().unwrap());

      let program = parser.parse();
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(program).unwrap(), expected);
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
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex().unwrap());

      let program = parser.parse();
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(program).unwrap(), expected);
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
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex().unwrap());

      let program = parser.parse();
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(program).unwrap(), expected);
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
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex().unwrap());

      let program = parser.parse();
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(program), Err(String::from(expected)));
    }
  }

  #[test]
  fn function_calls() {
    let test_cases: Vec<(&str, Object)> = vec![
      (
        "
        let identity = fn(x) { x }
        identity(5)
        ",
        Object::Number(5.0),
      ),
      (
        "
        let identity = fn(x) { return x }
        identity(5)
        ",
        Object::Number(5.0),
      ),
      (
        "
        let double = fn(x) { x * 2 }
        double(5)
        ",
        Object::Number(10.0),
      ),
      (
        "
        let add = fn(x, y) { x + y}
        add(2, 2)
        ",
        Object::Number(4.0),
      ),
      (
        "
          let factorial = fn(x) {
            if(x <= 1) {
              1
            } else {
              x * factorial(x - 1)
            }
          }

          factorial(5)
        ",
        Object::Number(120.0),
      ),
      ("fn(x) { x }(3)", Object::Number(3.0)),
      (
        "
          let double = fn(x) { x * 2 }
          2 |> double |> double
        ",
        Object::Number(8.0),
      ),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex().unwrap());

      let program = parser.parse();
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(program).unwrap(), expected);
    }
  }

  #[test]
  fn builtin_len_function() {
    let test_cases: Vec<(&str, Object)> = vec![
      (r#"len("hello world")"#, Object::Number(11.0)),
      (r#"len("")"#, Object::Number(0.0)),
      (r#"len("123")"#, Object::Number(3.0)),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex().unwrap());

      let program = parser.parse();
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(program).unwrap(), expected);
    }
  }

  #[test]
  fn builtin_len_function_errors() {
    let test_cases: Vec<(&str, &str)> = vec![
      ("len(1)", "len() can't be used on 1"),
      ("len()", "len() expected one argument, got 0"),
      (
        r#"len("hello", "world")"#,
        "len() expected one argument, got 2",
      ),
      (
        "len(fn(x){ x + 2})",
        "len() can't be used on (fn(x) (x + 2))",
      ),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex().unwrap());

      let program = parser.parse();
      let mut interpreter = Interpreter::new();

      assert_eq!(interpreter.evaluate(program), Err(String::from(expected)));
    }
  }
}
