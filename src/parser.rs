use crate::ast::*;
use crate::token::*;
use std::collections::HashMap;

#[non_exhaustive]
struct Precedence;

impl Precedence {
  pub const NONE: i32 = 0;
  pub const EQUALITY: i32 = 4; // == !=
  pub const COMPARISON: i32 = 5; // < > <= >=
  pub const TERM: i32 = 6; // + -
  pub const FACTOR: i32 = 7; // * /
  pub const UNARY: i32 = 8; // ! -
  pub const CALL: i32 = 9; // . () |>
}

fn token_precedence(token: Option<&Token>) -> i32 {
  match token {
    Some(Token::GreaterThan)
    | Some(Token::LessThan)
    | Some(Token::GreaterThanOrEqual)
    | Some(Token::LessThanOrEqual) => Precedence::COMPARISON,
    Some(Token::Assign) | Some(Token::Equal) | Some(Token::NotEqual) => Precedence::EQUALITY,
    Some(Token::Plus) | Some(Token::Minus) => Precedence::TERM,
    Some(Token::Star) | Some(Token::Slash) => Precedence::FACTOR,
    Some(Token::Pipe) | Some(Token::LeftParen) => Precedence::CALL,
    _ => Precedence::NONE,
  }
}

type PrefixParselet = fn(&mut Parser, &Token) -> Expression;
type InfixParselet = fn(&mut Parser, Expression, &Token) -> Expression;

pub struct Parser {
  current_position: usize,
  tokens: Vec<Token>,
  errors: Vec<String>,
  prefix_parselets: HashMap<std::mem::Discriminant<Token>, PrefixParselet>,
  infix_parselets: HashMap<std::mem::Discriminant<Token>, InfixParselet>,
}

impl Parser {
  pub fn new(tokens: Vec<Token>) -> Self {
    let mut parser = Parser {
      current_position: 0,
      errors: vec![],
      prefix_parselets: HashMap::new(),
      infix_parselets: HashMap::new(),
      tokens,
    };

    parser.prefix(
      std::mem::discriminant(&Token::Identifier(String::from("_"))),
      Parser::parse_identifier,
    );

    parser.prefix(
      std::mem::discriminant(&Token::Number(String::from("_"))),
      Parser::parse_number,
    );

    parser.prefix(std::mem::discriminant(&Token::True), Parser::parse_boolean);

    parser.prefix(std::mem::discriminant(&Token::False), Parser::parse_boolean);

    parser.prefix(
      std::mem::discriminant(&Token::Minus),
      Parser::parse_prefix_expression,
    );

    parser.prefix(
      std::mem::discriminant(&Token::Bang),
      Parser::parse_prefix_expression,
    );

    parser.prefix(
      std::mem::discriminant(&Token::LeftParen),
      Parser::parse_grouped_expression,
    );

    parser.prefix(
      std::mem::discriminant(&Token::If),
      Parser::parse_if_expression,
    );

    parser.prefix(
      std::mem::discriminant(&Token::Function),
      Parser::parse_function_literal,
    );

    parser.infix(
      std::mem::discriminant(&Token::Plus),
      Parser::parse_infix_expression,
    );

    parser.infix(
      std::mem::discriminant(&Token::Minus),
      Parser::parse_infix_expression,
    );

    parser.infix(
      std::mem::discriminant(&Token::Slash),
      Parser::parse_infix_expression,
    );

    parser.infix(
      std::mem::discriminant(&Token::Star),
      Parser::parse_infix_expression,
    );

    parser.infix(
      std::mem::discriminant(&Token::Equal),
      Parser::parse_infix_expression,
    );

    parser.infix(
      std::mem::discriminant(&Token::NotEqual),
      Parser::parse_infix_expression,
    );

    parser.infix(
      std::mem::discriminant(&Token::GreaterThan),
      Parser::parse_infix_expression,
    );

    parser.infix(
      std::mem::discriminant(&Token::LessThan),
      Parser::parse_infix_expression,
    );

    parser.infix(
      std::mem::discriminant(&Token::GreaterThanOrEqual),
      Parser::parse_infix_expression,
    );

    parser.infix(
      std::mem::discriminant(&Token::LessThanOrEqual),
      Parser::parse_infix_expression,
    );

    parser.infix(
      std::mem::discriminant(&Token::Pipe),
      Parser::parse_infix_expression,
    );

    parser.infix(
      std::mem::discriminant(&Token::LeftParen),
      Parser::parse_function_call_expression,
    );

    parser
  }

  pub fn parse(&mut self) -> Program {
    let mut statements = Vec::new();

    loop {
      match self.current_token() {
        Some(Token::Eof) | None => break,
        Some(_) => match self.parse_statement() {
          Ok(statement) => statements.push(statement),
          Err(message) => self.errors.push(message),
        },
      }
    }

    Program {
      statements,
      errors: self.errors.clone(),
    }
  }

  fn prefix(&mut self, token: std::mem::Discriminant<Token>, parselet: PrefixParselet) {
    self.prefix_parselets.insert(token, parselet);
  }

  fn infix(&mut self, token: std::mem::Discriminant<Token>, parselet: InfixParselet) {
    self.infix_parselets.insert(token, parselet);
  }

  fn has_tokens_to_parse(&self) -> bool {
    self.current_position < self.tokens.len() - 1
  }

  fn current_token(&self) -> Option<&Token> {
    self.tokens.get(self.current_position)
  }

  fn next_token(&mut self) -> Option<&Token> {
    let token = self.tokens.get(self.current_position);

    self.current_position += 1;

    token
  }

  fn consume(&mut self, expected_token: Token) {
    match self.next_token() {
      Some(token) if *token != expected_token => {
        let error = format!("expected {:?}, got {:?}", expected_token, token);

        self.errors.push(error);
      }
      None => {
        let error = format!("expected {:?}, got nothing", expected_token);

        self.errors.push(error);
      }
      _ => {}
    }
  }

  fn consume_with_value(&mut self, expected_token_constructor: fn(String) -> Token) -> Token {
    let token = self.next_token().cloned().unwrap();

    let expected_token_t = expected_token_constructor(String::from("_"));

    if std::mem::discriminant(&token) != std::mem::discriminant(&expected_token_t) {
      let error = format!("expected {:?}, got {:?}", expected_token_t, token);

      self.errors.push(error);
    }

    token
  }

  fn parse_statement(&mut self) -> Result<Statement, String> {
    match self.current_token() {
      Some(Token::Let) => self.parse_let_statement(),
      Some(Token::Return) => self.parse_return_statement(),
      Some(_statement) => self
        .parse_expression_statement(Precedence::NONE)
        .map(Statement::Expression),
      None => panic!("no tokens left to parse"),
    }
  }

  fn parse_expression_statement(&mut self, precedence: i32) -> Result<Expression, String> {
    let mut token = self.next_token().cloned().unwrap();

    match self.prefix_parselets.get(&std::mem::discriminant(&token)) {
      None => Err(format!("no prefix parselet found for {:?}", token)),
      Some(prefix_parselet) => {
        let mut left = prefix_parselet(self, &token);

        loop {
          if precedence >= token_precedence(self.current_token()) || !self.has_tokens_to_parse() {
            return Ok(left);
          }

          token = self.next_token().cloned().unwrap();

          let infix_parselet = self
            .infix_parselets
            .get(&std::mem::discriminant(&token))
            .unwrap();

          left = infix_parselet(self, left, &token);
        }
      }
    }
  }

  fn parse_prefix_expression(&mut self, token: &Token) -> Expression {
    let operand = self.parse_expression_statement(Precedence::UNARY).unwrap();

    Expression::Prefix(PrefixExpression {
      operator: token.clone(),
      operand: Box::new(operand),
    })
  }

  fn parse_infix_expression(&mut self, left_operand: Expression, token: &Token) -> Expression {
    let right_operand = self
      .parse_expression_statement(token_precedence(Some(token)))
      .unwrap();

    Expression::Infix(InfixExpression {
      left_operand: Box::new(left_operand),
      operator: token.clone(),
      right_operand: Box::new(right_operand),
    })
  }

  fn parse_grouped_expression(&mut self, _token: &Token) -> Expression {
    let expression = self.parse_expression_statement(Precedence::NONE).unwrap();

    self.consume(Token::RightParen);

    expression
  }

  fn parse_if_expression(&mut self, _token: &Token) -> Expression {
    self.consume(Token::LeftParen);

    let condition = self.parse_expression_statement(Precedence::NONE).unwrap();

    self.consume(Token::RightParen);

    let consequence = self.parse_block_statement();

    let alternative = match self.current_token() {
      Some(Token::Else) => {
        self.next_token();

        Some(self.parse_block_statement())
      }
      _ => None,
    };

    Expression::If(IfExpression {
      condition: Box::new(condition),
      consequence,
      alternative,
    })
  }

  fn parse_function_literal(&mut self, _token: &Token) -> Expression {
    let parameters = self.parse_function_parameters();

    let body = self.parse_block_statement();

    Expression::Function(FunctionExpression {
      parameters: parameters,
      body,
    })
  }

  fn parse_function_parameters(&mut self) -> Vec<String> {
    self.consume(Token::LeftParen);

    let mut parameters: Vec<String> = Vec::new();

    while self.has_tokens_to_parse()
      && matches!(self.current_token(), Some(token) if token != &Token::RightParen)
    {
      match self.next_token() {
        Some(Token::Comma) => {}
        Some(Token::Identifier(identifier)) => parameters.push(identifier.clone()),
        Some(Token::RightParen) => break,
        token => {
          let error_message = format!("expected function parameters, got {:?}", token);
          self.errors.push(error_message);
        }
      }
    }

    self.consume(Token::RightParen);

    parameters
  }

  fn parse_function_call_expression(&mut self, function: Expression, _token: &Token) -> Expression {
    let arguments = self.parse_function_call_arguments();

    Expression::Call(CallExpression {
      function: Box::new(function),
      arguments,
    })
  }

  fn parse_function_call_arguments(&mut self) -> Vec<Expression> {
    let mut arguments: Vec<Expression> = Vec::new();

    while self.has_tokens_to_parse()
      && matches!(self.current_token(), Some(token) if token != &Token::RightParen)
    {
      match self.current_token() {
        Some(Token::Comma) => {
          self.next_token();
        }
        _ => {
          let expression = self.parse_expression_statement(Precedence::NONE).unwrap();

          arguments.push(expression);
        }
      }
    }

    self.consume(Token::RightParen);

    arguments
  }

  fn parse_block_statement(&mut self) -> Vec<Statement> {
    self.consume(Token::LeftBrace);

    let mut statements: Vec<Statement> = vec![];

    loop {
      match self.current_token() {
        Some(Token::RightBrace) | Some(Token::Eof) | None => break,
        _ => match self.parse_statement() {
          Ok(statement) => statements.push(statement),
          Err(message) => self.errors.push(message),
        },
      }
    }

    self.consume(Token::RightBrace);

    statements
  }

  fn parse_identifier(&mut self, token: &Token) -> Expression {
    match token {
      Token::Identifier(identifier) => Expression::Identifier(identifier.clone()),
      _ => unreachable!(),
    }
  }

  fn parse_boolean(&mut self, token: &Token) -> Expression {
    match token {
      Token::True => Expression::Boolean(true),
      Token::False => Expression::Boolean(false),
      _ => unreachable!(),
    }
  }

  fn parse_number(&mut self, token: &Token) -> Expression {
    match &token {
      Token::Number(number) => Expression::Number(number.parse::<f64>().unwrap()),
      _ => unreachable!(),
    }
  }

  fn parse_return_statement(&mut self) -> Result<Statement, String> {
    self.consume(Token::Return);

    let return_value = self.parse_expression_statement(Precedence::NONE)?;

    Ok(Statement::Return(return_value))
  }

  fn parse_let_statement(&mut self) -> Result<Statement, String> {
    let token = self.next_token().cloned().unwrap();

    let identifier = self.consume_with_value(Token::Identifier);

    self.consume(Token::Assign);

    let value = self.parse_expression_statement(Precedence::NONE)?;

    Ok(Statement::Let(LetStatement {
      token,
      identifier,
      value,
    }))
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::Lexer;

  #[test]
  fn parse_let_statement() {
    let test_cases = vec![("let x = 5", "let x = 5")];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();

      assert!(!program.has_errors());

      assert_eq!(program.to_string(), expected);
    }
  }

  #[test]
  fn parse_let_statement_error() {
    let test_cases = vec![("let = 5;", "expected Identifier(\"_\"), got Assign")];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();

      assert!(program.has_errors());

      assert_eq!(program.errors[0], expected);
    }
  }

  #[test]
  fn parse_booleans() {
    let test_cases = vec![
      ("let a = true", "let a = true"),
      ("let b = false", "let b = false"),
      ("false > true", "(false > true)"),
      ("false < true", "(false < true)"),
      ("true", "true"),
      ("false", "false"),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();

      assert!(!program.has_errors());

      assert_eq!(program.to_string(), expected);
    }
  }

  #[test]
  fn parse_return_statement() {
    let test_cases = vec![
      ("return 5", "return 5"),
      ("return 10", "return 10"),
      ("return 2 + 2", "return (2 + 2)"),
      ("return 2 * 4 + c", "return ((2 * 4) + c)"),
      ("return -a -a -a", "return (((- a) - a) - a)"),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();

      assert!(!program.has_errors());

      assert_eq!(program.to_string(), expected);
    }
  }

  #[test]
  fn parse_identifier_expression() {
    let test_cases = vec![("foobar", "foobar"), ("baz", "baz"), ("hello", "hello")];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();

      assert!(!program.has_errors());

      assert_eq!(program.to_string(), expected);
    }
  }

  #[test]
  fn parse_number_expression() {
    let test_cases = vec![("50", "50"), ("10", "10"), ("3", "3"), ("0", "0")];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();

      assert!(!program.has_errors());

      assert_eq!(program.to_string(), expected);
    }
  }

  #[test]
  fn parse_prefix_expressions() {
    let test_cases = vec![
      ("!5", "(! 5)"),
      ("-15", "(- 15)"),
      ("!-a", "(! (- a))"),
      ("-a * b", "((- a) * b)"),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();

      assert!(!program.has_errors());

      assert_eq!(program.to_string(), expected);
    }
  }

  #[test]
  fn parse_infix_expressions() {
    let test_cases = vec![
      ("5 + 5", "(5 + 5)"),
      ("5 - 5", "(5 - 5)"),
      ("5 * 5", "(5 * 5)"),
      ("5 / 5", "(5 / 5)"),
      ("5 > 5", "(5 > 5)"),
      ("5 < 5", "(5 < 5)"),
      ("5 == 5", "(5 == 5)"),
      ("5 != 5", "(5 != 5)"),
      ("5 <= 5", "(5 <= 5)"),
      ("5 >= 5", "(5 >= 5)"),
      ("x |> f", "(x |> f)"),
      ("a <= b |> f", "(a <= (b |> f))"),
      ("a |> f |> g", "((a |> f) |> g)"),
      ("a |> f > x", "((a |> f) > x)"),
      ("a + b + c", "((a + b) + c)"),
      ("a + b - c", "((a + b) - c)"),
      ("a * b * c", "((a * b) * c)"),
      ("a * b / c", "((a * b) / c)"),
      ("a + b / c", "(a + (b / c))"),
      ("a / b + c", "((a / b) + c)"),
      ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
      ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();

      dbg!(&program.errors);

      assert!(!program.has_errors());

      assert_eq!(program.to_string(), expected);
    }
  }

  #[test]
  fn parse_grouped_expressions() {
    let test_cases = vec![
      ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
      ("(5 + 5) * 2", "((5 + 5) * 2)"),
      ("2 / (5 + 5)", "(2 / (5 + 5))"),
      ("-(5 + 5)", "(- (5 + 5))"),
      ("!(true == true)", "(! (true == true))"),
      ("(2 * 2) + 2", "((2 * 2) + 2)"),
      ("2 + (2 * 2)", "(2 + (2 * 2))"),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();

      assert!(!program.has_errors());

      assert_eq!(program.to_string(), expected);
    }
  }

  #[test]
  fn parse_if_expression() {
    let test_cases = vec![
      ("if(x > 5) { a }", "(if ((x > 5)) { a })"),
      (
        "if(x > 5) { a } else { b }",
        "(if ((x > 5)) { a } else { b })",
      ),
      ("if (true) { 2 + 2 * 3 }", "(if (true) { (2 + (2 * 3)) })"),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();

      assert!(!program.has_errors());

      assert_eq!(program.to_string(), expected);
    }
  }

  #[test]
  fn parse_function_literal() {
    let test_cases = vec![
      ("fn(x, y) { x + y }", "(fn(x, y) { (x + y) })"),
      ("fn() { 2 + 2 * 4 }", "(fn() { (2 + (2 * 4)) })"),
      ("fn() {}", "(fn() {  })"),
      (
        "let double = fn(x) { x * 2 }",
        "let double = (fn(x) { (x * 2) })",
      ),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();

      assert!(!program.has_errors());

      assert_eq!(program.to_string(), expected);
    }
  }

  #[test]
  fn parse_call_expressions() {
    let test_cases = vec![
      ("add(1, 2 * 3, 4 + 5)", "(add(1, (2 * 3), (4 + 5)))"),
      ("a + add(b * c) + d", "((a + (add((b * c)))) + d)"),
      (
        "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
        "(add(a, b, 1, (2 * 3), (4 + 5), (add(6, (7 * 8)))))",
      ),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      let program = parser.parse();

      assert!(!program.has_errors());

      assert_eq!(program.to_string(), expected);
    }
  }
}
