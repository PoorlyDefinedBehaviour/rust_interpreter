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
}

type PrefixParselet = fn(&mut Parser, Token) -> Expression;
type InfixParselet = fn(&mut Parser, Expression, Token) -> Expression;

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

    parser.prefix(
      std::mem::discriminant(&Token::Minus),
      Parser::parse_prefix_expression,
    );

    parser.prefix(
      std::mem::discriminant(&Token::Bang),
      Parser::parse_prefix_expression,
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

    parser
  }

  pub fn parse(&mut self) -> Vec<Statement> {
    let mut statements = Vec::new();

    loop {
      match self.current_token() {
        Some(Token::Eof) => return statements,
        Some(_) => match self.parse_statement() {
          Ok(statement) => statements.push(statement),
          Err(message) => self.errors.push(message),
        },
        None => return statements,
      }
    }
  }

  fn prefix(&mut self, token: std::mem::Discriminant<Token>, parselet: PrefixParselet) {
    self.prefix_parselets.insert(token, parselet);
  }

  fn infix(&mut self, token: std::mem::Discriminant<Token>, parselet: InfixParselet) {
    self.infix_parselets.insert(token, parselet);
  }

  fn current_token(&self) -> Option<&Token> {
    self.tokens.get(self.current_position)
  }

  fn has_tokens_to_parse(&self) -> bool {
    self.current_position < self.tokens.len() - 1
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

  fn current_token_precedence(&self) -> i32 {
    match self.current_token() {
      Some(Token::GreaterThan) | Some(Token::LessThan) => Precedence::COMPARISON,
      Some(Token::Assign) | Some(Token::NotEqual) => Precedence::EQUALITY,
      Some(Token::Plus) | Some(Token::Minus) => Precedence::TERM,
      Some(Token::Star) | Some(Token::Slash) => Precedence::FACTOR,
      _ => Precedence::NONE,
    }
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
        let mut left = prefix_parselet(self, token);

        loop {
          let current_token = self.current_token();

          if precedence > self.current_token_precedence()
            || current_token.is_none()
            || *current_token.unwrap() == Token::Semicolon
            || !self.has_tokens_to_parse()
          {
            return Ok(left);
          }

          token = self.next_token().cloned().unwrap();

          let infix_parselet = self
            .infix_parselets
            .get(&std::mem::discriminant(&token))
            .unwrap();

          left = infix_parselet(self, left, token);
        }
      }
    }
  }

  fn parse_prefix_expression(&mut self, token: Token) -> Expression {
    let operand = self.parse_expression_statement(Precedence::UNARY).unwrap();

    Expression::Prefix(PrefixExpression {
      operator: token,
      operand: Box::new(operand),
    })
  }

  fn parse_infix_expression(&mut self, left_operand: Expression, token: Token) -> Expression {
    let right_operand = self
      .parse_expression_statement(self.current_token_precedence())
      .unwrap();

    Expression::Infix(InfixExpression {
      left_operand: Box::new(left_operand),
      operator: token,
      right_operand: Box::new(right_operand),
    })
  }

  fn parse_identifier(&mut self, token: Token) -> Expression {
    match token {
      Token::Identifier(identifier) => Expression::Identifier(identifier),
      _ => unreachable!(),
    }
  }

  fn parse_number(&mut self, token: Token) -> Expression {
    match &token {
      Token::Number(number) => Expression::Number(number.parse::<f64>().unwrap()),
      _ => unreachable!(),
    }
  }

  fn parse_return_statement(&mut self) -> Result<Statement, String> {
    self.consume(Token::Return);

    while let Some(current_token) = self.current_token() {
      if *current_token == Token::Semicolon || *current_token == Token::Eof {
        break;
      }

      self.next_token();
    }

    self.consume(Token::Semicolon);

    Ok(Statement::Return(Expression::Identifier(String::from(
      "todo",
    ))))
  }

  fn parse_let_statement(&mut self) -> Result<Statement, String> {
    let token = self.next_token().cloned().unwrap();

    let identifier = self.consume_with_value(Token::Identifier);

    self.consume(Token::Assign);

    while let Some(current_token) = self.current_token() {
      if *current_token == Token::Semicolon || *current_token == Token::Eof {
        break;
      }

      self.next_token();
    }

    //let value = Expression(...)

    self.consume(Token::Semicolon);

    Ok(Statement::Let(LetStatement { token, identifier }))
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::Lexer;

  #[test]
  fn parse_let_statement() {
    let test_cases = vec![(
      "let x = 5;",
      vec![Statement::Let(LetStatement {
        token: Token::Let,
        identifier: Token::Identifier(String::from("x")),
      })],
    )];

    for (input, expected_statements) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      assert_eq!(parser.parse(), expected_statements);
    }
  }

  #[test]
  fn parse_let_statement_error() {
    let test_cases = vec![
      ("let x = 5", "expected Semicolon, got Eof"),
      ("let x 5;", "expected Assign, got Number(\"5\")"),
      ("let = 5;", "expected Identifier(\"_\"), got Assign"),
    ];

    for (input, expected_error) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      parser.parse();

      assert_eq!(parser.errors[0], expected_error);
    }
  }

  #[test]
  fn parse_return_statement() {
    let test_cases = vec![
      (
        "return 5;",
        Statement::Return(Expression::Identifier(String::from("todo"))),
      ),
      (
        "return 10;",
        Statement::Return(Expression::Identifier(String::from("todo"))),
      ),
    ];

    for (input, expected_statement) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      assert_eq!(parser.parse()[0], expected_statement);
    }
  }

  #[test]
  fn parse_return_statement_error() {
    let test_cases = vec![("return 5", "expected Semicolon, got Eof")];

    for (input, expected_error) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      parser.parse();

      assert_eq!(parser.errors[0], expected_error);
    }
  }

  #[test]
  fn parse_identifier_expression() {
    let test_cases = vec![
      ("foobar", "Identifier(\"foobar\")"),
      ("baz", "Identifier(\"baz\")"),
      ("hello", "Identifier(\"hello\")"),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      assert_eq!(parser.parse()[0].to_string(), expected);
    }
  }

  #[test]
  fn parse_number_expression() {
    let test_cases = vec![
      ("50", "Number(50.0)"),
      ("10", "Number(10.0)"),
      ("3", "Number(3.0)"),
      ("0", "Number(0.0)"),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      assert_eq!(parser.parse()[0].to_string(), expected);
    }
  }

  #[test]
  fn parse_prefix_expressions() {
    let test_cases = vec![
      ("!5", "(Bang Number(5.0))"),
      ("-15", "(Minus Number(15.0))"),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      assert_eq!(parser.parse()[0].to_string(), expected);
    }
  }

  #[test]
  fn parse_infix_expressions() {
    let test_cases = vec![
      ("5 + 5", "(Number(5.0) Plus Number(5.0))"),
      ("5 - 5", "(Number(5.0) Minus Number(5.0))"),
      ("5 * 5", "(Number(5.0) Star Number(5.0))"),
      ("5 / 5", "(Number(5.0) Slash Number(5.0))"),
      ("5 > 5", "(Number(5.0) GreaterThan Number(5.0))"),
      ("5 < 5", "(Number(5.0) LessThan Number(5.0))"),
    ];

    for (input, expected) in test_cases {
      let mut parser = Parser::new(Lexer::new(String::from(input)).lex());

      assert_eq!(parser.parse()[0].to_string(), expected);
    }
  }
}
