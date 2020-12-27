use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::*;

pub struct Parser {
  current_position: usize,
  tokens: Vec<Token>,
  errors: Vec<String>,
}

impl Parser {
  pub fn new(tokens: Vec<Token>) -> Self {
    let parser = Parser {
      current_position: 0,
      errors: vec![],
      tokens,
    };

    parser
  }

  pub fn parse(&mut self) -> Vec<Statement> {
    let mut statements = Vec::new();

    loop {
      match self.current_token() {
        Some(Token::Eof) => return statements,
        Some(_) => statements.push(self.parse_statement()),
        None => return statements,
      }
    }
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

  fn parse_statement(&mut self) -> Statement {
    match self.current_token() {
      Some(Token::Let) => self.parse_let_statement(),
      Some(statement) => panic!("unexpected statement: {:?}", statement),
      None => panic!("no tokens left to parse"),
    }
  }

  fn parse_let_statement(&mut self) -> Statement {
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

    Statement::Let(LetStatement { token, identifier })
  }
}

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
