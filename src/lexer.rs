use crate::token::*;

#[derive(Debug)]
pub(crate) struct Lexer {
  source_code: String,
  position: usize,
  next_position: usize,
  character: char,
}

impl Lexer {
  pub fn new(source_code: String) -> Lexer {
    let mut lexer = Lexer {
      source_code,
      position: 0,
      next_position: 0,
      character: '\0',
    };

    lexer.read_character();

    lexer
  }

  fn read_character(&mut self) {
    if self.next_position >= self.source_code.len() {
      self.character = '\0';
    } else {
      self.character = self.source_code.chars().nth(self.next_position).unwrap();
    }

    self.position = self.next_position;

    self.next_position += 1;
  }

  pub fn next(&mut self) -> Token {
    let token = match self.character {
      '=' => Token::Assign,
      ';' => Token::Semicolon,
      '(' => Token::LeftParen,
      ')' => Token::RightParen,
      ',' => Token::Comma,
      '+' => Token::Plus,
      '{' => Token::LeftBrace,
      '}' => Token::RightBrace,
      '\0' => Token::Eof,
      character if character.is_alphabetic() => self.read_identifier(),
      character => Token::Illegal(character),
    };

    self.read_character();

    token
  }

  fn read_identifier(&mut self) -> Token {
    let identifierStartsAt = self.position;

    while self.character.is_alphabetic() {
      self.read_character();
    }

    Token::Identifier(&self.source_code[identifierStartsAt..self.position])
  }
}

#[test]
fn next_token() {
  let input = String::from(
    "
    let five = 5;
    let ten = 10;
    let add = fn(x, y) {
      x + y;
    }

    let result = add(five, ten);
  ",
  );

  let tokens = vec![
    Token::Let,
    Token::Identifier("five"),
    Token::Assign,
    Token::Int("5"),
    Token::Semicolon,
    Token::Let,
    Token::Identifier("ten"),
    Token::Assign,
    Token::Int("10"),
    Token::Semicolon,
    Token::Let,
    Token::Identifier("add"),
    Token::Assign,
    Token::Function,
    Token::LeftParen,
    Token::Identifier("x"),
    Token::Comma,
    Token::Identifier("y"),
    Token::RightParen,
    Token::LeftBrace,
    Token::Identifier("x"),
    Token::Plus,
    Token::Identifier("y"),
    Token::Semicolon,
    Token::RightBrace,
    Token::Semicolon,
    Token::Let,
    Token::Identifier("result"),
    Token::Assign,
    Token::Identifier("add"),
    Token::LeftParen,
    Token::Identifier("five"),
    Token::Comma,
    Token::Identifier("ten"),
    Token::RightParen,
    Token::Semicolon,
    Token::Eof,
  ];

  let mut lexer = Lexer::new(input);

  for token in tokens {
    assert_eq!(token, lexer.next())
  }
}
