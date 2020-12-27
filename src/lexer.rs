use crate::token::*;

#[derive(Debug)]
pub struct Lexer {
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

  fn skip_whitespace(&mut self) {
    while self.character.is_ascii_whitespace() {
      self.read_character();
    }
  }

  fn read_identifier(&mut self) -> String {
    let identifier_starts_at = self.position;

    while self.character.is_alphabetic() {
      self.read_character();
    }

    self
      .source_code
      .chars()
      .skip(identifier_starts_at)
      .take(self.position - identifier_starts_at)
      .collect()
  }

  fn read_number(&mut self) -> String {
    let number_starts_at = self.position;

    while self.character.is_digit(10) {
      self.read_character();
    }

    self
      .source_code
      .chars()
      .skip(number_starts_at)
      .take(self.position - number_starts_at)
      .collect()
  }

  pub fn next(&mut self) -> Token {
    self.skip_whitespace();

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
      character if character.is_alphabetic() => {
        let identifier = self.read_identifier();
        return lookup_identifier(identifier);
      }
      character if character.is_digit(10) => return Token::Number(self.read_number()),
      character => Token::Illegal(character),
    };

    self.read_character();

    token
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
    Token::Identifier(String::from("five")),
    Token::Assign,
    Token::Number(String::from("5")),
    Token::Semicolon,
    Token::Let,
    Token::Identifier(String::from("ten")),
    Token::Assign,
    Token::Number(String::from("10")),
    Token::Semicolon,
    Token::Let,
    Token::Identifier(String::from("add")),
    Token::Assign,
    Token::Function,
    Token::LeftParen,
    Token::Identifier(String::from("x")),
    Token::Comma,
    Token::Identifier(String::from("y")),
    Token::RightParen,
    Token::LeftBrace,
    Token::Identifier(String::from("x")),
    Token::Plus,
    Token::Identifier(String::from("y")),
    Token::Semicolon,
    Token::RightBrace,
    Token::Let,
    Token::Identifier(String::from("result")),
    Token::Assign,
    Token::Identifier(String::from("add")),
    Token::LeftParen,
    Token::Identifier(String::from("five")),
    Token::Comma,
    Token::Identifier(String::from("ten")),
    Token::RightParen,
    Token::Semicolon,
    Token::Eof,
  ];

  let mut lexer = Lexer::new(input);

  for token in tokens {
    let t = lexer.next();
    println!("token: {:?}, t: {:?}", token, t);
    assert_eq!(token, t)
  }
}
