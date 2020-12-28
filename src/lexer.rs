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

  pub fn lex(&mut self) -> Vec<Token> {
    let mut tokens = Vec::new();

    while self.has_characters_to_lex() {
      tokens.push(self.next_token());
    }

    tokens
  }

  fn has_characters_to_lex(&self) -> bool {
    self.position <= self.source_code.len()
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

  fn next_token(&mut self) -> Token {
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

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn let_statements() {
    let test_cases: Vec<(&str, Vec<Token>)> = vec![
      (
        "let five = 5;",
        vec![
          Token::Let,
          Token::Identifier(String::from("five")),
          Token::Assign,
          Token::Number(String::from("5")),
          Token::Semicolon,
          Token::Eof,
        ],
      ),
      (
        "let ten = 10;",
        vec![
          Token::Let,
          Token::Identifier(String::from("ten")),
          Token::Assign,
          Token::Number(String::from("10")),
          Token::Semicolon,
          Token::Eof,
        ],
      ),
    ];

    for (input, expected_tokens) in test_cases {
      let mut lexer = Lexer::new(String::from(input));

      assert_eq!(expected_tokens, lexer.lex());
    }
  }

  #[test]
  fn returns_illegal_token_for_illegal_characters() {
    let test_cases: Vec<(&str, Vec<Token>)> = vec![
      ("let ?", vec![Token::Let, Token::Illegal('?'), Token::Eof]),
      ("@", vec![Token::Illegal('@'), Token::Eof]),
      (
        "@@@",
        vec![
          Token::Illegal('@'),
          Token::Illegal('@'),
          Token::Illegal('@'),
          Token::Eof,
        ],
      ),
    ];

    for (input, expected_tokens) in test_cases {
      let mut lexer = Lexer::new(String::from(input));

      assert_eq!(expected_tokens, lexer.lex());
    }
  }
}
