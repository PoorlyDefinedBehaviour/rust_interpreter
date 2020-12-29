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

  fn peek_character(&self) -> char {
    if self.next_position >= self.source_code.len() {
      '\0'
    } else {
      self.source_code.chars().nth(self.next_position).unwrap()
    }
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

    if self.character == '.' && self.peek_character().is_digit(10) {
      self.read_character();

      while self.character.is_digit(10) {
        self.read_character();
      }
    }

    self
      .source_code
      .chars()
      .skip(number_starts_at)
      .take(self.position - number_starts_at)
      .collect()
  }

  fn next_character_is(&self, expected_character: char) -> bool {
    if self.next_position >= self.source_code.len() {
      return false;
    }

    let character = self.source_code.chars().nth(self.next_position).unwrap();

    character == expected_character
  }

  fn next_token(&mut self) -> Token {
    self.skip_whitespace();

    let token = match self.character {
      ';' => Token::Semicolon,
      '(' => Token::LeftParen,
      ')' => Token::RightParen,
      ',' => Token::Comma,
      '+' => Token::Plus,
      '-' => Token::Minus,
      '{' => Token::LeftBrace,
      '}' => Token::RightBrace,
      '*' => Token::Star,
      '/' => Token::Slash,
      '>' => Token::GreaterThan,
      '<' => Token::LessThan,
      '!' => {
        if self.next_character_is('=') {
          self.read_character();
          Token::NotEqual
        } else {
          Token::Bang
        }
      }
      '=' => {
        if self.next_character_is('=') {
          self.read_character();
          Token::Equal
        } else {
          Token::Assign
        }
      }
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

  #[test]
  fn single_character_tokens() {
    let test_cases: Vec<(&str, Vec<Token>)> = vec![
      ("=", vec![Token::Assign, Token::Eof]),
      (";", vec![Token::Semicolon, Token::Eof]),
      ("(", vec![Token::LeftParen, Token::Eof]),
      (")", vec![Token::RightParen, Token::Eof]),
      (",", vec![Token::Comma, Token::Eof]),
      ("+", vec![Token::Plus, Token::Eof]),
      ("-", vec![Token::Minus, Token::Eof]),
      ("!", vec![Token::Bang, Token::Eof]),
      ("{", vec![Token::LeftBrace, Token::Eof]),
      ("}", vec![Token::RightBrace, Token::Eof]),
      ("*", vec![Token::Star, Token::Eof]),
      ("/", vec![Token::Slash, Token::Eof]),
      (">", vec![Token::GreaterThan, Token::Eof]),
      ("<", vec![Token::LessThan, Token::Eof]),
    ];

    for (input, expected_tokens) in test_cases {
      let mut lexer = Lexer::new(String::from(input));

      assert_eq!(expected_tokens, lexer.lex());
    }
  }

  #[test]
  fn double_character_tokens() {
    let test_cases: Vec<(&str, Vec<Token>)> = vec![
      ("==", vec![Token::Equal, Token::Eof]),
      ("!=", vec![Token::NotEqual, Token::Eof]),
    ];

    for (input, expected_tokens) in test_cases {
      let mut lexer = Lexer::new(String::from(input));

      assert_eq!(expected_tokens, lexer.lex());
    }
  }

  #[test]
  fn identifiers() {
    let test_cases: Vec<(&str, Vec<Token>)> = vec![
      (
        "hello",
        vec![Token::Identifier(String::from("hello")), Token::Eof],
      ),
      (
        "foo",
        vec![Token::Identifier(String::from("foo")), Token::Eof],
      ),
      (
        "bar",
        vec![Token::Identifier(String::from("bar")), Token::Eof],
      ),
      ("x", vec![Token::Identifier(String::from("x")), Token::Eof]),
      ("y", vec![Token::Identifier(String::from("y")), Token::Eof]),
    ];

    for (input, expected_tokens) in test_cases {
      let mut lexer = Lexer::new(String::from(input));

      assert_eq!(expected_tokens, lexer.lex());
    }
  }

  #[test]
  fn keywords() {
    let test_cases: Vec<(&str, Vec<Token>)> = vec![
      ("return", vec![Token::Return, Token::Eof]),
      ("let", vec![Token::Let, Token::Eof]),
      ("fn", vec![Token::Function, Token::Eof]),
    ];

    for (input, expected_tokens) in test_cases {
      let mut lexer = Lexer::new(String::from(input));

      assert_eq!(expected_tokens, lexer.lex());
    }
  }

  #[test]
  fn numbers() {
    let test_cases: Vec<(&str, Vec<Token>)> = vec![
      ("10", vec![Token::Number(String::from("10")), Token::Eof]),
      ("0", vec![Token::Number(String::from("0")), Token::Eof]),
      (
        "4124421311",
        vec![Token::Number(String::from("4124421311")), Token::Eof],
      ),
      ("1.0", vec![Token::Number(String::from("1.0")), Token::Eof]),
      ("0.5", vec![Token::Number(String::from("0.5")), Token::Eof]),
      (
        "432342343.43",
        vec![Token::Number(String::from("432342343.43")), Token::Eof],
      ),
      (
        "-0.5",
        vec![Token::Minus, Token::Number(String::from("0.5")), Token::Eof],
      ),
      (
        "-0",
        vec![Token::Minus, Token::Number(String::from("0")), Token::Eof],
      ),
      (
        "-241249129414141241.512521521512",
        vec![
          Token::Minus,
          Token::Number(String::from("241249129414141241.512521521512")),
          Token::Eof,
        ],
      ),
      (
        "-59.42",
        vec![
          Token::Minus,
          Token::Number(String::from("59.42")),
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
