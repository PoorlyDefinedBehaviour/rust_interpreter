use crate::token::*;

#[derive(Debug, PartialEq, Clone)]
pub struct LexerError {
  line: usize,
  column: usize,
  message: String,
}

#[derive(Debug)]
pub struct Lexer {
  source_code: String,
  position: usize,
  next_position: usize,
  line: usize,
  column: usize,
  character: char,
  errors: Vec<LexerError>,
}

impl Lexer {
  pub fn new(source_code: String) -> Lexer {
    let mut lexer = Lexer {
      source_code,
      position: 0,
      next_position: 0,
      character: '\0',
      line: 1,
      column: 0,
      errors: Vec::new(),
    };

    lexer.read_character();

    lexer
  }

  pub fn lex(&mut self) -> Result<Vec<Token>, Vec<LexerError>> {
    let mut tokens = Vec::new();

    while self.has_characters_to_lex() {
      tokens.push(self.next_token());
    }

    if !self.errors.is_empty() {
      return Err(self.errors.clone());
    }

    Ok(tokens)
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

    if self.character != '\0' {
      self.column += 1;
    }

    if self.character == '\n' {
      self.line += 1;
      self.column = 0;
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

  fn error(&mut self, message: String) {
    self.errors.push(LexerError {
      line: self.line,
      column: self.column,
      message,
    });
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

  fn read_string(&mut self) -> String {
    let string_starts_at = self.position;

    self.read_character(); // advance past "

    while self.character != '"' && self.has_characters_to_lex() {
      self.read_character();
    }

    let string = self
      .source_code
      .chars()
      .skip(string_starts_at + 1)
      .take(self.position - string_starts_at - 1)
      .collect();

    if self.character != '"' {
      self.read_character(); // advance past "
      self.error(format!(r#"unterminated string: "{}"#, string))
    } else {
      self.read_character(); // advance past "
    }

    string
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
      '|' => {
        if self.next_character_is('>') {
          self.read_character();
          Token::Pipe
        } else {
          Token::Illegal(self.character)
        }
      }
      '>' => {
        if self.next_character_is('=') {
          self.read_character();
          Token::GreaterThanOrEqual
        } else {
          Token::GreaterThan
        }
      }
      '<' => {
        if self.next_character_is('=') {
          self.read_character();
          Token::LessThanOrEqual
        } else {
          Token::LessThan
        }
      }
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
      '"' => return Token::String(self.read_string()),
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
  fn keeps_track_of_line_and_column() {
    let test_cases: Vec<(&str, usize, usize)> = vec![
      ("", 1, 0),
      ("abc", 1, 3),
      ("", 1, 0),
      ("a 1 c", 1, 5),
      (
        "let a = 10
let b = 20",
        2,
        10,
      ),
    ];

    for (input, expected_line, expected_column) in test_cases {
      let mut lexer = Lexer::new(String::from(input));

      lexer.lex().ok();

      assert_eq!(lexer.line, expected_line);
      assert_eq!(lexer.column, expected_column);
    }
  }

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

      assert_eq!(expected_tokens, lexer.lex().unwrap());
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

      assert_eq!(expected_tokens, lexer.lex().unwrap());
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

      assert_eq!(expected_tokens, lexer.lex().unwrap());
    }
  }

  #[test]
  fn double_character_tokens() {
    let test_cases: Vec<(&str, Vec<Token>)> = vec![
      ("==", vec![Token::Equal, Token::Eof]),
      ("!=", vec![Token::NotEqual, Token::Eof]),
      (">=", vec![Token::GreaterThanOrEqual, Token::Eof]),
      ("<=", vec![Token::LessThanOrEqual, Token::Eof]),
      ("|>", vec![Token::Pipe, Token::Eof]),
    ];

    for (input, expected_tokens) in test_cases {
      let mut lexer = Lexer::new(String::from(input));

      assert_eq!(expected_tokens, lexer.lex().unwrap());
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

      assert_eq!(expected_tokens, lexer.lex().unwrap());
    }
  }

  #[test]
  fn keywords_and_special_values() {
    let test_cases: Vec<(&str, Vec<Token>)> = vec![
      ("return", vec![Token::Return, Token::Eof]),
      ("let", vec![Token::Let, Token::Eof]),
      ("fn", vec![Token::Function, Token::Eof]),
      ("true", vec![Token::True, Token::Eof]),
      ("false", vec![Token::False, Token::Eof]),
      ("if", vec![Token::If, Token::Eof]),
      ("else", vec![Token::Else, Token::Eof]),
      (
        "if(x > 3) {}",
        vec![
          Token::If,
          Token::LeftParen,
          Token::Identifier(String::from("x")),
          Token::GreaterThan,
          Token::Number(String::from("3")),
          Token::RightParen,
          Token::LeftBrace,
          Token::RightBrace,
          Token::Eof,
        ],
      ),
      (
        "if(x > 3) { a } else { b }",
        vec![
          Token::If,
          Token::LeftParen,
          Token::Identifier(String::from("x")),
          Token::GreaterThan,
          Token::Number(String::from("3")),
          Token::RightParen,
          Token::LeftBrace,
          Token::Identifier(String::from("a")),
          Token::RightBrace,
          Token::Else,
          Token::LeftBrace,
          Token::Identifier(String::from("b")),
          Token::RightBrace,
          Token::Eof,
        ],
      ),
    ];

    for (input, expected_tokens) in test_cases {
      let mut lexer = Lexer::new(String::from(input));

      assert_eq!(expected_tokens, lexer.lex().unwrap());
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

      assert_eq!(expected_tokens, lexer.lex().unwrap());
    }
  }

  #[test]
  fn strings() {
    let test_cases: Vec<(&str, Vec<Token>)> = vec![
      (
        r#""10""#,
        vec![Token::String(String::from("10")), Token::Eof],
      ),
      (
        r#""hello world""#,
        vec![Token::String(String::from("hello world")), Token::Eof],
      ),
      (
        r#""-421894124128""#,
        vec![Token::String(String::from("-421894124128")), Token::Eof],
      ),
      (
        r#""let f = fn(x) { f() }""#,
        vec![
          Token::String(String::from("let f = fn(x) { f() }")),
          Token::Eof,
        ],
      ),
    ];

    for (input, expected_tokens) in test_cases {
      let mut lexer = Lexer::new(String::from(input));

      assert_eq!(expected_tokens, lexer.lex().unwrap());
    }
  }

  #[test]
  fn lexer_errors() {
    let test_cases: Vec<(&str, Vec<LexerError>)> = vec![(
      r#""10"#,
      vec![LexerError {
        line: 1,
        column: 3,
        message: String::from(r#"unterminated string: "10"#),
      }],
    )];

    for (input, expected_errors) in test_cases {
      let mut lexer = Lexer::new(String::from(input));

      assert_eq!(Err(expected_errors), lexer.lex());
    }
  }
}
