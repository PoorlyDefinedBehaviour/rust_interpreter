use std::fmt;

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Token {
  Illegal(char),
  Eof,
  Identifier(String),
  Number(String),
  Assign,
  Plus,
  Minus,
  Star,
  Slash,
  GreaterThan,
  LessThan,
  Equal,
  NotEqual,
  Bang,
  Comma,
  Semicolon,
  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,
  Function,
  Let,
  Return,
}

impl fmt::Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Token::Illegal(char) => write!(f, "{}", char),
      Token::Eof => write!(f, "EOF"),
      Token::Identifier(identifier) => write!(f, "{}", identifier),
      Token::Number(number) => write!(f, "{}", number),
      Token::Assign => write!(f, "="),
      Token::Plus => write!(f, "+"),
      Token::Minus => write!(f, "-"),
      Token::Star => write!(f, "*"),
      Token::Slash => write!(f, "/"),
      Token::GreaterThan => write!(f, ">"),
      Token::LessThan => write!(f, "<"),
      Token::Equal => write!(f, "=="),
      Token::NotEqual => write!(f, "!="),
      Token::Bang => write!(f, "!"),
      Token::Comma => write!(f, ","),
      Token::Semicolon => write!(f, ";"),
      Token::LeftParen => write!(f, "("),
      Token::RightParen => write!(f, ")"),
      Token::LeftBrace => write!(f, "{{"),
      Token::RightBrace => write!(f, "}}"),
      Token::Function => write!(f, "fn()"),
      Token::Let => write!(f, "let"),
      Token::Return => write!(f, "return"),
    }
  }
}

pub fn lookup_identifier(lexeme: String) -> Token {
  match lexeme.as_str() {
    "let" => Token::Let,
    "fn" => Token::Function,
    "return" => Token::Return,
    _ => Token::Identifier(lexeme),
  }
}
