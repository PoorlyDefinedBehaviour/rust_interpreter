#[derive(Debug, PartialEq, Clone)]
pub enum Token {
  Illegal(char),
  Eof,
  Identifier(String),
  Number(String),
  Assign,
  Plus,
  Comma,
  Semicolon,
  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,
  Function,
  Let,
}

pub fn lookup_identifier(lexeme: String) -> Token {
  match lexeme.as_str() {
    "let" => Token::Let,
    "fn" => Token::Function,
    _ => Token::Identifier(lexeme),
  }
}
