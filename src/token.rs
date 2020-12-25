#[derive(Debug, PartialEq)]
pub(crate) enum Token<'a> {
  Illegal(char),
  Eof,
  Identifier(&'a str),
  Int(&'a str),
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
