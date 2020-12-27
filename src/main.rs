use interpreter::lexer::Lexer;
use interpreter::parser::Parser;

fn main() {
  let mut lexer = Lexer::new(String::from("let a = 10"));
  let tokens = lexer.lex();

  println!("tokens: {:?}", tokens);

  println!("statements: {:?}", Parser::new(tokens).parse());
}
