use interpreter::lexer::Lexer;

fn main() {
  let mut lexer = Lexer::new(String::from("let a = 10"));
  println!("{:?}", lexer.next());
}
