use interpreter::lexer::Lexer;
use interpreter::parser::Parser;
use std::io::{self, Write};

fn main() {
  loop {
    print!("> ");

    io::stdout().flush().expect("flush failed");

    let mut buffer = String::new();

    io::stdin()
      .read_line(&mut buffer)
      .expect("unable to read input");

    let mut lexer = Lexer::new(buffer);

    let mut parser = Parser::new(lexer.lex());

    let program = parser.parse();

    if program.has_errors() {
      dbg!(program.errors);
    } else {
      dbg!(program.statements);
    }
  }
}
