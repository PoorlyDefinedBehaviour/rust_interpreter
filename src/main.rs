use interpreter::interpreter::Interpreter;
use interpreter::lexer::Lexer;
use interpreter::parser::Parser;
use std::io::{self, Write};

fn main() {
  let mut interpreter = Interpreter::new();

  loop {
    print!("> ");

    io::stdout().flush().expect("flush failed");

    let mut buffer = String::new();

    io::stdin()
      .read_line(&mut buffer)
      .expect("unable to read input");

    let mut lexer = Lexer::new(buffer);

    match lexer.lex() {
      Err(errors) => {
        dbg!(errors);
      }
      Ok(tokens) => {
        let mut parser = Parser::new(tokens);

        let program = parser.parse();

        if program.has_errors() {
          dbg!(&program.errors);
        } else {
          dbg!(&program.statements);
          match interpreter.evaluate(program) {
            Ok(result) => {
              println!("{}", result);
            }
            Err(message) => {
              dbg!(message);
            }
          }
        }
      }
    }
  }
}
