use std::io;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::eval::{eval_statements, Environment};

const MONKEY: &str = "
           __,__
  .--.  .-\"     \"-.  .--.
 / .. \\/  .-. .-.  \\/ .. \\
| |  '|  /   Y   \\  |'  | |
| \\   \\  \\ 0 | 0 /  /   / |
 \\ '- ,\\.-\"\"\"\"\"\"\"-./, -' /
  ''-' /_   ^ ^   _\\ '-''
      |  \\._   _./  |
      \\   \\ '~' /   /
       '._ '-=-' _.'
          '-----'
";
const PROMPT: &str = ">> ";

pub fn start() {
    println!("{}", MONKEY);
    println!("Hello! This is the Monkey programming language");
    println!("Feel free to type in commands");
    let mut environment = Environment::new();
    loop {
        print!("{}", PROMPT);
        let mut buf = String::new();
        io::Write::flush(&mut io::stdout()).expect("flush failed!");  

        io::stdin().read_line(&mut buf).unwrap();

        let lexer = Lexer::new(buf);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        if program.errors.len() != 0 {
            for error in program.errors {
                println!("{}", error);
            }
        } else {
            match eval_statements(program.statements, &mut environment) {
                Ok(res) => println!("{}", res.inspect()),
                Err(err) => println!("{}", err)
            };
        }

    }
}
