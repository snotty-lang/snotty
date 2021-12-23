mod lexer;
mod parser;
mod utils;

use std::fs;

pub fn run(filename: &str) {
    let tokens = lexer::Lexer::lex(&fs::read_to_string(filename).unwrap());
    // println!("{:#?}", tokens);
    let tokens = tokens.unwrap();
    let ast = parser::Parser::parse(tokens);
    match &ast {
        Ok(ast) => {
            println!("{:#?}", ast);
        }
        Err(error) => {
            println!("{}", error);
        }
    }
    // let ast = ast.unwrap();
    // let result = interpreter::Interpreter::visit(ast.unwrap());
    // println!("{:?}", result.unwrap());
}
