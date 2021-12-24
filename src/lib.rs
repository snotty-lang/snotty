mod ir_code;
mod lexer;
mod parser;
mod utils;

use std::fs;
use std::process;

pub fn run(filename: &str) {
    let contents = fs::read_to_string(filename).unwrap();

    let tokens = match lexer::Lexer::lex(&contents) {
        Ok(tokens) => tokens,
        Err(error) => {
            eprintln!("{}", error);
            process::exit(1);
        }
    };

    let ast = match parser::Parser::parse(tokens) {
        Ok(ast) => ast,
        Err(error) => {
            eprintln!("{}", error);
            process::exit(1);
        }
    };

    let code = ir_code::CodeGenerator::generate_code(ast);

    println!("{}", code);

    // let result = interpreter::Interpreter::visit(ast.unwrap());
    // println!("{:?}", result.unwrap());
}
