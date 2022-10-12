pub mod compiler;
pub mod error;
pub mod ir;
pub mod parser;

use error::Error;
use parser::{ParseResult, Parser};

pub fn compile(file: String, contents: &str) -> Result<String, Vec<Error>> {
    let parser = Parser::new_snotty(contents);
    let ParseResult {
        rules, mut errors, ..
    } = parser.shift_reduce();
    for error in &mut errors {
        error.set_path(file.clone());
    }

    println!("{:?}\n", rules);
    println!("{:?}\n", errors);
    // let compiled = CCompiler::compile(ir);
    // Ok(compiled)
    todo!()
}
