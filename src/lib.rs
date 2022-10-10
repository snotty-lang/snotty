use parser::{Error, Rule};

use crate::compiler::{c::CCompiler, Compiler};

extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod compiler;
pub mod parser;

pub fn run(file: String, contents: &str) -> Result<String, Vec<Error>> {
    let ir = parser::parse(contents, file)?;
    println!("{:?}\n", ir.fxs);
    println!("{:?}\n", ir.code);
    let compiled = CCompiler::compile(ir);
    Ok(compiled)
}

fn format_error(err: Error) -> Error {
    err.renamed_rules(|rule| match rule {
        Rule::binop_expr => String::from("binary operation"),
        _ => format!("{:?}", rule),
    })
}
