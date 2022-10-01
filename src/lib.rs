use std::{error, fs};

use parser::{Error, Rule};

use crate::compiler::{c::CCompiler, Compiler};

extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod compiler;
pub mod parser;

pub fn run(file: &str) -> Result<String, Box<dyn error::Error>> {
    let contents = fs::read_to_string(file)?;
    let ir = parser::parse(&contents, file).map_err(|err| {
        if err.path().is_none() {
            format_error(err.with_path(file))
        } else {
            err
        }
    })?;
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
