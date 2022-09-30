use std::{error, fs};

use parser::Rule;

extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod compiler;
pub mod parser;

pub fn run(file: &str) -> Result<(), Box<dyn error::Error>> {
    let contents = fs::read_to_string(file)?;
    let ir = match parser::parse(&contents) {
        Ok(ir) => ir,
        Err(err) => {
            return Err(Box::new(err.with_path(file).renamed_rules(
                |rule| match rule {
                    Rule::binop_expr => String::from("binary operation"),
                    _ => format!("{:?}", rule),
                },
            )))
        }
    };
    println!("{:?}", ir.code);
    Ok(())
}
