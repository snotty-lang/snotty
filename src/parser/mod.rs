pub mod error;
pub mod scope;

use pest::Parser;

use crate::parser::scope::Scope;

#[derive(Parser)]
#[grammar = "src/parser/grammar.pest"]
struct EzParser;

pub fn parse<'a>(program: &'a str) -> Result<(), Box<dyn std::error::Error + 'a>> {
    let program = EzParser::parse(Rule::program, program)?;
    let mut scope = Scope::new();
    for code in program {
        println!("{}", code);
        scope.push(code)?;
    }
    println!("{:?}", scope.code());
    Ok(())
}
