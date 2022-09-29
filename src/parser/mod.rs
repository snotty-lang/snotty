pub mod scope;

use pest::{error::Error as PestError, Parser};

use crate::parser::scope::Scope;

pub type Error = PestError<Rule>;

#[macro_export]
macro_rules! error {
    ($pair: expr => $message: expr) => {
        Err($crate::parser::Error::new_from_span(
            pest::error::ErrorVariant::CustomError { message: $message },
            $pair.as_span(),
        ))
    };

    (E $pair: expr => $message: expr) => {
        $crate::parser::Error::new_from_span(
            pest::error::ErrorVariant::CustomError { message: $message },
            $pair.as_span(),
        )
    };

    (R $pair: expr => $message: expr) => {
        return Err($crate::parser::Error::new_from_span(
            pest::error::ErrorVariant::CustomError { message: $message },
            $pair.as_span(),
        ))
    };
}

#[derive(Parser)]
#[grammar = "src/parser/grammar.pest"]
struct EzParser;

pub fn parse(program: &str) -> Result<(), Error> {
    let program = EzParser::parse(Rule::program, program)?;
    let mut scope = Scope::new();
    for code in program {
        scope.push(code)?;
    }
    println!("{:?}", scope.code());
    Ok(())
}
