pub mod analyzer;
pub mod instruction;
pub mod value;

use pest::{error::Error as PestError, Parser};

use crate::parser::analyzer::Analyzer;

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

    (S $span: expr => $message: expr) => {
        Err($crate::parser::Error::new_from_span(
            pest::error::ErrorVariant::CustomError { message: $message },
            $span,
        ))
    };

    (ES $span: expr => $message: expr) => {
        $crate::parser::Error::new_from_span(
            pest::error::ErrorVariant::CustomError { message: $message },
            $span,
        )
    };

    (RS $span: expr => $message: expr) => {
        return Err($crate::parser::Error::new_from_span(
            pest::error::ErrorVariant::CustomError { message: $message },
            $span,
        ))
    };
}

#[derive(Parser)]
#[grammar = "src/parser/grammar.pest"]
struct SnottyParser;

pub fn parse(program: &str) -> Result<(), Error> {
    let program = SnottyParser::parse(Rule::program, program)?;
    let mut scope = Analyzer::new();
    for code in program {
        // println!("{}", code);
        scope.push(code)?;
    }
    println!("{:?}", scope.code());
    Ok(())
}
