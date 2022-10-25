pub mod compiler;
pub mod error;
pub mod ir;
pub mod parser;

use parser::Parser;

pub type Span = core::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> From<(T, Span)> for Spanned<T> {
    fn from(item: (T, Span)) -> Self {
        Self {
            span: item.1,
            value: item.0,
        }
    }
}

use error::Error;
// use parser::{ParseResult, Parser};

pub fn compile(file: String, contents: &str) -> Result<String, Vec<Error>> {
    let parser = Parser::new(contents);
    let mut parse = parser.parse();
    for error in &mut parse.errors {
        error.set_path(file.clone());
        eprintln!("{}", error);
    }
    println!("{:#?}", parse.syntax());

    // println!("{:?}\n", rules);
    // println!("{:?}\n", errors);
    // let compiled = CCompiler::compile(ir);
    // Ok(compiled)
    todo!()
}
