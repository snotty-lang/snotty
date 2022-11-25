// pub mod compiler;
pub mod analyzer;
pub mod error;
pub mod ir;
pub mod parser;
pub mod tree;

use analyzer::Analyzer;
use parser::Parser;

pub type Span = core::ops::Range<usize>;

use error::Error;

pub fn compile(file: String, contents: &str) -> Result<String, Vec<Error>> {
    let parser = Parser::new(contents);
    let mut parsed = parser.parse();
    for error in &mut parsed.errors {
        error.set_path(file.clone());
        eprintln!("{}", error);
    }
    let parse = parsed.output;
    println!("\nPARSE:\n{:?}", parse);

    let analyzer = Analyzer::new(contents);
    let mut analyzed = analyzer.analyze(&parse);
    for error in &mut analyzed.errors {
        error.set_path(file.clone());
        eprintln!("{}", error);
    }
    let analysis = analyzed.output;
    println!("\nTREE:\n{}", analysis);
    todo!()
}
