pub mod analyzer;
pub mod error;
pub mod parser;
mod tree;

use analyzer::{analyzed_builder::Analyzer, type_checker::TypeChecker};
use parser::Parser;

pub type Span = core::ops::Range<usize>;

use error::Error;

pub fn compile(file: String, source: &str) -> Result<String, Vec<Error>> {
    let parsed = Parser::new(source).parse();
    let analyzed = Analyzer::new(source, parsed).analyze();
    let mut checked = TypeChecker::new(source, analyzed).analyze();
    for error in &mut checked.errors {
        error.set_path(file.clone());
        eprintln!("{}", error);
    }
    println!("\nTREE:\n{}", checked.analyzed.tree);
    todo!()
}
