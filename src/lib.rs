// pub mod compiler;
// pub mod analyzer;
pub mod error;
pub mod ir;
pub mod parser;
pub mod tree;

// use analyzer::Analyzer;
use parser::Parser;

pub type Span = core::ops::Range<usize>;

use error::Error;

pub fn compile(file: String, contents: &str) -> Result<String, Vec<Error>> {
    let parser = Parser::new(contents);
    let mut parse = parser.parse();
    for error in &mut parse.errors {
        error.set_path(file.clone());
        eprintln!("{}", error);
    }
    let syntax = parse.output;
    println!("PARSE:\n{:#?}", syntax);

    // let analyzer = Analyzer::new(contents);
    // let tree = analyzer.analyze(&syntax);
    // println!("TREE:\n{:?}", tree);
    todo!()
}
