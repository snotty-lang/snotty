pub mod compiler;
pub mod error;
pub mod ir;
pub mod parser;

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
    println!("{:#?}", parse.syntax());

    // println!("{:?}\n", rules);
    // println!("{:?}\n", errors);
    // let compiled = CCompiler::compile(ir);
    // Ok(compiled)
    todo!()
}
