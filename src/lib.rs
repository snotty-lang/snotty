pub mod analyzer;
pub mod compiler;
pub mod error;
pub mod parser;
mod tree;

use analyzer::{builder::Analyzer, type_checker::TypeChecker};
use compiler::JIT;
use parser::Parser;

pub type Span = core::ops::Range<usize>;

use error::Error;

pub fn compile(file: String, source: &str) -> Result<String, Vec<Error>> {
    let parsed = Parser::new(source).parse();
    let analyzed = Analyzer::new(source, parsed).analyze();
    let mut checked = TypeChecker::new(source, analyzed).analyze();
    println!("{}", checked.analyzed.tree);
    match checked.errors.as_mut_slice() {
        [] => {
            let mut jit = JIT::new(source);
            match jit.compile(checked.analyzed) {
                Err(err) => eprintln!("{}", err),
                Ok(code) => {
                    let f = unsafe { std::mem::transmute::<_, fn() -> i64>(code) };
                    println!("Code ran: {}", f());
                }
            }
        }
        errors => {
            for error in errors {
                error.set_path(file.clone());
                eprintln!("{}", error);
            }
        }
    }

    todo!()
}
