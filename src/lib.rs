use std::ops::{Deref, DerefMut};

pub mod compiler;
pub mod error;
pub mod ir;
pub mod parser;

pub type Span = core::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

impl<T> Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

use error::Error;
// use parser::{ParseResult, Parser};

pub fn compile(file: String, contents: &str) -> Result<String, Vec<Error>> {
    // let parser = Parser::new_snotty(contents);
    // let ParseResult {
    //     rules, mut errors, ..
    // } = parser.shift_reduce();
    // for error in &mut errors {
    //     error.set_path(file.clone());
    // }

    // println!("{:?}\n", rules);
    // println!("{:?}\n", errors);
    // let compiled = CCompiler::compile(ir);
    // Ok(compiled)
    todo!()
}
