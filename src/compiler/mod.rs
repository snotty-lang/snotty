use crate::parser::IR;

pub mod py;

pub trait Compiler {
    fn compile(ir: IR) -> String;
}
