use crate::ir::IR;

// pub mod c;

pub trait Compiler {
    fn compile(ir: IR) -> String;
}
