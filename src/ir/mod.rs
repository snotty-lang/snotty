pub mod ir;

use ir::{HIRArgument, HIRCode, Tuple};

use crate::analyzer::Analyzed;

pub struct IRGenerator {
    instructions: Vec<Tuple<HIRCode, HIRArgument>>,
}

impl IRGenerator {
    fn push1(&mut self, code: HIRCode, argument: [HIRArgument; 1]) {
        self.instructions.push(Tuple::new(code).push(argument[0]));
    }

    fn push2(&mut self, code: HIRCode, argument: [HIRArgument; 2]) {
        self.instructions
            .push(Tuple::new(code).push(argument[0]).push(argument[1]));
    }

    fn push3(&mut self, code: HIRCode, argument: [HIRArgument; 3]) {
        self.instructions.push(
            Tuple::new(code)
                .push(argument[0])
                .push(argument[1])
                .push(argument[2]),
        );
    }

    pub fn generate(&mut self, analyzed: Analyzed) {
        todo!()
    }
}
