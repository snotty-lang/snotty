use crate::{compiler::Compiler, parser::{IR, instruction::Instruction}};

pub struct PythonCompiler;

impl Compiler for PythonCompiler {
    fn compile(ir: IR) -> String {
        let mut compiled = PythonCode::new();
        let IR { code, .. } = ir;
        compiled.push("memory = []");
        for instruction in &code {
            match instruction {
                Instruction::Deref(_, _) => todo!(),
                Instruction::DerefAssign(_, _, _) => todo!(),
                Instruction::While(_) => todo!(),
                Instruction::EndWhile(_) => todo!(),
                Instruction::If(_, _, _) => todo!(),
                Instruction::Else(_) => todo!(),
                Instruction::EndIf(_, _) => todo!(),
                Instruction::TernaryIf(_, _, _, _) => todo!(),
                Instruction::Copy(_, _) => todo!(),
                Instruction::Clear(_, _) => todo!(),
                Instruction::Input(_) => todo!(),
                Instruction::Add(_, _, _) => todo!(),
                Instruction::Sub(_, _, _) => todo!(),
                Instruction::Mul(_, _, _) => todo!(),
                Instruction::Div(_, _, _) => todo!(),
                Instruction::Mod(_, _, _) => todo!(),
                Instruction::Neg(_, _) => todo!(),
                Instruction::Out(_) => todo!(),
                Instruction::Eq(_, _, _) => todo!(),
                Instruction::Neq(_, _, _) => todo!(),
                Instruction::Lt(_, _, _) => todo!(),
                Instruction::Le(_, _, _) => todo!(),
                Instruction::Gt(_, _, _) => todo!(),
                Instruction::Ge(_, _, _) => todo!(),
                Instruction::Inc(_) => todo!(),
                Instruction::Dec(_) => todo!(),
                Instruction::Pow(_, _, _) => todo!(),
                Instruction::Shl(_, _, _) => todo!(),
                Instruction::Shr(_, _, _) => todo!(),
                Instruction::And(_, _, _) => todo!(),
                Instruction::Or(_, _, _) => todo!(),
                Instruction::Not(_, _) => todo!(),
                Instruction::Xor(_, _, _) => todo!(),
                Instruction::Ref(_, _) => todo!(),
            }
        }
        compiled.finalized()
    }
}

struct PythonCode {
    code: String,
    indent: usize,
}

impl PythonCode {
    fn new() -> PythonCode {
        PythonCode { code: String::from("def main():\n"), indent: 0, }
    }

    fn push(&mut self, code: &str) {
        self.code.push_str(&"\t".repeat(self.indent));
        self.code.push_str(code);
        self.code.push('\n');
    }

    fn finalized(self) -> String {
        self.code
    }
}
