use crate::compiler::Compiler;
use crate::parser::instruction::Instruction;
use crate::parser::value::{BaseValue, Value};
use crate::parser::IR;

const HEADER: &str = "\
#include <stdio.h>
#include <stdint.h>
#include <termios.h>
#define OUT(ch) printf(\"%c\", ch)
#define DEBUG(len) for (int i = 0; i < len; i++) printf(\"%d \", memory[i]); OUT(10);

int in() {
    struct termios old, new;
    int ch;
    tcgetattr(0, &old);
    new = old;
    new.c_lflag &= ~ICANON;
    new.c_lflag &= ~ECHO;
    tcsetattr(0, TCSANOW, &new);
    ch = getchar();
    tcsetattr(0, TCSANOW, &old);
    return ch;
}

";

fn to_c(v: &Value) -> String {
    match &v.value {
        BaseValue::Byte(b) => b.to_string(),
        BaseValue::Function(b, ..) => b.to_string(),
        BaseValue::None => String::from("0 /* NULL */"),
        BaseValue::Ref(v) => to_c(v),
        BaseValue::Pointer(i) => format!("{i} + start"),
        BaseValue::Memory(i) => format!("memory[{i} + start]"),
        BaseValue::DataBox(_) => todo!(),
    }
}

pub struct CCompiler;

impl Compiler for CCompiler {
    fn compile(ir: IR) -> String {
        let IR {
            memory_used,
            code,
            fxs,
        } = ir;
        let mut compiled = HEADER.to_string();

        compiled.push_str(
            "void fx(uint8_t *memory, uint8_t start, uint8_t fx_code) {\n    switch (fx_code) {\n",
        );
        for (i, fx) in fxs.into_iter().enumerate() {
            compiled.push_str(&format!("        case {i}:\n"));
            for instruction in fx {
                compiled.push_str("            ");
                compile_instruction(&instruction, &mut compiled);
            }
            compiled.push_str("            break;\n\n");
        }
        compiled.push_str("    }\n}\n\n");

        compiled.push_str(&format!(
            "int main() {{\n    uint8_t memory[{memory_used}0];\n    uint8_t start = 0;\n"
        ));
        for instruction in code {
            compiled.push_str("    ");
            compile_instruction(&instruction, &mut compiled);
        }
        compiled.push_str("}\n");

        compiled
    }
}

fn compile_instruction(instruction: &Instruction, compiled: &mut String) {
    match instruction {
        Instruction::Deref(v, i) => {
            compiled.push_str(&format!("memory[{i} + start] = memory[{}];\n", to_c(v)));
        }
        Instruction::DerefAssign(v, d, a) => compiled.push_str(&format!(
            "{}{}{} = {};\n",
            "memory[".repeat(*d),
            to_c(v),
            "]".repeat(*d),
            to_c(a)
        )),
        Instruction::While(v) => compiled.push_str(&format!("while ({}) {{\n", to_c(v))),
        Instruction::EndWhile(_) => compiled.push_str("}\n"),
        Instruction::If(v, _, _) => compiled.push_str(&format!("if ({}) {{\n", to_c(v))),
        Instruction::Else(_) => compiled.push_str("} else {\n"),
        Instruction::EndIf(_, _) => compiled.push_str("}\n"),
        Instruction::TernaryIf(v, a, b, i) => compiled.push_str(&format!(
            "if ({}) {{ memory[{i} + start] = {}; }} else {{ memory[{i} + start] = {}; }}\n",
            to_c(v),
            to_c(a),
            to_c(b)
        )),
        Instruction::Copy(v, i) => {
            compiled.push_str(&format!("memory[{i} + start] = {};\n", to_c(v)));
        }
        Instruction::Clear(_, _) => compiled.push_str("// clear\n"),
        Instruction::Input(i) => compiled.push_str(&format!("memory[{i} + start] = in();\n")),
        Instruction::Add(a, b, i) => compiled.push_str(&format!(
            "memory[{i} + start] = {} + {};\n",
            to_c(a),
            to_c(b)
        )),
        Instruction::Sub(a, b, i) => compiled.push_str(&format!(
            "memory[{i} + start] = {} - {};\n",
            to_c(a),
            to_c(b)
        )),
        Instruction::Mul(a, b, i) => compiled.push_str(&format!(
            "memory[{i} + start] = {} * {};\n",
            to_c(a),
            to_c(b)
        )),
        Instruction::Div(a, b, i) => compiled.push_str(&format!(
            "memory[{i} + start] = {} / {};\n",
            to_c(a),
            to_c(b)
        )),
        Instruction::Mod(a, b, i) => compiled.push_str(&format!(
            "memory[{i} + start] = {} % {};\n",
            to_c(a),
            to_c(b)
        )),
        Instruction::Neg(v, i) => {
            compiled.push_str(&format!("memory[{i} + start] = -{};\n", to_c(v)))
        }
        Instruction::Out(v) => compiled.push_str(&format!("OUT({});\n", to_c(v))),
        Instruction::Eq(a, b, i) => compiled.push_str(&format!(
            "memory[{i} + start] = {} == {};\n",
            to_c(a),
            to_c(b)
        )),
        Instruction::Neq(a, b, i) => compiled.push_str(&format!(
            "memory[{i} + start] = {} != {};\n",
            to_c(a),
            to_c(b)
        )),
        Instruction::Lt(a, b, i) => compiled.push_str(&format!(
            "memory[{i} + start] = {} < {};\n",
            to_c(a),
            to_c(b)
        )),
        Instruction::Le(a, b, i) => compiled.push_str(&format!(
            "memory[{i} + start] = {} <= {};\n",
            to_c(a),
            to_c(b)
        )),
        Instruction::Gt(a, b, i) => compiled.push_str(&format!(
            "memory[{i} + start] = {} > {};\n",
            to_c(a),
            to_c(b)
        )),
        Instruction::Ge(a, b, i) => compiled.push_str(&format!(
            "memory[{i} + start] = {} >= {};\n",
            to_c(a),
            to_c(b)
        )),
        Instruction::Inc(v) => compiled.push_str(&format!("++{};\n", to_c(v))),
        Instruction::Dec(v) => compiled.push_str(&format!("--{};\n", to_c(v))),
        Instruction::Shl(a, b, i) => compiled.push_str(&format!(
            "memory[{i} + start] = {} >> {};\n",
            to_c(a),
            to_c(b)
        )),
        Instruction::Shr(a, b, i) => compiled.push_str(&format!(
            "memory[{i} + start] = {} << {};\n",
            to_c(a),
            to_c(b)
        )),
        Instruction::And(a, b, i) => compiled.push_str(&format!(
            "memory[{i} + start] = {} & {};\n",
            to_c(a),
            to_c(b)
        )),
        Instruction::Or(a, b, i) => compiled.push_str(&format!(
            "memory[{i} + start] = {} | {};\n",
            to_c(a),
            to_c(b)
        )),
        Instruction::Not(v, i) => {
            compiled.push_str(&format!("memory[{i} + start] = ~{};\n", to_c(v)))
        }
        Instruction::Xor(a, b, i) => compiled.push_str(&format!(
            "memory[{i} + start] = {} ^ {};\n",
            to_c(a),
            to_c(b)
        )),
        Instruction::Call(fx, mem) => compiled.push_str(&format!(
            "fx(memory, {mem} + start, memory[{fx} + start]);\n"
        )),
    }
}
