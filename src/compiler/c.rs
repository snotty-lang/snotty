use crate::compiler::Compiler;
use crate::parser::instruction::Instruction;
use crate::parser::value::Value;
use crate::parser::{Memory, IR};

fn get_header(memory_used: Memory) -> String {
    format!(
        r#"#include <stdio.h>
#include <stdint.h>
#include <termios.h>
#define OUT(ch) printf("%c", ch)
#define MEMORY {}

int in() {{
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
}}

int main() {{
    uint8_t memory[MEMORY];
"#,
        memory_used
    )
}

fn to_c(v: &Value) -> String {
    match v {
        Value::Byte(b) => b.to_string(),
        Value::None => String::from("NULL"),
        Value::Ref(v) => to_c(v),
        Value::Pointer(i, _) => i.to_string(),
        Value::Memory(i, _) => format!("memory[{i}]"),
        Value::Function(..) => todo!(),
        Value::DataBox(_, _) => todo!(),
    }
}

pub struct CCompiler;

impl Compiler for CCompiler {
    fn compile(ir: IR) -> String {
        let IR {
            memory_used, code, ..
        } = ir;
        let mut compiled = get_header(memory_used);

        for instruction in code {
            match instruction {
                Instruction::Deref(v, i) => {
                    compiled.push_str(&format!("    memory[{i}] = memory[{}];\n", to_c(&v)));
                }
                Instruction::DerefAssign(_, _, _) => todo!(),
                Instruction::While(v) => {
                    compiled.push_str(&format!("    while ({}) {{\n", to_c(&v)))
                }
                Instruction::EndWhile(_) => compiled.push_str("    }\n"),
                Instruction::If(v, _, _) => {
                    compiled.push_str(&format!("    if ({}) {{\n", to_c(&v)))
                }
                Instruction::Else(_) => compiled.push_str("    } else {\n"),
                Instruction::EndIf(_, _) => compiled.push_str("    }\n"),
                Instruction::TernaryIf(v, a, b, i) => compiled.push_str(&format!(
                    "    if ({}) {{ memory[{i}] = {}; }} else {{ memory[{i}] = {}; }}\n",
                    to_c(&v),
                    to_c(&a),
                    to_c(&b)
                )),
                Instruction::Copy(v, i) => {
                    compiled.push_str(&format!("    memory[{i}] = {};\n", to_c(&v)));
                }
                Instruction::Clear(_, _) => (),
                Instruction::Input(i) => compiled.push_str(&format!("    memory[{i}] = in();\n")),
                Instruction::Add(a, b, i) => {
                    compiled.push_str(&format!("    memory[{i}] = {} + {};\n", to_c(&a), to_c(&b)))
                }
                Instruction::Sub(a, b, i) => {
                    compiled.push_str(&format!("    memory[{i}] = {} - {};\n", to_c(&a), to_c(&b)))
                }
                Instruction::Mul(a, b, i) => {
                    compiled.push_str(&format!("    memory[{i}] = {} * {};\n", to_c(&a), to_c(&b)))
                }
                Instruction::Div(a, b, i) => {
                    compiled.push_str(&format!("    memory[{i}] = {} / {};\n", to_c(&a), to_c(&b)))
                }
                Instruction::Mod(a, b, i) => {
                    compiled.push_str(&format!("    memory[{i}] = {} % {};\n", to_c(&a), to_c(&b)))
                }
                Instruction::Neg(v, i) => {
                    compiled.push_str(&format!("    memory[{i}] = -{};\n", to_c(&v)))
                }
                Instruction::Out(v) => compiled.push_str(&format!("    OUT({});\n", to_c(&v))),
                Instruction::Eq(a, b, i) => compiled.push_str(&format!(
                    "    memory[{i}] = {} == {};\n",
                    to_c(&a),
                    to_c(&b)
                )),
                Instruction::Neq(a, b, i) => compiled.push_str(&format!(
                    "    memory[{i}] = {} != {};\n",
                    to_c(&a),
                    to_c(&b)
                )),
                Instruction::Lt(a, b, i) => {
                    compiled.push_str(&format!("    memory[{i}] = {} < {};\n", to_c(&a), to_c(&b)))
                }
                Instruction::Le(a, b, i) => compiled.push_str(&format!(
                    "    memory[{i}] = {} <= {};\n",
                    to_c(&a),
                    to_c(&b)
                )),
                Instruction::Gt(a, b, i) => {
                    compiled.push_str(&format!("    memory[{i}] = {} > {};\n", to_c(&a), to_c(&b)))
                }
                Instruction::Ge(a, b, i) => compiled.push_str(&format!(
                    "    memory[{i}] = {} >= {};\n",
                    to_c(&a),
                    to_c(&b)
                )),
                Instruction::Inc(v) => compiled.push_str(&format!("    ++{};\n", to_c(&v))),
                Instruction::Dec(v) => compiled.push_str(&format!("    --{};\n", to_c(&v))),
                Instruction::Shl(a, b, i) => compiled.push_str(&format!(
                    "    memory[{i}] = {} >> {};\n",
                    to_c(&a),
                    to_c(&b)
                )),
                Instruction::Shr(a, b, i) => compiled.push_str(&format!(
                    "    memory[{i}] = {} << {};\n",
                    to_c(&a),
                    to_c(&b)
                )),
                Instruction::And(a, b, i) => {
                    compiled.push_str(&format!("    memory[{i}] = {} & {};\n", to_c(&a), to_c(&b)))
                }
                Instruction::Or(a, b, i) => {
                    compiled.push_str(&format!("    memory[{i}] = {} | {};\n", to_c(&a), to_c(&b)))
                }
                Instruction::Not(v, i) => {
                    compiled.push_str(&format!("    memory[{i}] = ~{};\n", to_c(&v)))
                }
                Instruction::Xor(a, b, i) => {
                    compiled.push_str(&format!("    memory[{i}] = {} ^ {};\n", to_c(&a), to_c(&b)))
                }
            }
        }

        compiled.push('}');

        compiled
    }
}
