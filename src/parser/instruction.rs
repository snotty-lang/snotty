use crate::parser::{value::Value, Memory};

#[derive(Debug, Clone)]
pub enum Instruction {
    Deref(Value, Memory),
    DerefAssign(Value, usize, Value),
    While(Value),
    EndWhile(Value),
    If(Value, usize, bool),
    Else(usize),
    EndIf(usize, bool),
    TernaryIf(Value, Value, Value, Memory),
    Copy(Value, Memory),
    Clear(Memory, Memory),
    Input(Memory),
    Add(Value, Value, Memory),
    Sub(Value, Value, Memory),
    Mul(Value, Value, Memory),
    Div(Value, Value, Memory),
    Mod(Value, Value, Memory),
    Neg(Value, Memory),
    Out(Value),
    Eq(Value, Value, Memory),
    Neq(Value, Value, Memory),
    Lt(Value, Value, Memory),
    Le(Value, Value, Memory),
    Gt(Value, Value, Memory),
    Ge(Value, Value, Memory),
    Inc(Value),
    Dec(Value),
    Shl(Value, Value, Memory),
    Shr(Value, Value, Memory),
    And(Value, Value, Memory),
    Or(Value, Value, Memory),
    Not(Value, Memory),
    Xor(Value, Value, Memory),
}

impl Instruction {
    pub fn with_offset(mut self, offset: Memory) -> Self {
        match &mut self {
            Instruction::Not(a, m)
            | Instruction::Neg(a, m)
            | Instruction::Deref(a, m)
            | Instruction::Copy(a, m) => {
                a.offset_memory(offset);
                *m += offset
            }
            Instruction::DerefAssign(a, _, b) => {
                a.offset_memory(offset);
                b.offset_memory(offset);
            }
            Instruction::Out(a)
            | Instruction::While(a)
            | Instruction::EndWhile(a)
            | Instruction::Inc(a)
            | Instruction::Dec(a)
            | Instruction::If(a, _, _) => a.offset_memory(offset),
            Instruction::TernaryIf(a, b, c, d) => {
                a.offset_memory(offset);
                b.offset_memory(offset);
                c.offset_memory(offset);
                *d += offset;
            }
            Instruction::Clear(a, b) => {
                *a += offset;
                *b += offset;
            }
            Instruction::Input(a) => *a += offset,
            Instruction::Add(a, b, m)
            | Instruction::Sub(a, b, m)
            | Instruction::Mul(a, b, m)
            | Instruction::Div(a, b, m)
            | Instruction::Mod(a, b, m)
            | Instruction::Eq(a, b, m)
            | Instruction::Neq(a, b, m)
            | Instruction::Lt(a, b, m)
            | Instruction::Le(a, b, m)
            | Instruction::Gt(a, b, m)
            | Instruction::Ge(a, b, m)
            | Instruction::Shl(a, b, m)
            | Instruction::Shr(a, b, m)
            | Instruction::And(a, b, m)
            | Instruction::Or(a, b, m)
            | Instruction::Xor(a, b, m) => {
                a.offset_memory(offset);
                b.offset_memory(offset);
                *m += offset;
            }
            _ => (),
        };
        self
    }
}
