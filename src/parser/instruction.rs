use crate::parser::{value::Value, Memory};
use std::fmt;

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

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::DerefAssign(val, derefs, expr) => {
                write!(f, "{}{} = {}", "*".repeat(*derefs), val, expr)
            }
            Instruction::Clear(from, to) => write!(f, "clear {} - {}", from, to - 1),
            Instruction::While(cond) => write!(f, "WHILE {}", cond),
            Instruction::EndWhile(cond) => write!(f, "END WHILE {}", cond),
            Instruction::TernaryIf(a, b, c, mem) => {
                write!(f, "[{mem}] = if {:?} then {:?} else {:?}", a, b, c)
            }
            Instruction::Copy(val, mem) => write!(f, "[{mem}] = {:?}", val),
            Instruction::Input(mem) => write!(f, "[{mem}] = ?"),
            Instruction::Add(left, right, mem) => write!(f, "[{mem}] = {:?} + {:?}", left, right),
            Instruction::Sub(left, right, mem) => write!(f, "[{mem}] = {:?} - {:?}", left, right),
            Instruction::Mul(left, right, mem) => write!(f, "[{mem}] = {:?} * {:?}", left, right),
            Instruction::Div(left, right, mem) => write!(f, "[{mem}] = {:?} / {:?}", left, right),
            Instruction::Mod(left, right, mem) => write!(f, "[{mem}] = {:?} % {:?}", left, right),
            Instruction::Neg(val, mem) => write!(f, "[{mem}] = -{:?}", val),
            Instruction::Out(val) => write!(f, "print {:?}", val),
            Instruction::Shl(left, right, mem) => write!(f, "[{mem}] = {:?} << {:?}", left, right),
            Instruction::Shr(left, right, mem) => write!(f, "[{mem}] = {:?} >> {:?}", left, right),
            Instruction::Eq(left, right, mem) => write!(f, "[{mem}] = {:?} == {:?}", left, right),
            Instruction::Neq(left, right, mem) => write!(f, "[{mem}] = {:?} != {:?}", left, right),
            Instruction::Lt(left, right, mem) => write!(f, "[{mem}] = {:?} < {:?}", left, right),
            Instruction::Le(left, right, mem) => write!(f, "[{mem}] = {:?} <= {:?}", left, right),
            Instruction::Gt(left, right, mem) => write!(f, "[{mem}] = {:?} > {:?}", left, right),
            Instruction::Ge(left, right, mem) => write!(f, "[{mem}] = {:?} >= {:?}", left, right),
            Instruction::Inc(val) => write!(f, "++{:?}", val),
            Instruction::Dec(val) => write!(f, "--{:?}", val),
            Instruction::Deref(val, mem) => write!(f, "[{mem}] = *{:?}", val),
            Instruction::If(cond, _, _) => write!(f, "IF {:?}", cond),
            Instruction::Else(_) => write!(f, "ELSE"),
            Instruction::EndIf(_, _) => write!(f, "ENDIF"),
            Instruction::And(left, right, mem) => write!(f, "[{mem}] = {} & {}", left, right),
            Instruction::Or(left, right, mem) => write!(f, "[{mem}] = {} | {}", left, right),
            Instruction::Not(val, mem) => write!(f, "[{mem}] = !{}", val),
            Instruction::Xor(left, right, mem) => write!(f, "[{mem}] = {} ^ {}", left, right),
        }
    }
}
