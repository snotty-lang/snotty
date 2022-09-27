use crate::value::{Value, ValueKind};
use std::fmt;

#[derive(Debug, Clone)]
/// Last usize -> assign location
pub enum Instruction {
    If(Value, usize, bool),
    DerefAssign(Value, Value),
    DerefRef(Value, usize),
    DerefAssignRef(Value, Value),
    While(Value),
    EndWhile(Value),
    Clear(usize, usize),
    Return(Value),
    Call(usize, Vec<Value>, usize),
    Else(usize),
    EndIf(usize, bool),
    TernaryIf(Value, Value, Value, usize),
    Copy(Value, usize),
    Ref(Value, usize),
    Deref(Value, usize),
    Input(usize),
    Add(Value, Value, usize),
    Sub(Value, Value, usize),
    Mul(Value, Value, usize),
    Div(Value, Value, usize),
    Mod(Value, Value, usize),
    Neg(Value, usize),
    Print(Value),
    Ascii(Value),
    Eq(Value, Value, usize),
    Neq(Value, Value, usize),
    Lt(Value, Value, usize),
    Le(Value, Value, usize),
    Inc(Value),
    Dec(Value),
    Pow(Value, Value, usize),
    Shl(Value, Value, usize),
    Shr(Value, Value, usize),
    And(Value, Value, usize),
    Or(Value, Value, usize),
    Not(Value, usize),
    Xor(Value, Value, usize),
}

impl Instruction {
    pub fn kind(&self) -> ValueKind {
        todo!()
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::DerefAssign(val, expr) | Instruction::DerefAssignRef(val, expr) => {
                write!(f, "{} = {}", val, expr)
            }
            Instruction::Clear(from, to) => write!(f, "clear {} - {}", from, to - 1),
            Instruction::While(cond) => write!(f, "WHILE {}", cond),
            Instruction::EndWhile(cond) => write!(f, "END WHILE {}", cond),
            Instruction::Return(v) => write!(f, "return {}", v),
            Instruction::Call(a, b, mem) => write!(f, "[{mem}] = call {:?} : {:?}", a, b),
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
            Instruction::Print(val) => write!(f, "print {:?}", val),
            Instruction::Ascii(val) => write!(f, "ascii {:?}", val),
            Instruction::Pow(base, exp, mem) => write!(f, "[{mem}] = {:?} ** {:?}", base, exp),
            Instruction::Shl(left, right, mem) => write!(f, "[{mem}] = {:?} << {:?}", left, right),
            Instruction::Shr(left, right, mem) => write!(f, "[{mem}] = {:?} >> {:?}", left, right),
            Instruction::Eq(left, right, mem) => write!(f, "[{mem}] = {:?} == {:?}", left, right),
            Instruction::Neq(left, right, mem) => write!(f, "[{mem}] = {:?} != {:?}", left, right),
            Instruction::Lt(left, right, mem) => write!(f, "[{mem}] = {:?} < {:?}", left, right),
            Instruction::Le(left, right, mem) => write!(f, "[{mem}] = {:?} <= {:?}", left, right),
            Instruction::Inc(val) => write!(f, "++{:?}", val),
            Instruction::Dec(val) => write!(f, "--{:?}", val),
            Instruction::Ref(val, mem) => write!(f, "[{mem}] = &[{}]", val),
            Instruction::Deref(val, mem) | Instruction::DerefRef(val, mem) => {
                write!(f, "[{mem}] = *{:?}", val)
            }
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
