use crate::value::{Value, ValueKind};
use std::fmt;

#[derive(Debug, Clone)]
pub enum Instruction {
    If(Value, usize, bool),
    DerefAssign(Value, Value),
    DerefRef(Value),
    DerefAssignRef(Value, Value),
    While(Value),
    EndWhile(Value),
    Clear(usize, usize),
    Return(Value),
    Call(usize, Vec<Value>),
    Else(usize),
    EndIf(usize, bool),
    TernaryIf(Value, Value, Value),
    Copy(Value),
    Ref(usize),
    Deref(Value),
    LXor(Value, Value),
    Input,
    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Div(Value, Value),
    Mod(Value, Value),
    Neg(Value),
    Print(Value),
    Ascii(Value),
    Eq(Value, Value),
    Neq(Value, Value),
    Lt(Value, Value),
    Le(Value, Value),
    LAnd(Value, Value),
    LOr(Value, Value),
    LNot(Value),
    Inc(Value),
    Dec(Value),
    Pow(Value, Value),
    Shl(Value, Value),
    Shr(Value, Value),
    BAnd(Value, Value),
    BOr(Value, Value),
    BXor(Value, Value),
    BNot(Value),
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
            Instruction::Call(a, b) => write!(f, "call {:?} : {:?}", a, b),
            Instruction::LXor(left, right) => write!(f, "{:?} !&| {:?}", left, right),
            Instruction::TernaryIf(a, b, c) => write!(f, "if {:?} then {:?} else {:?}", a, b, c),
            Instruction::Copy(val) => write!(f, "{:?}", val),
            Instruction::Input => write!(f, "?"),
            Instruction::Add(left, right) => write!(f, "{:?} + {:?}", left, right),
            Instruction::Sub(left, right) => write!(f, "{:?} - {:?}", left, right),
            Instruction::Mul(left, right) => write!(f, "{:?} * {:?}", left, right),
            Instruction::Div(left, right) => write!(f, "{:?} / {:?}", left, right),
            Instruction::Mod(left, right) => write!(f, "{:?} % {:?}", left, right),
            Instruction::Neg(val) => write!(f, "-{:?}", val),
            Instruction::Print(val) => write!(f, "print {:?}", val),
            Instruction::Ascii(val) => write!(f, "ascii {:?}", val),
            Instruction::Pow(base, exp) => write!(f, "{:?} ** {:?}", base, exp),
            Instruction::Shl(left, right) => write!(f, "{:?} << {:?}", left, right),
            Instruction::Shr(left, right) => write!(f, "{:?} >> {:?}", left, right),
            Instruction::BAnd(left, right) => write!(f, "{:?} & {:?}", left, right),
            Instruction::BOr(left, right) => write!(f, "{:?} | {:?}", left, right),
            Instruction::BXor(left, right) => write!(f, "{:?} ^ {:?}", left, right),
            Instruction::BNot(val) => write!(f, "~{:?}", val),
            Instruction::Eq(left, right) => write!(f, "{:?} == {:?}", left, right),
            Instruction::Neq(left, right) => write!(f, "{:?} != {:?}", left, right),
            Instruction::Lt(left, right) => write!(f, "{:?} < {:?}", left, right),
            Instruction::Le(left, right) => write!(f, "{:?} <= {:?}", left, right),
            Instruction::LAnd(left, right) => write!(f, "{:?} && {:?}", left, right),
            Instruction::LOr(left, right) => write!(f, "{:?} || {:?}", left, right),
            Instruction::LNot(val) => write!(f, "!{:?}", val),
            Instruction::Inc(val) => write!(f, "++{:?}", val),
            Instruction::Dec(val) => write!(f, "--{:?}", val),
            Instruction::Ref(mem) => write!(f, "&[{}]", mem),
            Instruction::Deref(val) | Instruction::DerefRef(val) => write!(f, "*{:?}", val),
            Instruction::If(cond, _, _) => write!(f, "IF {:?}", cond),
            Instruction::Else(_) => write!(f, "ELSE"),
            Instruction::EndIf(_, _) => write!(f, "ENDIF"),
        }
    }
}
