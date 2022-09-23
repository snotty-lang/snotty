use std::{
    collections::HashMap,
    fmt,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::parser::{error::Error, Rule};
use pest::iterators::Pair;

pub type ID = u128;

#[inline]
fn gen_id() -> u128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos()
}

#[derive(Clone, PartialEq)]
pub enum Value {
    Num(u8),
    Bool(bool),
    None,
    Ref(usize, ValueKind),
    Char(u8),
    Memory(ID, ValueKind),
}

impl Value {
    pub fn kind(&self) -> ValueKind {
        match self {
            Value::Char(_) => ValueKind::Char,
            Value::Num(_) => ValueKind::Number,
            Value::Bool(_) => ValueKind::Boolean,
            Value::None => ValueKind::None,
            Value::Ref(_, t) => ValueKind::Ref(Box::new(t.clone())),
            Value::Memory(_, t) => t.clone(),
        }
    }

    pub fn get_size(&self) -> usize {
        self.kind().get_size()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueKind {
    None,
    Number,
    Char,
    Boolean,
    Ref(Box<ValueKind>),
    DataBox(ID, Vec<(ID, ValueKind)>),
    Function(ID, Vec<ValueKind>, Box<ValueKind>),
}

impl ValueKind {
    pub fn from_pair<'a>(
        mut pair: Pair<'a, Rule>,
        memory: &HashMap<&'a str, (Pair<'a, Rule>, ValueKind)>,
    ) -> Result<Self, Error<'a>> {
        if pair.as_rule() == Rule::expr {
            pair = pair.into_inner().next().unwrap();
        }
        match pair.as_rule() {
            Rule::expr => ValueKind::from_pair(pair.into_inner().next().unwrap(), memory),
            Rule::number => Ok(ValueKind::Number),
            Rule::boolean => Ok(ValueKind::Boolean),
            Rule::none => Ok(ValueKind::None),
            Rule::char => Ok(ValueKind::Char),
            Rule::type_cast => {
                let mut iter = pair.into_inner();
                let kind = ValueKind::from_pair(iter.next().unwrap(), memory)?;
                let expr_kind = ValueKind::from_pair(iter.next().unwrap(), memory)?;
                println!("<{}>{}", kind, expr_kind);
                Ok(kind)
            }
            Rule::ident => memory
                .get(pair.as_str())
                .ok_or_else(|| Error::UndefinedReference(pair))
                .and_then(|(p, k)| {
                    if !matches!(p.as_rule(), Rule::expr | Rule::function) {
                        Ok(k.clone())
                    } else {
                        Err(Error::TypeError(p.clone()))
                    }
                }),
            Rule::ternary => {
                let mut iter = pair.into_inner();
                let cond_pair = iter.next().unwrap();
                let cond = ValueKind::from_pair(cond_pair.clone(), memory)?;
                if cond != ValueKind::Boolean {
                    return Err(Error::TypeError(cond_pair));
                }
                let then = ValueKind::from_pair(iter.next().unwrap(), memory)?;
                let otherwise_pair = iter.next().unwrap();
                let otherwise = ValueKind::from_pair(otherwise_pair.clone(), memory)?;
                if otherwise != then {
                    return Err(Error::TypeError(otherwise_pair));
                }
                Ok(then)
            }
            Rule::increment | Rule::decrement => {
                let kind = ValueKind::from_pair(pair.clone().into_inner().next().unwrap(), memory)?;
                match kind {
                    ValueKind::Number | ValueKind::Boolean | ValueKind::Char => Ok(kind),
                    _ => Err(Error::TypeError(pair)),
                }
            }
            Rule::function => {
                let mut iter = pair.into_inner();
                drop(iter.next());
                let mut ret_kind = ValueKind::None;
                let mut params = Vec::new();
                while let Some(next) = iter.peek() {
                    match next.as_rule() {
                        Rule::ident => {
                            drop(iter.next().unwrap().as_str());
                            params.push(ValueKind::from_pair(iter.next().unwrap(), memory)?);
                        }
                        Rule::kind => {
                            ret_kind = ValueKind::from_pair(iter.next().unwrap(), memory)?
                        }
                        Rule::stmt => break,
                        _ => unreachable!(),
                    }
                }
                let id = gen_id();
                Ok(ValueKind::Function(id, params, Box::new(ret_kind)))
            }
            Rule::databox => {
                let mut iter = pair.into_inner();
                drop(iter.next());
                let mut fields = Vec::new();
                while iter.next().is_some() {
                    fields.push((
                        gen_id(),
                        ValueKind::from_pair(iter.next().unwrap(), memory)?,
                    ));
                }
                let id = gen_id();
                Ok(ValueKind::DataBox(id, fields))
            }
            Rule::kind => {
                let kind = if let Some(kind) = pair.clone().into_inner().flatten().next() {
                    kind.as_str()
                } else {
                    pair.as_str()
                };
                let mut kind = match kind {
                    "int" => ValueKind::Number,
                    "bool" => ValueKind::Boolean,
                    "char" => ValueKind::Char,
                    ";" => ValueKind::None,
                    _ => unreachable!(),
                };

                while let Some(inner) = pair.into_inner().next() {
                    kind = ValueKind::Ref(Box::new(kind));
                    pair = inner;
                }
                Ok(kind)
            }
            _ => unreachable!(),
        }
    }

    pub fn get_size(&self) -> usize {
        match self {
            ValueKind::None => 0,
            ValueKind::Number => std::mem::size_of::<u8>(),
            ValueKind::Char => 1,
            ValueKind::Boolean => 1,
            ValueKind::Ref(t) => t.get_size(),
            ValueKind::DataBox(.., s) => s.iter().fold(0, |acc, (_, t)| acc + t.get_size()),
            ValueKind::Function(..) => 0,
        }
    }
}

impl fmt::Display for ValueKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ValueKind::Ref(t) => write!(f, "&{}", t),
            ValueKind::Char => write!(f, "char"),
            ValueKind::None => write!(f, ";"),
            ValueKind::Number => write!(f, "integer"),
            ValueKind::Boolean => write!(f, "bool"),
            ValueKind::DataBox(t, ..) => write!(f, "databox {}", t),
            ValueKind::Function(t, ..) => write!(f, "function {}", t),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Char(c) => write!(f, "{}", *c as char),
            Value::None => write!(f, ";"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Num(num) => write!(f, "{}", num),
            Value::Ref(ptr, _) => write!(f, "&{}", ptr),
            Value::Memory(mem, _) => write!(f, "<{}>", mem),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

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

// / Assignment Index, Result type, Free memory location
// pub type AssignType = (Option<(usize, usize)>, usize);

// /// A vector of instructions.
// #[derive(Debug)]
// pub struct Instructions(pub Vec<(AssignType, Instruction)>);

// impl Instructions {
//     pub fn new() -> Self {
//         Self(Vec::new())
//     }

//     pub fn push(&mut self, instruction: Instruction, assign: AssignType) {
//         self.0.push((assign, instruction));
//     }
// }

// impl Default for Instructions {
//     fn default() -> Self {
//         Self::new()
//     }
// }

// impl fmt::Display for Instructions {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         for (assign, instruction) in &self.0 {
//             match assign.0 {
//                 Some(assign) => writeln!(f, "[{}] = {}", assign.0, instruction),
//                 None => writeln!(f, "{}", instruction),
//             }?;
//         }
//         Ok(())
//     }
// }
