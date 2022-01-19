use crate::utils::{
    Token, TokenType, Type, ValNumber, BOOLEAN_EXCLUSIVE, BOOLEAN_OPERATORS, NONE_SIZE,
    POINTER_SIZE,
};
use std::fmt;

/// An enum to specify the type of the instruction.
#[derive(Debug, Clone)]
pub enum Instruction {
    If(Val),
    While(Val),
    EndWhile,
    Return(Val),
    Call(usize, Vec<Val>),
    Else,
    EndIf,
    TernaryIf(Val, Val, Val),
    Copy(Val),
    Ref(Val),
    Deref(Val),
    LXor(Val, Val),
    Input,
    Add(Val, Val),
    Sub(Val, Val),
    Mul(Val, Val),
    Div(Val, Val),
    Mod(Val, Val),
    Neg(Val),
    Print(Val),
    PrintChar(Val),
    Ascii(Val),
    Eq(Val, Val),
    Neq(Val, Val),
    Lt(Val, Val),
    Le(Val, Val),
    LAnd(Val, Val),
    LOr(Val, Val),
    LNot(Val),
    Inc(Val),
    Dec(Val),
    Pow(Val, Val),
    Shl(Val, Val),
    Shr(Val, Val),
    BAnd(Val, Val),
    BOr(Val, Val),
    BXor(Val, Val),
    BNot(Val),
}

impl Instruction {
    /// Converts a `Token` to an `Operator`.
    /// # Arguments
    /// * `token` - The `Token` to convert.
    /// * `unary` - Whether the operator is unary.
    /// # Returns
    /// The `Instruction` variant corresponding to the `Token`.
    pub fn from_token_binary(t: &Token) -> fn(Val, Val) -> Self {
        match t.token_type {
            TokenType::Add => Self::Add,
            TokenType::Sub => Self::Sub,
            TokenType::Mul => Self::Mul,
            TokenType::Div => Self::Div,
            TokenType::Mod => Self::Mod,
            TokenType::Eq => Self::Eq,
            TokenType::Neq => Self::Neq,
            TokenType::Lt => Self::Lt,
            TokenType::Le => Self::Le,
            TokenType::LAnd => Self::LAnd,
            TokenType::LOr => Self::LOr,
            TokenType::Pow => Self::Pow,
            TokenType::Shl => Self::Shl,
            TokenType::Shr => Self::Shr,
            TokenType::BAnd => Self::BAnd,
            TokenType::LXor => Self::LXor,
            TokenType::BOr => Self::BOr,
            TokenType::BXor => Self::BXor,
            _ => unreachable!("{}", t),
        }
    }

    pub fn from_token_unary(t: &Token) -> fn(Val) -> Self {
        match t.token_type {
            TokenType::Sub => Self::Neg,
            TokenType::LNot => Self::LNot,
            TokenType::Inc => Self::Inc,
            TokenType::Dec => Self::Dec,
            TokenType::BNot => Self::BNot,
            _ => unreachable!("{}", t),
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::While(cond) => write!(f, "WHILE {}", cond),
            Self::EndWhile => write!(f, "END WHILE"),
            Self::Return(v) => write!(f, "return {}", v),
            Self::Call(a, b) => write!(f, "call {:?} : {:?}", a, b),
            Self::LXor(left, right) => write!(f, "{:?} !&| {:?}", left, right),
            Self::TernaryIf(a, b, c) => write!(f, "if {:?} then {:?} else {:?}", a, b, c),
            Self::Copy(val) => write!(f, "{:?}", val),
            Self::Input => write!(f, "?"),
            Self::Add(left, right) => write!(f, "{:?} + {:?}", left, right),
            Self::Sub(left, right) => write!(f, "{:?} - {:?}", left, right),
            Self::Mul(left, right) => write!(f, "{:?} * {:?}", left, right),
            Self::Div(left, right) => write!(f, "{:?} / {:?}", left, right),
            Self::Mod(left, right) => write!(f, "{:?} % {:?}", left, right),
            Self::Neg(val) => write!(f, "-{:?}", val),
            Self::Print(val) | Self::PrintChar(val) => write!(f, "print {:?}", val),
            Self::Ascii(val) => write!(f, "ascii {:?}", val),
            Self::Pow(base, exp) => write!(f, "{:?} ** {:?}", base, exp),
            Self::Shl(left, right) => write!(f, "{:?} << {:?}", left, right),
            Self::Shr(left, right) => write!(f, "{:?} >> {:?}", left, right),
            Self::BAnd(left, right) => write!(f, "{:?} & {:?}", left, right),
            Self::BOr(left, right) => write!(f, "{:?} | {:?}", left, right),
            Self::BXor(left, right) => write!(f, "{:?} ^ {:?}", left, right),
            Self::BNot(val) => write!(f, "~{:?}", val),
            Self::Eq(left, right) => write!(f, "{:?} == {:?}", left, right),
            Self::Neq(left, right) => write!(f, "{:?} != {:?}", left, right),
            Self::Lt(left, right) => write!(f, "{:?} < {:?}", left, right),
            Self::Le(left, right) => write!(f, "{:?} <= {:?}", left, right),
            Self::LAnd(left, right) => write!(f, "{:?} && {:?}", left, right),
            Self::LOr(left, right) => write!(f, "{:?} || {:?}", left, right),
            Self::LNot(val) => write!(f, "!{:?}", val),
            Self::Inc(val) => write!(f, "++{:?}", val),
            Self::Dec(val) => write!(f, "--{:?}", val),
            Self::Ref(val) => write!(f, "&{:?}", val),
            Self::Deref(val) => write!(f, "*{:?}", val),
            Self::If(cond) => write!(f, "IF {:?}", cond),
            Self::Else => write!(f, "ELSE"),
            Self::EndIf => write!(f, "ENDIF"),
        }
    }
}

/// An enum to specify the type of the value.
#[derive(Clone, PartialEq)]
pub enum Val {
    /// A number.
    Num(ValNumber),
    /// A boolean.
    Bool(bool),
    /// An index (variable).
    Index(usize, ValType),
    /// None
    None,
    /// A pointer.
    Pointer(usize, ValType),
    /// A Char
    Char(u8),
    /// Array
    Array(Vec<Val>, ValType),
    /// Function
    Function(usize, Vec<Val>, ValType),
}

impl Val {
    /// # Panics
    /// Panics if the variant is not `Self::Num or Self::Bool` or `Self::Char`.
    pub fn get_int(&self) -> ValNumber {
        match self {
            Val::Num(num) => *num,
            Val::Bool(b) => *b as ValNumber,
            Val::Char(c) => *c as ValNumber,
            _ => unreachable!(),
        }
    }

    pub fn r#type(&self) -> ValType {
        match self {
            Val::Function(_, _, ty) => ty.clone(),
            Val::Array(v, t) => ValType::Array(v.len(), Box::new(t.clone())),
            Val::Char(_) => ValType::Char,
            Val::Num(_) => ValType::Number,
            Val::Bool(_) => ValType::Boolean,
            Val::Index(_, t) => t.clone(),
            Val::Pointer(_, t) => t.clone(),
            Val::None => ValType::None,
        }
    }

    pub fn to_ptr(&self) -> Option<usize> {
        match self {
            Val::Pointer(ptr, _) => Some(*ptr),
            Val::Num(num) => Some(*num as usize),
            Val::Index(num, ValType::Number) => Some(*num),
            Val::Index(ptr, ValType::Pointer(_)) => Some(*ptr),
            _ => None,
        }
    }

    pub fn is_constant(&self) -> bool {
        match self {
            Val::Function(_, _, _) => true,
            Val::Array(v, _) => v.iter().any(|v| v.is_constant()) || v.is_empty(),
            Val::Char(_) => true,
            Val::Num(_) => true,
            Val::Bool(_) => true,
            Val::Index(_, _) => false,
            Val::Pointer(_, _) => false,
            Val::None => true,
        }
    }

    pub fn get_size(&self) -> usize {
        self.r#type().get_size()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValType {
    None,
    Number,
    Char,
    Boolean,
    Pointer(Box<ValType>),
    Array(usize, Box<ValType>),
}

impl ValType {
    pub fn is_ptr(&self) -> bool {
        matches!(self, ValType::Pointer(_) | ValType::Array(_, _))
    }

    pub fn get_result_type(&self, rhs: &ValType, op: &Token) -> Option<Self> {
        match (self, rhs) {
            (Self::Number, Self::Number) => {
                if BOOLEAN_OPERATORS.contains(&op.token_type) {
                    Some(Self::Boolean)
                } else if BOOLEAN_EXCLUSIVE.contains(&op.token_type) {
                    None
                } else {
                    Some(Self::Number)
                }
            }
            (Self::Pointer(t) | Self::Array(_, t), Self::Number) => {
                if [TokenType::Add, TokenType::Sub].contains(&op.token_type) {
                    Some(Self::Pointer(t.clone()))
                } else {
                    None
                }
            }
            (Self::Boolean, Self::Boolean) => {
                if BOOLEAN_OPERATORS.contains(&op.token_type)
                    || BOOLEAN_EXCLUSIVE.contains(&op.token_type)
                {
                    Some(Self::Boolean)
                } else {
                    None
                }
            }
            (Self::Char, Self::Char) => {
                if BOOLEAN_OPERATORS.contains(&op.token_type) {
                    Some(Self::Boolean)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn get_result_type_unary(&self, op: &Token) -> Option<Self> {
        match self {
            Self::Number => {
                if op.token_type == TokenType::LNot {
                    None
                } else {
                    Some(Self::Number)
                }
            }
            Self::Boolean => {
                if op.token_type == TokenType::LNot {
                    Some(Self::Boolean)
                } else {
                    None
                }
            }
            Self::Pointer(t) => {
                if [TokenType::Inc, TokenType::Dec].contains(&op.token_type) {
                    Some(Self::Pointer(t.clone()))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn from_parse_type(t: &Type) -> Self {
        match t {
            Type::Char => Self::Char,
            Type::Number => Self::Number,
            Type::Boolean => Self::Boolean,
            Type::Ref(t) => Self::Pointer(Box::new(Self::from_parse_type(t))),
            Type::None => Self::None,
            Type::Function(_, _) => todo!(),
            Type::Array(t, l) => Self::Array(*l, Box::new(Self::from_parse_type(t))),
        }
    }

    pub fn get_size(&self) -> usize {
        match self {
            Self::None => NONE_SIZE,
            Self::Number => std::mem::size_of::<ValNumber>(),
            Self::Char => 1,
            Self::Boolean => 1,
            Self::Pointer(_) => POINTER_SIZE,
            Self::Array(l, t) => l * t.get_size(),
        }
    }
}

impl fmt::Display for ValType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Array(l, t) => write!(f, "[{}; {}]", t, l),
            Self::Char => write!(f, "char"),
            Self::Pointer(t) => write!(f, "&{}", **t),
            Self::None => write!(f, "()"),
            Self::Number => write!(f, "integer"),
            Self::Boolean => write!(f, "bool"),
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Val::Function(name, args, ret) => write!(
                f,
                "func {} : {} -> {}",
                name,
                args.iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                ret
            ),
            Val::Array(v, _) => write!(
                f,
                "[{}]",
                v.iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Val::Char(c) => write!(f, "{}", *c as char),
            Val::Pointer(mem, _) => write!(f, "*{}", mem),
            Val::None => write!(f, "()"),
            Val::Bool(b) => write!(f, "{}", b),
            Val::Num(num) => write!(f, "{}", num),
            Val::Index(index, _) => write!(f, "[{}]", index),
        }
    }
}

impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Val::Function(name, args, ret) => write!(
                f,
                "func {} : {} -> {}",
                name,
                args.iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                ret
            ),
            Val::Array(v, _) => write!(
                f,
                "[{}]",
                v.iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Val::Char(c) => write!(f, "'{}'", *c as char),
            Val::Pointer(mem, _) => write!(f, "*{}", mem),
            Val::None => write!(f, "()"),
            Val::Bool(b) => write!(f, "{}", b),
            Val::Num(num) => write!(f, "{}", num),
            Val::Index(index, _) => write!(f, "[{}]", index),
        }
    }
}

/// A vector of instructions.
#[derive(Debug)]
pub struct Instructions(pub Vec<(Option<usize>, Instruction)>);

impl Instructions {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, instruction: Instruction, assign: Option<usize>) {
        self.0.push((assign, instruction));
    }
}

impl Default for Instructions {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for Instructions {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (assign, instruction) in &self.0 {
            match assign {
                Some(assign) => writeln!(f, "[{}] = {}", assign, instruction),
                None => writeln!(f, "{}", instruction),
            }?;
        }
        Ok(())
    }
}
