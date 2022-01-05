use super::{Token, TokenType};
use std::fmt;

/// An enum to specify the type of the operator.
#[derive(Debug, Clone)]
pub enum Instruction {
    // Label(String),
    // Jmp(String),
    Copy(Val),
    Input,
    Add(Val, Val),
    Sub(Val, Val),
    Mul(Val, Val),
    Div(Val, Val),
    Mod(Val, Val),
    Neg(Val),
    Print(Val),
    Ascii(Val),
    Eq(Val, Val),
    Neq(Val, Val),
    Lt(Val, Val),
    Gt(Val, Val),
    Le(Val, Val),
    Ge(Val, Val),
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
            TokenType::Gt => Self::Gt,
            TokenType::Le => Self::Le,
            TokenType::Ge => Self::Ge,
            TokenType::LAnd => Self::LAnd,
            TokenType::LOr => Self::LOr,
            TokenType::Pow => Self::Pow,
            TokenType::Shl => Self::Shl,
            TokenType::Shr => Self::Shr,
            TokenType::BAnd => Self::BAnd,
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
            Instruction::Copy(val) => write!(f, "{}", val),
            Self::Input => write!(f, "?"),
            Self::Add(left, right) => write!(f, "{} + {}", left, right),
            Self::Sub(left, right) => write!(f, "{} - {}", left, right),
            Self::Mul(left, right) => write!(f, "{} * {}", left, right),
            Self::Div(left, right) => write!(f, "{} / {}", left, right),
            Self::Mod(left, right) => write!(f, "{} % {}", left, right),
            Self::Neg(val) => write!(f, "-{}", val),
            Self::Print(val) => write!(f, "print {}", val),
            Self::Ascii(val) => write!(f, "ascii {}", val),
            Self::Pow(base, exp) => write!(f, "{} ** {}", base, exp),
            Self::Shl(left, right) => write!(f, "{} << {}", left, right),
            Self::Shr(left, right) => write!(f, "{} >> {}", left, right),
            Self::BAnd(left, right) => write!(f, "{} & {}", left, right),
            Self::BOr(left, right) => write!(f, "{} | {}", left, right),
            Self::BXor(left, right) => write!(f, "{} ^ {}", left, right),
            Self::BNot(val) => write!(f, "~{}", val),
            Self::Eq(left, right) => write!(f, "{} == {}", left, right),
            Self::Neq(left, right) => write!(f, "{} != {}", left, right),
            Self::Lt(left, right) => write!(f, "{} < {}", left, right),
            Self::Gt(left, right) => write!(f, "{} > {}", left, right),
            Self::Le(left, right) => write!(f, "{} <= {}", left, right),
            Self::Ge(left, right) => write!(f, "{} >= {}", left, right),
            Self::LAnd(left, right) => write!(f, "{} && {}", left, right),
            Self::LOr(left, right) => write!(f, "{} || {}", left, right),
            Self::LNot(val) => write!(f, "!{}", val),
            Self::Inc(val) => write!(f, "++{}", val),
            Self::Dec(val) => write!(f, "--{}", val),
        }
    }
}

/// An enum to specify the type of the value.
#[derive(Debug, Clone)]
pub enum Val {
    /// A number.
    Num(i32),
    /// A boolean.
    Bool(bool),
    /// An index (variable).
    Index(usize),
}

impl Val {
    /// # Panics
    /// Panics if the variant is `Self::Index`
    pub fn get_int(&self) -> i32 {
        match self {
            Val::Num(num) => *num,
            Val::Bool(b) => *b as i32,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Val::Bool(b) => write!(f, "{}", b),
            Val::Num(num) => write!(f, "{}", num),
            Val::Index(index) => write!(f, "[{}]", index),
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
