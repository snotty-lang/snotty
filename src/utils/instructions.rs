use super::{Token, TokenType};
use std::fmt;

#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,
    Print,
    Ascii,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
    LAnd,
    LOr,
    LNot,
}

impl Operator {
    pub fn from_token(t: &Token, unary: bool) -> Self {
        match t.token_type {
            TokenType::Add => Self::Add,
            TokenType::Sub => {
                if unary {
                    Self::Neg
                } else {
                    Self::Sub
                }
            }
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
            TokenType::LNot => Self::LNot,
            _ => unreachable!("{}", t),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Val {
    Num(i32),
    Bool(bool),
    Index(usize),
}

#[derive(Debug)]
pub struct Instructions {
    pub instructions: Vec<Instruction>,
}

impl Instructions {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }

    pub fn push(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub op: Operator,
    pub arg1: Val,
    pub arg2: Option<Val>,
    pub assign: Option<Val>,
}

impl Instruction {
    pub fn new(op: Operator, arg1: Val) -> Self {
        Self {
            op,
            arg1,
            arg2: None,
            assign: None,
        }
    }

    pub fn arg(mut self, arg: Val) -> Self {
        self.arg2 = Some(arg);
        self
    }

    pub fn assign(mut self, assign: Val) -> Self {
        self.assign = Some(assign);
        self
    }
}

impl fmt::Display for Instructions {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for instruction in &self.instructions {
            writeln!(f, "{}", instruction)?;
        }
        Ok(())
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let text = match &self.arg2 {
            Some(arg2) => format!("{} {} {}", self.arg1, self.op, arg2),
            None => format!("{}{}", self.op, self.arg1,),
        };
        match self.assign {
            Some(ref assign) => write!(f, "{} = {}", assign, text),
            None => write!(f, "{}", text),
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

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operator::Add => write!(f, "+"),
            Operator::Sub => write!(f, "-"),
            Operator::Mul => write!(f, "*"),
            Operator::Div => write!(f, "/"),
            Operator::Mod => write!(f, "%"),
            Operator::Neg => write!(f, "-"),
            Operator::Print => write!(f, "print "),
            Operator::Ascii => write!(f, "ascii "),
            Operator::Eq => write!(f, "=="),
            Operator::Neq => write!(f, "!="),
            Operator::Lt => write!(f, "<"),
            Operator::Gt => write!(f, ">"),
            Operator::Le => write!(f, "<="),
            Operator::Ge => write!(f, ">="),
            Operator::LAnd => write!(f, "&&"),
            Operator::LOr => write!(f, "||"),
            Operator::LNot => write!(f, "!"),
        }
    }
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
