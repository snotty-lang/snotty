#![allow(dead_code, unused_variables)]

use std::{collections::HashMap, fmt};

use super::utils::{Node, Token, TokenType};

pub struct CodeGenerator {
    instructions: Instructions,
    array_index: usize,
    vars: HashMap<String, Val>,
}

impl CodeGenerator {
    pub fn generate_code(ast: Node) -> Instructions {
        let mut obj = Self {
            instructions: Instructions::new(),
            array_index: 0,
            vars: HashMap::new(),
        };
        obj.match_node(&ast);
        obj.instructions
    }

    fn match_node(&mut self, node: &Node) -> Option<Val> {
        match node {
            Node::Number(num) => {
                if let TokenType::Number(num) = num.token_type {
                    Some(Val::Num(num))
                } else {
                    unreachable!();
                }
            }

            Node::BinaryOp(op, left, right) => {
                let left = self.match_node(left)?;
                let right = self.match_node(right)?;
                self.instructions.push(
                    Instruction::new(Operator::from_token(op, false), left)
                        .arg(right)
                        .assign(Val::Index(self.array_index)),
                );
                self.array_index += 1;
                Some(Val::Index(self.array_index - 1))
            }

            Node::UnaryOp(op, expr) => {
                let expr = self.match_node(expr)?;
                self.instructions.push(
                    Instruction::new(Operator::from_token(op, true), expr)
                        .assign(Val::Index(self.array_index)),
                );
                self.array_index += 1;
                Some(Val::Index(self.array_index - 1))
            }

            Node::VarAssign(var, expr) => {
                if let TokenType::Identifier(ref var) = var.token_type {
                    let index = self.match_node(expr)?;
                    self.vars.insert(var.clone(), index);
                    Some(Val::Index(self.array_index))
                } else {
                    unreachable!();
                }
            }

            Node::VarAccess(var) => {
                if let TokenType::Identifier(ref var) = var.token_type {
                    self.vars.get(var).cloned()
                } else {
                    unreachable!();
                }
            }

            Node::VarReassign(var, expr) => {
                if let TokenType::Identifier(ref var) = var.token_type {
                    let index = self.match_node(expr)?;
                    self.vars.insert(var.clone(), index);
                    Some(Val::Index(self.array_index))
                } else {
                    unreachable!();
                }
            }

            Node::Statements(statements) => {
                for statement in statements {
                    if let Some(val) = self.match_node(statement) {
                        println!("{}", val);
                        println!("{:?}", self.vars);
                    }
                }
                println!("--------------");
                None
            }

            Node::Call(_, _) => todo!(),
            Node::FuncDef(_, _, _) => todo!(),
            Node::Return(_, _) => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,
}

impl Operator {
    fn from_token(t: &Token, unary: bool) -> Self {
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
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Val {
    Var(String),
    Num(u32),
    Index(usize),
}

#[derive(Debug)]
pub struct Instructions {
    instructions: Vec<Instruction>,
}

impl Instructions {
    fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }

    fn push(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
}

#[derive(Debug)]
pub struct Instruction {
    op: Operator,
    arg1: Val,
    arg2: Option<Val>,
    assign: Option<Val>,
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
            Val::Var(var) => write!(f, "{}", var),
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
        }
    }
}
