use std::collections::HashMap;

use super::utils::{Instruction, Instructions, Node, Operator, TokenType, Val};

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

    fn match_node(&mut self, node: &Node) -> Val {
        match node {
            Node::Number(num) => match num.token_type {
                TokenType::Number(num) => Val::Num(num as i32),
                TokenType::Keyword(ref boolean) => match boolean.as_ref() {
                    "true" => Val::Bool(true),
                    "false" => Val::Bool(false),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },

            Node::BinaryOp(op, left, right) => {
                let left = self.match_node(left);
                let right = self.match_node(right);
                self.instructions.push(
                    Instruction::new(Operator::from_token(op, false), left)
                        .arg(right)
                        .assign(Val::Index(self.array_index)),
                );
                self.array_index += 1;
                Val::Index(self.array_index - 1)
            }

            Node::UnaryOp(op, expr) => {
                let expr = self.match_node(expr);
                self.instructions.push(
                    Instruction::new(Operator::from_token(op, true), expr)
                        .assign(Val::Index(self.array_index)),
                );
                self.array_index += 1;
                Val::Index(self.array_index - 1)
            }

            Node::VarAssign(var, expr) => {
                if let TokenType::Identifier(ref var) = var.token_type {
                    let index = self.match_node(expr);
                    self.vars.insert(var.clone(), index);
                    Val::Index(self.array_index)
                } else {
                    unreachable!();
                }
            }

            Node::VarAccess(var) => {
                if let TokenType::Identifier(ref var) = var.token_type {
                    self.vars.get(var).cloned().unwrap()
                } else {
                    unreachable!();
                }
            }

            Node::VarReassign(var, expr) => {
                if let TokenType::Identifier(ref var) = var.token_type {
                    let index = self.match_node(expr);
                    self.vars.insert(var.clone(), index);
                    Val::Index(self.array_index)
                } else {
                    unreachable!();
                }
            }

            Node::Statements(statements) => {
                for statement in statements {
                    let val = self.match_node(statement);
                    println!("{}", val);
                    println!("{:?}", self.vars);
                }
                println!("--------------");
                Val::Index(self.array_index)
            }

            Node::Print(expr) => {
                let expr = self.match_node(expr);
                self.instructions
                    .push(Instruction::new(Operator::Print, expr));
                Val::Index(self.array_index)
            }

            Node::Ascii(expr) => {
                let expr = self.match_node(expr);
                self.instructions
                    .push(Instruction::new(Operator::Ascii, expr));
                Val::Index(self.array_index)
            }

            Node::Input => todo!(),
            Node::Call(_, _) => todo!(),
            Node::FuncDef(_, _, _) => todo!(),
            Node::Return(_, _) => todo!(),
        }
    }
}
