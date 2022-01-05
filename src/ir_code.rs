use std::collections::HashMap;

use super::utils::{Instruction, Instructions, Node, TokenType, Val};

/// Generates the Intermediate 3-address code from the AST
pub struct CodeGenerator {
    instructions: Instructions,
    array_index: usize,
    vars: HashMap<String, Val>,
}

impl CodeGenerator {
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
                    Instruction::from_token_binary(op)(left, right),
                    Some(self.array_index),
                );
                self.array_index += 1;
                Val::Index(self.array_index - 1)
            }

            Node::UnaryOp(op, expr) => {
                let expr = self.match_node(expr);
                self.instructions.push(
                    Instruction::from_token_unary(op)(expr),
                    Some(self.array_index),
                );
                self.array_index += 1;
                Val::Index(self.array_index - 1)
            }

            Node::VarAssign(var, expr) => {
                if let TokenType::Identifier(ref var) = var.token_type {
                    match self.match_node(expr) {
                        Val::Index(index) => {
                            self.vars.insert(var.clone(), Val::Index(self.array_index));
                            self.instructions
                                .push(Instruction::Copy(Val::Index(index)), Some(self.array_index));
                            self.array_index += 1;
                            Val::Index(self.array_index - 1)
                        }
                        val => {
                            self.vars.insert(var.clone(), val);
                            Val::Index(self.array_index)
                        }
                    }
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
                    match self.match_node(expr) {
                        Val::Index(index) => {
                            self.vars.insert(var.clone(), Val::Index(self.array_index));
                            self.instructions
                                .push(Instruction::Copy(Val::Index(index)), Some(self.array_index));
                            self.array_index += 1;
                            Val::Index(self.array_index - 1)
                        }
                        val => {
                            self.vars.insert(var.clone(), val);
                            Val::Index(self.array_index)
                        }
                    }
                } else {
                    unreachable!();
                }
            }

            Node::Statements(statements) => {
                let mut val = Val::Index(self.array_index);
                for statement in statements {
                    val = self.match_node(statement);
                }
                val
            }

            Node::Print(expr) => {
                let expr = self.match_node(expr);
                self.instructions.push(Instruction::Print(expr), None);
                Val::Index(self.array_index)
            }

            Node::Ascii(expr) => {
                let expr = self.match_node(expr);
                self.instructions.push(Instruction::Ascii(expr), None);
                Val::Index(self.array_index)
            }

            Node::Input => {
                self.instructions
                    .push(Instruction::Input, Some(self.array_index));
                self.array_index += 1;
                Val::Index(self.array_index - 1)
            }

            Node::If(_cond, _then, _else_) => {
                // let cond = self.match_node(cond);
                // let then = self.match_node(then);
                // let else_ = else_.map(|e| self.match_node(&*e));
                // self.instructions.push(
                //     Instruction::If(cond, then, else_),
                //     Some(self.array_index),
                // );
                // self.array_index += 1;
                // Val::Index(self.array_index - 1)
                todo!()
            }

            Node::Call(_, _) => todo!(),
            Node::FuncDef(_, _, _) => todo!(),
            Node::Return(_, _) => todo!(),
        }
    }
}

/// Generates and returns the Intermediate Representation of the AST
pub fn generate_code(ast: Node) -> Instructions {
    let mut obj = CodeGenerator {
        instructions: Instructions::new(),
        array_index: 0,
        vars: HashMap::new(),
    };
    obj.match_node(&ast);
    obj.instructions
}
