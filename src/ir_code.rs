use std::collections::HashMap;

use super::utils::{Error, ErrorType, Instruction, Instructions, Node, TokenType, Val, ValType};

/// Generates the Intermediate 3-address code from the AST
pub struct CodeGenerator {
    instructions: Instructions,
    array_index: usize,
    vars: HashMap<String, (Val, ValType)>,
}

impl CodeGenerator {
    fn match_node(&mut self, node: &Node) -> Result<(Val, ValType, bool), Error> {
        match node {
            Node::Number(num) => match num.token_type {
                TokenType::Number(num1) => Ok((Val::Num(num1 as i32), ValType::Number, false)),
                TokenType::Keyword(ref boolean) => match boolean.as_ref() {
                    "true" => Ok((Val::Bool(true), ValType::Boolean, false)),
                    "false" => Ok((Val::Bool(false), ValType::Boolean, false)),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },

            Node::BinaryOp(op, left, right) => {
                let (left, left_variant, _) = self.match_node(left)?;
                let (right, right_variant, _) = self.match_node(right)?;
                if left_variant != right_variant {
                    return Err(Error::new(
                        ErrorType::TypeError,
                        op.position,
                        format!(
                            "Cannot do {} for {:?} and {:?}",
                            op, right_variant, left_variant
                        ),
                    ));
                }
                self.instructions.push(
                    Instruction::from_token_binary(op)(left, right),
                    Some(self.array_index),
                );
                self.array_index += 1;
                Ok((Val::Index(self.array_index - 1), left_variant, false))
            }

            Node::UnaryOp(op, expr) => {
                let (expr, variant, _) = self.match_node(expr)?;
                self.instructions.push(
                    Instruction::from_token_unary(op)(expr),
                    Some(self.array_index),
                );
                self.array_index += 1;
                Ok((Val::Index(self.array_index - 1), variant, false))
            }

            Node::VarAssign(var, expr) => {
                if let TokenType::Identifier(ref var) = var.token_type {
                    match self.match_node(expr)? {
                        (Val::Index(index), variant, _) => {
                            self.vars
                                .insert(var.clone(), (Val::Index(self.array_index), variant));
                            self.instructions
                                .push(Instruction::Copy(Val::Index(index)), Some(self.array_index));
                            self.array_index += 1;
                            Ok((Val::Index(self.array_index - 1), variant, false))
                        }
                        (val, variant, _) => {
                            self.vars.insert(var.clone(), (val, variant));
                            Ok((Val::Index(self.array_index), variant, false))
                        }
                    }
                } else {
                    unreachable!();
                }
            }

            Node::VarAccess(var) => {
                if let TokenType::Identifier(ref var) = var.token_type {
                    let (var, variant) = self.vars.get(var).cloned().unwrap();
                    Ok((var, variant, false))
                } else {
                    unreachable!();
                }
            }

            Node::VarReassign(var1, expr) => {
                if let TokenType::Identifier(ref var) = var1.token_type {
                    match self.match_node(expr)? {
                        (Val::Index(index), variant, _) => {
                            let var = self.vars.get_mut(var).unwrap();
                            if var.1 != variant {
                                return Err(Error::new(
                                    ErrorType::TypeError,
                                    var1.position,
                                    format!(
                                        "Variable {} is of type {:?} but is being assigned {:?}",
                                        var1, var.1, variant
                                    ),
                                ));
                            }
                            var.0 = Val::Index(index);
                            self.instructions
                                .push(Instruction::Copy(Val::Index(index)), Some(self.array_index));
                            self.array_index += 1;
                            Ok((Val::Index(self.array_index), variant, false))
                        }
                        val => {
                            let var = self.vars.get_mut(var).unwrap();
                            if var.1 != val.1 {
                                return Err(Error::new(
                                    ErrorType::TypeError,
                                    var1.position,
                                    format!(
                                        "Variable {} is of type {:?} but is being assigned {:?}",
                                        var1, var.1, val.1
                                    ),
                                ));
                            }
                            var.0 = val.0;
                            Ok((Val::Index(self.array_index), val.1, false))
                        }
                    }
                } else {
                    unreachable!();
                }
            }

            Node::Statements(statements) => {
                for statement in statements {
                    let (val, variant, return_) = self.match_node(statement)?;
                    if return_ {
                        return Ok((val, variant, false));
                    }
                }
                Ok((Val::None, ValType::None, false))
            }

            Node::Print(expr) => {
                let (expr, _, _) = self.match_node(expr)?;
                self.instructions.push(Instruction::Print(expr), None);
                Ok((Val::Index(self.array_index), ValType::None, false))
            }

            Node::Ascii(expr) => {
                let (expr, _, _) = self.match_node(expr)?;
                self.instructions.push(Instruction::Ascii(expr), None);
                Ok((Val::Index(self.array_index), ValType::None, false))
            }

            Node::Input => {
                self.instructions
                    .push(Instruction::Input, Some(self.array_index));
                self.array_index += 1;
                Ok((Val::Index(self.array_index - 1), ValType::Number, false))
            }

            Node::If(cond, then, else_) => {
                let (cond, cond_variant, _) = self.match_node(cond)?;
                if cond_variant != ValType::Boolean {
                    // return Err(Error::new(
                    //     ErrorType::TypeError,
                    //     cond.position,
                    //     format!("Cannot do if for {:?}", cond_variant),
                    // ));
                }
                let (then, then_variant, _) = self.match_node(then)?;
                let else_ = match else_.as_ref().map(|e| self.match_node(e)) {
                    Some(Ok((else_, else_variant, _))) => {
                        if then_variant != else_variant {
                            // return Err(Error::new(
                            //     ErrorType::TypeError,
                            //     cond.position,
                            //     format!("Cannot do if for {:?}", cond_variant),
                            // ));
                        }
                        Some(else_)
                    }
                    Some(Err(e)) => {
                        return Err(e);
                    }
                    None => None,
                };
                self.instructions
                    .push(Instruction::If(cond, then, else_), Some(self.array_index));
                self.array_index += 1;
                Ok((Val::Index(self.array_index - 1), then_variant, false))
            }

            Node::None => Ok((Val::None, ValType::None, false)),

            Node::Call(_, _) => todo!(),
            Node::FuncDef(_, _, _) => todo!(),

            Node::Return(_, expr) => match expr {
                Some(expr) => {
                    let (expr, variant, _) = self.match_node(expr)?;
                    Ok((expr, variant, true))
                }
                None => Ok((Val::None, ValType::None, true)),
            },
        }
    }
}

/// Generates and returns the Intermediate Representation of the AST
pub fn generate_code(ast: Node) -> Result<Instructions, Error> {
    let mut obj = CodeGenerator {
        instructions: Instructions::new(),
        array_index: 0,
        vars: HashMap::new(),
    };
    obj.match_node(&ast)?;
    Ok(obj.instructions)
}
