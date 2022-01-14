use std::collections::HashMap;

use super::utils::{Error, ErrorType, Instruction, Instructions, Node, TokenType, Val, ValType};

/// Generates the Intermediate 3-address code from the AST
pub struct CodeGenerator {
    instructions: Instructions,
    array_index: usize,
    vars: HashMap<String, Val>,
}

impl CodeGenerator {
    fn make_instruction(&mut self, node: &Node) -> Result<(Val, bool), Error> {
        match node {
            Node::Number(num) => {
                if let TokenType::Number(num1) = num.token_type {
                    Ok((Val::Num(num1 as i32), false))
                } else {
                    unreachable!()
                }
            }

            Node::Boolean(b) => {
                if let TokenType::Keyword(ref boolean) = b.token_type {
                    match boolean.as_ref() {
                        "true" => Ok((Val::Bool(true), false)),
                        "false" => Ok((Val::Bool(false), false)),
                        _ => unreachable!(),
                    }
                } else {
                    unreachable!()
                }
            }

            Node::BinaryOp(op, left, right) => {
                let (left, lr) = self.make_instruction(left)?;
                let (right, rr) = self.make_instruction(right)?;
                let left_type = left.r#type();
                let right_type = right.r#type();
                if left_type != right.r#type() {
                    return Err(Error::new(
                        ErrorType::TypeError,
                        op.position.clone(),
                        format!(
                            "Cannot {} `{}` to `{}`",
                            op.token_type.get_operation_name(),
                            right_type,
                            left_type
                        ),
                    ));
                }
                match op.token_type {
                    TokenType::Ge => {
                        self.instructions
                            .push(Instruction::Lt(left, right), Some(self.array_index));
                        self.array_index += 1;
                        self.instructions.push(
                            Instruction::LNot(Val::Index(self.array_index - 1, ValType::Boolean)),
                            Some(self.array_index),
                        );
                    }
                    TokenType::Gt => {
                        self.instructions
                            .push(Instruction::Le(left, right), Some(self.array_index));
                        self.array_index += 1;
                        self.instructions.push(
                            Instruction::LNot(Val::Index(self.array_index - 1, ValType::Boolean)),
                            Some(self.array_index),
                        );
                    }
                    _ => {
                        self.instructions.push(
                            Instruction::from_token_binary(op)(left, right),
                            Some(self.array_index),
                        );
                    }
                }
                self.array_index += 1;
                match left_type.get_result_type(&right_type, op) {
                    Some(result_type) => {
                        Ok((Val::Index(self.array_index - 1, result_type), lr || rr))
                    }
                    None => Err(Error::new(
                        ErrorType::TypeError,
                        op.position.clone(),
                        format!(
                            "Cannot {} `{}` to `{}`",
                            op.token_type.get_operation_name(),
                            right_type,
                            left_type
                        ),
                    )),
                }
            }

            Node::UnaryOp(op, expr) => {
                let (expr, r) = self.make_instruction(expr)?;
                let expr_type = expr.r#type();
                self.instructions.push(
                    Instruction::from_token_unary(op)(expr),
                    Some(self.array_index),
                );
                self.array_index += 1;
                match expr_type.get_result_type_unary(op) {
                    Some(result_type) => Ok((Val::Index(self.array_index - 1, result_type), r)),
                    None => Err(Error::new(
                        ErrorType::TypeError,
                        op.position.clone(),
                        format!(
                            "Cannot apply `{}` to `{}`",
                            op.token_type.get_operation_name(),
                            expr_type,
                        ),
                    )),
                }
            }

            Node::VarAssign(var1, expr, dec_type) => {
                if let TokenType::Identifier(ref var) = var1.token_type {
                    match self.make_instruction(expr)? {
                        (Val::Index(index, type_), r) => {
                            if let Some(t) = dec_type {
                                if ValType::from_parse_type(t) != type_ {
                                    return Err(Error::new(
                                        ErrorType::TypeError,
                                        var1.position.clone(),
                                        format!(
                                            "Cannot assign `{}` to `{}`",
                                            type_,
                                            ValType::from_parse_type(t)
                                        ),
                                    ));
                                }
                            }
                            self.vars
                                .insert(var.clone(), Val::Index(self.array_index, type_.clone()));
                            self.instructions.push(
                                Instruction::Copy(Val::Index(index, type_.clone())),
                                Some(self.array_index),
                            );
                            self.array_index += 1;
                            Ok((Val::Index(self.array_index - 1, type_), r))
                        }
                        (val, r) => {
                            if let Some(t) = dec_type {
                                if ValType::from_parse_type(t) != val.r#type() {
                                    return Err(Error::new(
                                        ErrorType::TypeError,
                                        var1.position.clone(),
                                        format!(
                                            "Cannot assign `{}` to `{}`",
                                            val.r#type(),
                                            ValType::from_parse_type(t)
                                        ),
                                    ));
                                }
                            }
                            self.vars.insert(var.clone(), val);
                            Ok((Val::None, r))
                        }
                    }
                } else {
                    unreachable!();
                }
            }

            Node::VarAccess(var) => {
                if let TokenType::Identifier(ref var) = var.token_type {
                    Ok((self.vars.get(var).cloned().unwrap(), false))
                } else {
                    unreachable!();
                }
            }

            Node::VarReassign(var1, expr) => {
                if let TokenType::Identifier(ref var) = var1.token_type {
                    match self.make_instruction(expr)? {
                        (Val::Index(index, type_), r) => {
                            let var = self.vars.get_mut(var).unwrap();
                            if var.r#type() != type_ {
                                return Err(Error::new(
                                    ErrorType::TypeError,
                                    var1.position.clone(),
                                    format!(
                                        "Variable {} is of type {:?} but is being assigned to type {:?}",
                                        var1,
                                        var.r#type(),
                                        type_
                                    ),
                                ));
                            }
                            *var = Val::Index(index, var.r#type());
                            self.instructions.push(
                                Instruction::Copy(Val::Index(index, type_)),
                                Some(self.array_index),
                            );
                            self.array_index += 1;
                            Ok((Val::None, r))
                        }
                        (val, r) => {
                            let var = self.vars.get_mut(var).unwrap();
                            let val_type = val.r#type();
                            if var.r#type() != val_type {
                                return Err(Error::new(
                                    ErrorType::TypeError,
                                    var1.position.clone(),
                                    format!(
                                        "Variable {} is of type {:?} but is being assigned to type {:?}",
                                        var1,
                                        var.r#type(),
                                        val_type
                                    ),
                                ));
                            }
                            *var = val;
                            Ok((Val::Index(self.array_index, val_type), r))
                        }
                    }
                } else {
                    unreachable!();
                }
            }

            Node::Statements(statements, _) => {
                for statement in statements {
                    let (val, return_) = self.make_instruction(statement)?;
                    if return_ {
                        return Ok((val, false));
                    }
                }
                Ok((Val::None, false))
            }

            Node::Print(exprs, _) => {
                let mut r = false;
                for expr in exprs {
                    let (expr, r1) = self.make_instruction(expr)?;
                    r |= r1;
                    if let Val::Char(_) = expr {
                        self.instructions.push(Instruction::PrintChar(expr), None);
                    } else {
                        self.instructions.push(Instruction::Print(expr), None);
                    }
                }
                Ok((Val::None, r))
            }

            Node::Ascii(exprs, _) => {
                let mut r = false;
                for expr in exprs {
                    let (expr, r1) = self.make_instruction(expr)?;
                    r |= r1;
                    self.instructions.push(Instruction::Ascii(expr), None);
                }
                Ok((Val::None, r))
            }

            Node::Input(_) => {
                self.instructions
                    .push(Instruction::Input, Some(self.array_index));
                self.array_index += 1;
                Ok((Val::Index(self.array_index - 1, ValType::Number), false))
            }

            Node::If(_, _, _, _) => {
                // let (cond, _) = self.match_node(cond1)?;
                // if cond.r#type() != ValType::Boolean {
                //     return Err(Error::new(
                //         ErrorType::TypeError,
                //         cond1.position(),
                //         format!(
                //             "Condition in an if statement can only be of type Boolean, and not of type {:?}",
                //             cond.r#type()
                //         ),
                //     ));
                // }

                // let then_part = self.instructions.0.len();
                // self.match_node(then1)?;

                // let label = self.current_label;
                // self.current_label += 1;
                // self.instructions
                //     .push(Instruction::Label(Label(label)), None);

                // let else_part = self.instructions.0.len();
                // let else_ = match else_1.as_ref().map(|e| self.match_node(e)) {
                //     Some(Ok((else_, _))) => Some(else_),
                //     Some(Err(e)) => {
                //         return Err(e);
                //     }
                //     None => None,
                // };

                // self.instructions
                //     .0
                //     .insert(then_part, (None, Instruction::JmpFalse(cond, Label(label))));

                // if else_.is_some() {
                //     self.instructions.0.insert(
                //         else_part,
                //         (None, Instruction::Jmp(Label(self.current_label))),
                //     );
                //     self.instructions
                //         .push(Instruction::Label(Label(self.current_label)), None);
                //     self.current_label += 1;
                // }
                Ok((Val::None, false))
            }

            Node::Ternary(cond1, then1, else_1, _) => {
                let (cond, cr) = self.make_instruction(cond1)?;
                if cond.r#type() != ValType::Boolean {
                    return Err(Error::new(
                        ErrorType::TypeError,
                        cond1.position(),
                        format!(
                            "Condition in an if statement can only be of type Boolean, and not of type {:?}",
                            cond.r#type()
                        ),
                    ));
                }

                let (then, tr) = self.make_instruction(then1)?;
                let then_type = then.r#type();

                let (else_, er) = self.make_instruction(else_1)?;
                if then_type != else_.r#type() {
                    return Err(Error::new(
                        ErrorType::TypeError,
                        else_1.position(),
                        format!("Then and else branch have different types, expected type {:?}, found type {:?}", then_type, else_.r#type()),
                    ));
                }

                self.instructions.push(
                    Instruction::TernaryIf(cond, then, else_),
                    Some(self.array_index),
                );
                self.array_index += 1;
                Ok((Val::Index(self.array_index - 1, then_type), cr || tr || er))
            }

            Node::None(_) => Ok((Val::None, false)),

            Node::Call(_, _, _) => todo!(),
            Node::FuncDef(_, _, _, _, _) => todo!(),

            Node::Index(arr, index1, _) => {
                let arr = if let TokenType::Identifier(ref var) = arr.token_type {
                    self.vars.get(var).cloned().unwrap()
                } else {
                    unreachable!();
                };
                let (index, _) = self.make_instruction(index1)?;
                if index.r#type() != ValType::Number {
                    return Err(Error::new(
                        ErrorType::TypeError,
                        index1.position(),
                        format!(
                            "Indexing can only be done with numbers, and not of type {:?}",
                            index.r#type()
                        ),
                    ));
                }
                let arr_type = match arr.r#type() {
                    ValType::Array(_, t) => *t,
                    _ => unreachable!(),
                };
                self.instructions
                    .push(Instruction::Index(arr, index), Some(self.array_index));
                self.array_index += 1;
                Ok((Val::Index(self.array_index - 1, arr_type), false))
            }

            Node::IndexAssign(_, _, _) => todo!(),

            Node::Array(elements, _) => {
                let mut r = false;
                let mut type_ = None;
                let mut new = vec![];
                for element1 in elements {
                    let (element, r1) = self.make_instruction(element1)?;
                    r |= r1;
                    if let Some(t) = type_ {
                        if t != element.r#type() {
                            return Err(Error::new(
                                ErrorType::TypeError,
                                element1.position(),
                                format!(
                                    "Array elements have different types, expected type {}, found type {}",
                                    t,
                                    element.r#type()
                                ),
                            ));
                        }
                    }
                    type_ = Some(element.r#type());
                    new.push(element);
                }
                Ok((Val::Array(new, type_.unwrap_or(ValType::None)), r))
            }

            Node::Return(expr, _) => match expr {
                Some(expr) => Ok((self.make_instruction(expr)?.0, true)),
                None => Ok((Val::None, true)),
            },

            Node::Ref(val1, _) => {
                let (val, r) = self.make_instruction(val1)?;
                if val.is_constant() {
                    return Err(Error::new(
                        ErrorType::TypeError,
                        val1.position(),
                        format!("Cannot reference a {}", val.r#type()),
                    ));
                }
                let t = val.r#type();
                self.instructions
                    .push(Instruction::Ref(val), Some(self.array_index));
                self.array_index += 1;
                Ok((
                    Val::Index(self.array_index - 1, ValType::Pointer(Box::new(t))),
                    r,
                ))
            }

            Node::Deref(val1, _) => {
                let (val, r) = self.make_instruction(val1)?;
                if let ValType::Pointer(t) = val.r#type() {
                    self.instructions
                        .push(Instruction::Deref(val), Some(self.array_index));
                    self.array_index += 1;
                    Ok((Val::Index(self.array_index - 1, *t), r))
                } else {
                    Err(Error::new(
                        ErrorType::TypeError,
                        val1.position(),
                        format!("Cannot dereference a {}", val.r#type()),
                    ))
                }
            }

            Node::Tuple(_) => Ok((Val::None, false)),

            Node::Char(c, _) => {
                if let TokenType::Char(c) = c.token_type {
                    Ok((Val::Char(c), false))
                } else {
                    unreachable!()
                }
            }
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
    obj.make_instruction(&ast)?;
    Ok(obj.instructions)
}
