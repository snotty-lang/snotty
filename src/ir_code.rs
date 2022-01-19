use std::collections::HashMap;

use crate::utils::{
    Error, ErrorType, Instruction, Instructions, Memory, Node, TokenType, Val, ValNumber, ValType,
    POINTER_SIZE,
};

/// Generates the Intermediate 3-address code from the AST
pub struct CodeGenerator {
    instructions: Instructions,
}

impl CodeGenerator {
    fn make_instruction(
        &mut self,
        node: &Node,
        vars: &mut HashMap<String, Val>,
        memory: &mut Memory,
    ) -> Result<Val, Error> {
        match node {
            Node::Number(num) => {
                if let TokenType::Number(num1) = num.token_type {
                    Ok(Val::Num(num1 as ValNumber))
                } else {
                    unreachable!()
                }
            }

            Node::Boolean(b) => {
                if let TokenType::Keyword(ref boolean) = b.token_type {
                    match boolean.as_ref() {
                        "true" => Ok(Val::Bool(true)),
                        "false" => Ok(Val::Bool(false)),
                        _ => unreachable!(),
                    }
                } else {
                    unreachable!()
                }
            }

            Node::BinaryOp(op, left, right) => {
                let left = self.make_instruction(left, vars, memory)?;
                let right = self.make_instruction(right, vars, memory)?;
                let left_type = left.r#type();
                let right_type = right.r#type();
                let t = match left_type.get_result_type(&right_type, op) {
                    Some(result_type) => result_type,
                    None => {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            op.position.clone(),
                            format!(
                                "Cannot {} `{}` to `{}`",
                                op.token_type.get_operation_name(),
                                right_type,
                                left_type
                            ),
                        ))
                    }
                };
                let mut mem = memory.allocate(t.get_size());
                match op.token_type {
                    TokenType::Ge => {
                        self.instructions
                            .push(Instruction::Lt(left, right), Some(mem));
                        let new_mem = memory.allocate(1);
                        self.instructions.push(
                            Instruction::LNot(Val::Index(mem, ValType::Boolean)),
                            Some(new_mem),
                        );
                        mem = new_mem;
                    }
                    TokenType::Gt => {
                        self.instructions
                            .push(Instruction::Le(left, right), Some(mem));
                        let new_mem = memory.allocate(1);
                        self.instructions.push(
                            Instruction::LNot(Val::Index(mem, ValType::Boolean)),
                            Some(new_mem),
                        );
                        mem = new_mem;
                    }
                    _ => {
                        self.instructions
                            .push(Instruction::from_token_binary(op)(left, right), Some(mem));
                    }
                }
                Ok(Val::Index(mem, t))
            }

            Node::UnaryOp(op, expr) => {
                let expr = self.make_instruction(expr, vars, memory)?;
                let expr_type = expr.r#type();
                let t = match expr_type.get_result_type_unary(op) {
                    Some(result_type) => result_type,
                    None => {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            op.position.clone(),
                            format!(
                                "Cannot apply `{}` to `{}`",
                                op.token_type.get_operation_name(),
                                expr_type,
                            ),
                        ))
                    }
                };
                let mem = memory.allocate(t.get_size());
                self.instructions
                    .push(Instruction::from_token_unary(op)(expr), Some(mem));
                Ok(Val::Index(mem, t))
            }

            Node::VarAssign(var1, expr, dec_type) => {
                if let TokenType::Identifier(ref var) = var1.token_type {
                    match self.make_instruction(expr, vars, memory)? {
                        Val::Index(index, type_) => {
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
                            let mem = memory.allocate(type_.get_size());
                            vars.insert(var.clone(), Val::Index(mem, type_.clone()));
                            self.instructions.push(
                                Instruction::Copy(Val::Index(index, type_.clone())),
                                Some(mem),
                            );
                            Ok(Val::Index(mem, type_))
                        }
                        val => {
                            let v = val.r#type();
                            if let Some(t) = dec_type {
                                if ValType::from_parse_type(t) != v {
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
                            let mem = memory.allocate(v.get_size());
                            self.instructions.push(Instruction::Copy(val), Some(mem));
                            vars.insert(var.clone(), Val::Index(mem, v));
                            Ok(Val::None)
                        }
                    }
                } else {
                    unreachable!();
                }
            }

            Node::VarAccess(var) => {
                if let TokenType::Identifier(ref var) = var.token_type {
                    Ok(vars.get(var).cloned().unwrap())
                } else {
                    unreachable!();
                }
            }

            Node::VarReassign(var1, expr) => {
                if let TokenType::Identifier(ref var2) = var1.token_type {
                    match self.make_instruction(expr, vars, memory)? {
                        Val::Index(index, type_) => {
                            let var = vars.get_mut(var2).unwrap();
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
                            if let Val::Index(mem, _) = var {
                                self.instructions.push(
                                    Instruction::Copy(Val::Index(index, type_)),
                                    Some(*mem),
                                );
                            } else {
                                unreachable!();
                            }
                            Ok(Val::None)
                        }
                        val => {
                            let var = vars.get_mut(var2).unwrap();
                            let val_type = val.r#type();
                            if var.r#type() != val_type {
                                return Err(Error::new(
                                    ErrorType::TypeError,
                                    var1.position.clone(),
                                    format!(
                                        "Variable {} is of type {:?} but is being assigned to type {:?}",
                                        val_type,
                                        var.r#type(),
                                        val_type
                                    ),
                                ));
                            }
                            if let Val::Index(mem, _) = var {
                                self.instructions.push(Instruction::Copy(val), Some(*mem));
                            } else {
                                unreachable!();
                            }
                            Ok(Val::None)
                        }
                    }
                } else {
                    unreachable!();
                }
            }

            Node::Statements(statements, _) => {
                let mut new = memory.clone();
                for statement in statements {
                    self.make_instruction(statement, vars, &mut new)?;
                }
                Ok(Val::None)
            }

            Node::Print(exprs, _) => {
                for expr in exprs {
                    let expr = self.make_instruction(expr, vars, memory)?;
                    if let Val::Char(_) = expr {
                        self.instructions.push(Instruction::PrintChar(expr), None);
                    } else {
                        self.instructions.push(Instruction::Print(expr), None);
                    }
                }
                Ok(Val::None)
            }

            Node::Ascii(exprs, _) => {
                for expr in exprs {
                    let expr = self.make_instruction(expr, vars, memory)?;
                    self.instructions.push(Instruction::Ascii(expr), None);
                }
                Ok(Val::None)
            }

            Node::Input(_) => {
                let t = ValType::Number;
                let mem = memory.allocate(t.get_size());
                self.instructions.push(Instruction::Input, Some(mem));
                Ok(Val::Index(mem, t))
            }

            Node::If(cond1, then1, else1, _) => {
                let cond = self.make_instruction(cond1, vars, memory)?;
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

                self.instructions.push(Instruction::If(cond), None);
                let then = self.make_instruction(then1, vars, memory)?;
                if then.r#type() != ValType::None {
                    return Err(Error::new(
                        ErrorType::TypeError,
                        then1.position(),
                        format!(
                            "If statement can only return None, and not type {:?}",
                            then.r#type()
                        ),
                    ));
                }

                match else1 {
                    Some(else_) => {
                        self.instructions.push(Instruction::Else, None);
                        let e = self.make_instruction(else_, vars, memory)?;
                        if e.r#type() != ValType::None {
                            return Err(Error::new(
                                ErrorType::TypeError,
                                then1.position(),
                                format!(
                                    "If statement can only return None, and not type {:?}",
                                    then.r#type()
                                ),
                            ));
                        }
                    }
                    None => {}
                };

                self.instructions.push(Instruction::EndIf, None);
                Ok(Val::None)
            }

            Node::Ternary(cond1, then1, else_1, _) => {
                let cond = self.make_instruction(cond1, vars, memory)?;
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

                let then = self.make_instruction(then1, vars, memory)?;
                let then_type = then.r#type();

                let else_ = self.make_instruction(else_1, vars, memory)?;
                if then_type != else_.r#type() {
                    return Err(Error::new(
                        ErrorType::TypeError,
                        else_1.position(),
                        format!("Then and else branch have different types, expected type {:?}, found type {:?}", then_type, else_.r#type()),
                    ));
                }

                let mem = memory.allocate(1);
                self.instructions
                    .push(Instruction::TernaryIf(cond, then, else_), Some(mem));
                Ok(Val::Index(mem, then_type))
            }

            Node::None(_) => Ok(Val::None),

            Node::Call(_, args, _) => {
                let mut new = vec![];
                for arg in args {
                    let arg = self.make_instruction(arg, vars, memory)?;
                    new.push(arg);
                }
                let mem = memory.allocate(1);
                self.instructions.push(Instruction::Call(0, new), Some(mem));
                Ok(Val::Index(mem, ValType::None))
            }

            Node::FuncDef(_, _, _, _, _) => todo!(),

            Node::Index(arr1, index1, _) => {
                let arr = if let TokenType::Identifier(ref var) = arr1.token_type {
                    vars.get(var).cloned().unwrap()
                } else {
                    unreachable!();
                };
                let index = self.make_instruction(index1, vars, memory)?;
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
                    ValType::Pointer(t) => *t,
                    _ => {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            arr1.position.clone(),
                            format!("Cannot index type {:?}", arr.r#type()),
                        ))
                    }
                };
                let mem = memory.allocate(2);
                self.instructions
                    .push(Instruction::Add(arr, index), Some(mem));
                self.instructions.push(
                    Instruction::Deref(Val::Index(mem, arr_type.clone())),
                    Some(mem + 1),
                );
                Ok(Val::Index(mem + 1, arr_type))
            }

            Node::IndexAssign(_, _, _) => todo!(),

            Node::Array(elements, _) => {
                let mut type_ = None;
                let mut new = vec![];
                for element1 in elements {
                    let element = self.make_instruction(element1, vars, memory)?;
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
                Ok(Val::Array(new, type_.unwrap_or(ValType::None)))
            }

            Node::Return(val, _) => {
                let val = if let Some(val) = val {
                    self.make_instruction(val, vars, memory)?
                } else {
                    Val::None
                };
                self.instructions.push(Instruction::Return(val), None);
                Ok(Val::None)
            }

            Node::Ref(val1, _) => {
                let val = self.make_instruction(val1, vars, memory)?;
                if val.is_constant() {
                    return Err(Error::new(
                        ErrorType::TypeError,
                        val1.position(),
                        format!("Cannot reference a {}", val.r#type()),
                    ));
                }
                let t = val.r#type();
                let mem = memory.allocate(POINTER_SIZE);
                self.instructions.push(Instruction::Ref(val), Some(mem));
                Ok(Val::Index(mem, ValType::Pointer(Box::new(t))))
            }

            Node::Deref(val1, _) => {
                let val = self.make_instruction(val1, vars, memory)?;
                if let ValType::Pointer(t) = val.r#type() {
                    let mem = memory.allocate(t.get_size());
                    self.instructions.push(Instruction::Deref(val), Some(mem));
                    Ok(Val::Index(mem, *t))
                } else {
                    Err(Error::new(
                        ErrorType::TypeError,
                        val1.position(),
                        format!("Cannot dereference a {}", val.r#type()),
                    ))
                }
            }

            Node::Char(c, _) => {
                if let TokenType::Char(c) = c.token_type {
                    Ok(Val::Char(c))
                } else {
                    unreachable!()
                }
            }

            Node::Lambda(_, _, _, _) => todo!(),
            Node::DerefAssign(_, _, _) => todo!(),

            Node::While(cond1, body1, _) => {
                let cond = self.make_instruction(cond1, vars, memory)?;
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

                self.instructions.push(Instruction::While(cond), None);
                let body = self.make_instruction(body1, vars, memory)?;
                if body.r#type() != ValType::None {
                    return Err(Error::new(
                        ErrorType::TypeError,
                        body1.position(),
                        format!(
                            "Body of a while loop can only be of type None, and not of type {:?}",
                            body.r#type()
                        ),
                    ));
                }
                self.instructions.push(Instruction::EndWhile, None);
                Ok(Val::None)
            }
        }
    }
}

/// Generates and returns the Intermediate Representation of the AST
pub fn generate_code(ast: Node) -> Result<Instructions, Error> {
    let mut obj = CodeGenerator {
        instructions: Instructions::new(),
    };
    let mut vars = HashMap::new();
    let mut index = Memory::new();
    obj.make_instruction(&ast, &mut vars, &mut index)?;
    Ok(obj.instructions)
}
