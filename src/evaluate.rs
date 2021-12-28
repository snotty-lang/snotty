use std::collections::HashMap;

use super::utils::{Instruction, Instructions, Operator, Val};

pub fn evaluate(code: &Instructions) -> Instructions {
    let mut vars = HashMap::new();
    let mut new = Instructions::new();
    for instruction in &code.instructions {
        let left = match instruction.arg1 {
            Val::Num(num) => num,
            Val::Index(index) => match vars.get(&index) {
                Some(Val::Num(num)) => *num,
                _ => {
                    new.push(instruction.clone());
                    continue;
                }
            },
        };

        let assign = match instruction.assign {
            Some(Val::Index(index)) => index,

            // ezout and ezascii
            None => match instruction.op {
                Operator::Print => {
                    let left_str = left.to_string();
                    let left_vec = left_str.chars().collect::<Vec<char>>();
                    for left in left_vec {
                        new.push(Instruction::new(
                            Operator::Print,
                            Val::Num(left as u32 as i32),
                        ));
                    }
                    new.push(Instruction::new(Operator::Print, Val::Num(10)));
                    continue;
                }
                Operator::Ascii => {
                    let mut new_instruction = instruction.clone();
                    new_instruction.arg1 = Val::Num(left);
                    new.push(new_instruction);
                    continue;
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };

        let right = match instruction.arg2 {
            Some(Val::Num(num)) => num,
            Some(Val::Index(index)) => match vars.get(&index) {
                Some(Val::Num(num)) => *num,
                _ => {
                    new.push(instruction.clone());
                    continue;
                }
            },

            // Unary Operations
            None => match instruction.op {
                Operator::Neg => {
                    vars.insert(assign, Val::Num(-left));
                    continue;
                }
                _ => unreachable!(),
            },
        };

        vars.insert(
            assign,
            Val::Num(match instruction.op {
                Operator::Add => left + right,
                Operator::Sub => left - right,
                Operator::Mul => left * right,
                Operator::Div => left / right,
                Operator::Mod => left % right,
                _ => unreachable!(),
            }),
        );
    }
    println!("{:?}", vars);
    println!("{}", new);
    new
}
