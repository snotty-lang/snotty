use std::collections::HashMap;

use super::utils::{Instructions, Operator, Val};

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

            // ezout
            None => {
                let mut new_instruction = instruction.clone();
                new_instruction.arg1 = Val::Num(left);
                new.push(new_instruction);
                continue;
            }
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
