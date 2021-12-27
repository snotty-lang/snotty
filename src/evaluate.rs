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
            _ => unreachable!(),
        };

        let assign = match instruction.assign {
            Some(Val::Index(index)) => index,
            _ => unreachable!(),
        };

        let right = match instruction.arg2 {
            Some(Val::Num(num)) => Some(num),
            Some(Val::Index(index)) => match vars.get(&index) {
                Some(Val::Num(num)) => Some(*num),
                _ => {
                    new.push(instruction.clone());
                    continue;
                }
            },
            None => None,
            _ => unreachable!(),
        };

        match instruction.op {
            Operator::Add => vars.insert(assign, Val::Num(left + right.unwrap())),
            Operator::Sub => vars.insert(assign, Val::Num(left - right.unwrap())),
            Operator::Mul => vars.insert(assign, Val::Num(left * right.unwrap())),
            Operator::Div => vars.insert(assign, Val::Num(left / right.unwrap())),
            Operator::Mod => vars.insert(assign, Val::Num(left % right.unwrap())),
            Operator::Neg => vars.insert(assign, Val::Num(-left)),
        };
    }
    println!("{:?}", vars);
    new
}
