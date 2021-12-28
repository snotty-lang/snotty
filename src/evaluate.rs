use std::collections::HashMap;

use super::utils::{Instruction, Instructions, Operator, Val};

/// Evaluates constant time operations during compile time
pub fn evaluate(code: &Instructions) -> Instructions {
    let mut vars = HashMap::new();
    let mut new = Instructions::new();
    for instruction in &code.instructions {
        let left = match &instruction.arg1 {
            Val::Index(index) => match vars.get(index) {
                Some(Val::Index(_)) | None => {
                    new.push(instruction.clone());
                    continue;
                }
                Some(x) => x.clone(),
            },
            x => x.clone(),
        };

        let assign = match instruction.assign {
            Some(Val::Index(index)) => index,

            // Printing
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
                    new_instruction.arg1 = Val::Num(left.get_int());
                    new.push(new_instruction);
                    continue;
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };

        let right = match &instruction.arg2 {
            Some(Val::Index(index)) => match vars.get(index) {
                Some(Val::Index(_)) | None => {
                    new.push(instruction.clone());
                    continue;
                }
                Some(x) => x.clone(),
            },
            Some(x) => x.clone(),

            // Unary Operations
            None => match instruction.op {
                Operator::Neg => {
                    vars.insert(assign, Val::Num(-(left.get_int())));
                    continue;
                }
                Operator::LNot => {
                    vars.insert(assign, Val::Bool(left.get_int() == 0));
                    continue;
                }
                Operator::Inc => {
                    vars.insert(assign, Val::Num(left.get_int() + 1));
                    continue;
                }
                Operator::Dec => {
                    vars.insert(assign, Val::Num(left.get_int() - 1));
                    continue;
                }
                Operator::BNot => {
                    vars.insert(assign, Val::Num(!left.get_int()));
                    continue;
                }
                Operator::Input => {
                    new.push(instruction.clone());
                    continue;
                }
                _ => unreachable!(),
            },
        };

        let left = left.get_int();
        let right = right.get_int();

        vars.insert(
            assign,
            match instruction.op {
                Operator::Add => Val::Num(left + right),
                Operator::Sub => Val::Num(left - right),
                Operator::Mul => Val::Num(left * right),
                Operator::Div => Val::Num(left / right),
                Operator::Mod => Val::Num(left % right),
                Operator::Eq => Val::Bool(left == right),
                Operator::Neq => Val::Bool(left != right),
                Operator::Lt => Val::Bool(left < right),
                Operator::Gt => Val::Bool(left > right),
                Operator::Le => Val::Bool(left <= right),
                Operator::Ge => Val::Bool(left >= right),
                Operator::LAnd => Val::Bool(left != 0 && right != 0),
                Operator::LOr => Val::Bool(left != 0 || right != 0),
                Operator::BAnd => Val::Num(left & right),
                Operator::BOr => Val::Num(left | right),
                Operator::BXor => Val::Num(left ^ right),
                Operator::Shl => Val::Num(left << right),
                Operator::Shr => Val::Num(left >> right),
                Operator::Pow => Val::Num(if right < 0 {
                    1 / left.pow(right.abs() as u32)
                } else {
                    left.pow(right as u32)
                }),
                _ => unreachable!(),
            },
        );
    }
    println!("{:?}", vars);
    println!("{}", new);
    new
}
