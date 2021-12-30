use std::collections::HashMap;

use super::utils::{Instruction, Instructions, Val};

/// Evaluates constant time operations during compile time
pub fn evaluate(code: &Instructions) -> Instructions {
    let mut vars = HashMap::new();
    let mut new = Instructions::new();
    for (assign, instruction) in &code.0 {
        let evaluated = match instruction {
            Instruction::Input => {
                new.push(instruction.clone(), *assign);
                continue;
            }
            Instruction::Add(left, right) => {
                super::check!(left, new, vars, assign, instruction);
                super::check!(right, new, vars, assign, instruction);
                Val::Num(left + right)
            }
            Instruction::Sub(left, right) => {
                super::check!(left, new, vars, assign, instruction);
                super::check!(right, new, vars, assign, instruction);
                Val::Num(left - right)
            }
            Instruction::Mul(left, right) => {
                super::check!(left, new, vars, assign, instruction);
                super::check!(right, new, vars, assign, instruction);
                Val::Num(left * right)
            }
            Instruction::Div(left, right) => {
                super::check!(left, new, vars, assign, instruction);
                super::check!(right, new, vars, assign, instruction);
                Val::Num(left / right)
            }
            Instruction::Mod(left, right) => {
                super::check!(left, new, vars, assign, instruction);
                super::check!(right, new, vars, assign, instruction);
                Val::Num(left % right)
            }
            Instruction::Neg(val) => {
                super::check!(val, new, vars, assign, instruction);
                Val::Num(-val)
            }
            Instruction::Print(val) => {
                super::check!(val, new, vars, assign, instruction);
                let left_str = val.to_string();
                let left_vec = left_str.chars().collect::<Vec<char>>();
                for left in left_vec {
                    new.push(Instruction::Print(Val::Num(left as u32 as i32)), None);
                }
                new.push(Instruction::Print(Val::Num(10)), None);
                continue;
            }
            Instruction::Ascii(_) => {
                new.push(instruction.clone(), *assign);
                continue;
            }
            Instruction::Eq(left, right) => {
                super::check!(left, new, vars, assign, instruction);
                super::check!(right, new, vars, assign, instruction);
                Val::Bool(left == right)
            }
            Instruction::Neq(left, right) => {
                super::check!(left, new, vars, assign, instruction);
                super::check!(right, new, vars, assign, instruction);
                Val::Bool(left != right)
            }
            Instruction::Lt(left, right) => {
                super::check!(left, new, vars, assign, instruction);
                super::check!(right, new, vars, assign, instruction);
                Val::Bool(left < right)
            }
            Instruction::Gt(left, right) => {
                super::check!(left, new, vars, assign, instruction);
                super::check!(right, new, vars, assign, instruction);
                Val::Bool(left > right)
            }
            Instruction::Le(left, right) => {
                super::check!(left, new, vars, assign, instruction);
                super::check!(right, new, vars, assign, instruction);
                Val::Bool(left <= right)
            }
            Instruction::Ge(left, right) => {
                super::check!(left, new, vars, assign, instruction);
                super::check!(right, new, vars, assign, instruction);
                Val::Bool(left >= right)
            }
            Instruction::LAnd(left, right) => {
                super::check!(left, new, vars, assign, instruction);
                super::check!(right, new, vars, assign, instruction);
                Val::Bool(left != 0 && right != 0)
            }
            Instruction::LOr(left, right) => {
                super::check!(left, new, vars, assign, instruction);
                super::check!(right, new, vars, assign, instruction);
                Val::Bool(left != 0 || right != 0)
            }
            Instruction::LNot(val) => {
                super::check!(val, new, vars, assign, instruction);
                Val::Bool(val == 0)
            }
            Instruction::Inc(val) => {
                super::check!(val, new, vars, assign, instruction);
                Val::Num(val + 1)
            }
            Instruction::Dec(val) => {
                super::check!(val, new, vars, assign, instruction);
                Val::Num(val - 1)
            }
            Instruction::Pow(left, right) => {
                super::check!(left, new, vars, assign, instruction);
                super::check!(right, new, vars, assign, instruction);
                if right < 0 {
                    Val::Num(1 / left.pow(-right as u32))
                } else {
                    Val::Num(left.pow(right as u32))
                }
            }
            Instruction::Shl(left, right) => {
                super::check!(left, new, vars, assign, instruction);
                super::check!(right, new, vars, assign, instruction);
                Val::Num(left << right)
            }
            Instruction::Shr(left, right) => {
                super::check!(left, new, vars, assign, instruction);
                super::check!(right, new, vars, assign, instruction);
                Val::Num(left >> right)
            }
            Instruction::BAnd(left, right) => {
                super::check!(left, new, vars, assign, instruction);
                super::check!(right, new, vars, assign, instruction);
                Val::Num(left & right)
            }
            Instruction::BOr(left, right) => {
                super::check!(left, new, vars, assign, instruction);
                super::check!(right, new, vars, assign, instruction);
                Val::Num(left | right)
            }
            Instruction::BXor(left, right) => {
                super::check!(left, new, vars, assign, instruction);
                super::check!(right, new, vars, assign, instruction);
                Val::Num(left ^ right)
            }
            Instruction::BNot(val) => {
                super::check!(val, new, vars, assign, instruction);
                Val::Num(!val)
            }
        };

        vars.insert(assign.unwrap(), evaluated);
    }

    new
}

#[macro_export]
macro_rules! check {
    ($val:ident, $new: ident, $vars: ident, $assign: ident, $instruction: ident) => {
        let $val = if let Val::Index(index) = $val {
            match $vars.get(index) {
                Some(Val::Index(_)) | None => {
                    $new.push($instruction.clone(), *$assign);
                    continue;
                }
                Some(val) => val.get_int(),
            }
        } else {
            $val.get_int()
        };
    };
}
