use std::collections::HashMap;

use super::utils::{Instruction, Instructions, Val};

/// Evaluates constant time operations during compile time
pub fn evaluate(code: &Instructions) -> Instructions {
    let mut vars = HashMap::new();
    let mut new = Instructions::new();
    for (assign, instruction) in &code.0 {
        let evaluated = match instruction {
            Instruction::Add(left, right) => {
                super::check!(BINARY left, right, new, vars, assign, instruction);
                Val::Num(left + right)
            }
            Instruction::Sub(left, right) => {
                super::check!(BINARY left, right, new, vars, assign, instruction);
                Val::Num(left - right)
            }
            Instruction::Mul(left, right) => {
                super::check!(BINARY left, right, new, vars, assign, instruction);
                Val::Num(left * right)
            }
            Instruction::Div(left, right) => {
                super::check!(BINARY left, right, new, vars, assign, instruction);
                Val::Num(left / right)
            }
            Instruction::Mod(left, right) => {
                super::check!(BINARY left, right, new, vars, assign, instruction);
                Val::Num(left % right)
            }
            Instruction::Neg(val) => {
                super::check!(val, new, vars, assign, instruction);
                Val::Num(-val)
            }
            Instruction::Ascii(val) => {
                super::check!(val, new, vars, assign, instruction);
                new.push(Instruction::Ascii(Val::Num(val)), *assign);
                continue;
            }
            Instruction::Input => {
                new.push(instruction.clone(), *assign);
                continue;
            }
            Instruction::Print(val) => {
                super::check!(VAL val, new, vars, assign, instruction);
                let left_str = val.to_string();
                let left_vec = left_str.chars().collect::<Vec<char>>();
                for left in left_vec {
                    new.push(Instruction::Ascii(Val::Num(left as u32 as i32)), *assign);
                }
                new.push(Instruction::Ascii(Val::Num(10)), *assign);
                continue;
            }
            Instruction::Eq(left, right) => {
                super::check!(BINARY left, right, new, vars, assign, instruction);
                Val::Bool(left == right)
            }
            Instruction::Neq(left, right) => {
                super::check!(BINARY left, right, new, vars, assign, instruction);
                Val::Bool(left != right)
            }
            Instruction::Lt(left, right) => {
                super::check!(BINARY left, right, new, vars, assign, instruction);
                Val::Bool(left < right)
            }
            Instruction::Le(left, right) => {
                super::check!(BINARY left, right, new, vars, assign, instruction);
                Val::Bool(left <= right)
            }
            Instruction::LAnd(left, right) => {
                super::check!(BINARY left, right, new, vars, assign, instruction);
                Val::Bool(left != 0 && right != 0)
            }
            Instruction::LOr(left, right) => {
                super::check!(BINARY left, right, new, vars, assign, instruction);
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
                super::check!(BINARY left, right, new, vars, assign, instruction);
                if right < 0 {
                    Val::Num(1 / left.pow(-right as u32))
                } else {
                    Val::Num(left.pow(right as u32))
                }
            }
            Instruction::Shl(left, right) => {
                super::check!(BINARY left, right, new, vars, assign, instruction);
                Val::Num(left << right)
            }
            Instruction::Shr(left, right) => {
                super::check!(BINARY left, right, new, vars, assign, instruction);
                Val::Num(left >> right)
            }
            Instruction::BAnd(left, right) => {
                super::check!(BINARY left, right, new, vars, assign, instruction);
                Val::Num(left & right)
            }
            Instruction::BOr(left, right) => {
                super::check!(BINARY left, right, new, vars, assign, instruction);
                Val::Num(left | right)
            }
            Instruction::BXor(left, right) => {
                super::check!(BINARY left, right, new, vars, assign, instruction);
                Val::Num(left ^ right)
            }
            Instruction::BNot(val) => {
                super::check!(val, new, vars, assign, instruction);
                Val::Num(!val)
            }
            Instruction::Copy(val) => {
                if let Val::Index(index, _) = val {
                    match vars.get(index) {
                        Some(Val::Index(..)) | None => {
                            new.push(instruction.clone(), *assign);
                            continue;
                        }
                        Some(val) => val.clone(),
                    }
                } else {
                    val.clone()
                }
            }
            Instruction::TernaryIf(cond, then, else_) => {
                let cond2 = if let Val::Index(index, _) = cond {
                    match vars.get(index) {
                        Some(Val::Index(..)) | None => None,
                        Some(val) => Some(val.get_int()),
                    }
                } else {
                    Some(cond.get_int())
                };
                let then2 = if let Val::Index(index, _) = then {
                    match vars.get(index) {
                        Some(Val::Index(..)) | None => None,
                        Some(val) => Some(val),
                    }
                } else {
                    Some(then)
                };
                let else_2 = if let Val::Index(index, _) = else_ {
                    match vars.get(index) {
                        Some(Val::Index(..)) | None => None,
                        Some(val) => Some(val),
                    }
                } else {
                    Some(else_)
                };

                match (cond2, then2, else_2) {
                    (None, _, _) => {
                        new.push(instruction.clone(), *assign);
                        continue;
                    }
                    (Some(cond), None, None) => {
                        if cond != 0 {
                            then.clone()
                        } else {
                            else_.clone()
                        }
                    }
                    (Some(cond), None, Some(else_)) => {
                        if cond == 0 {
                            else_.clone()
                        } else {
                            then.clone()
                        }
                    }
                    (Some(cond), Some(then), None) => {
                        if cond != 0 {
                            then.clone()
                        } else {
                            else_.clone()
                        }
                    }
                    (Some(cond), Some(then), Some(else_)) => {
                        if cond != 0 {
                            then.clone()
                        } else {
                            else_.clone()
                        }
                    }
                }
            }
            Instruction::LXor(left, right) => {
                super::check!(BINARY left, right, new, vars, assign, instruction);
                Val::Bool((left != 0) ^ (right != 0))
            }
            Instruction::Ref(_) => todo!(),
            Instruction::Deref(_) => todo!(),
        };
        vars.insert(assign.unwrap(), evaluated);
    }
    new
}

#[macro_export]
macro_rules! check {
    ($val:ident, $new: ident, $vars: ident, $assign: ident, $instruction: ident) => {
        let $val = if let Val::Index(index, _) = $val {
            match $vars.get(index) {
                Some(Val::Index(..)) | None => {
                    $new.push($instruction.clone(), *$assign);
                    continue;
                }
                Some(val) => val.get_int(),
            }
        } else {
            $val.get_int()
        };
    };
    (2 $val:ident, $new: ident, $vars: ident, $assign: ident, $instruction: ident) => {{
        if let Val::Index(index, _) = $val {
            match $vars.get(index) {
                Some(Val::Index(..)) | None => (),
                Some(a) => {
                    let new_ins = match $instruction.clone() {
                        Instruction::Neg(_) => Instruction::Neg(a.clone()),
                        Instruction::Inc(_) => Instruction::Inc(a.clone()),
                        Instruction::Dec(_) => Instruction::Dec(a.clone()),
                        Instruction::LNot(_) => Instruction::LNot(a.clone()),
                        Instruction::BNot(_) => Instruction::BNot(a.clone()),
                        Instruction::Print(_) => Instruction::Print(a.clone()),
                        Instruction::Ascii(_) => Instruction::Ascii(a.clone()),
                        _ => unreachable!(),
                    };
                    $new.push(new_ins, *$assign);
                    continue;
                }
            }
        }
        $new.push($instruction.clone(), *$assign);
        continue;
    }};
    (VAL $val:ident, $new: ident, $vars: ident, $assign: ident, $instruction: ident) => {
        let $val = if let Val::Index(index, _) = $val {
            match $vars.get(index) {
                Some(Val::Index(..)) | None => {
                    $new.push($instruction.clone(), *$assign);
                    continue;
                }
                Some(val) => val,
            }
        } else {
            $val
        };
    };
    (BINARY $left:ident, $right: ident, $new: ident, $vars: ident, $assign: ident, $instruction: ident) => {
        let $left = if let Val::Index(index, _) = $left {
            match $vars.get(index) {
                Some(Val::Index(..)) | None => None,
                Some(val) => Some(val.clone()),
            }
        } else {
            Some($left.clone())
        };
        let $right = if let Val::Index(index, _) = $right {
            match $vars.get(index) {
                Some(Val::Index(..)) | None => None,
                Some(val) => Some(val.clone()),
            }
        } else {
            Some($right.clone())
        };
        let ($left, $right) = match ($left, $right) {
            (Some(a), Some(b)) => (a.get_int(), b.get_int()),
            (Some(a), None) => {
                let new_ins = match $instruction.clone() {
                    Instruction::Add(_, b) => Instruction::Add(a, b),
                    Instruction::Sub(_, b) => Instruction::Sub(a, b),
                    Instruction::Mul(_, b) => Instruction::Mul(a, b),
                    Instruction::Div(_, b) => Instruction::Div(a, b),
                    Instruction::Mod(_, b) => Instruction::Mod(a, b),
                    Instruction::Lt(_, b) => Instruction::Lt(a, b),
                    Instruction::Le(_, b) => Instruction::Le(a, b),
                    Instruction::Eq(_, b) => Instruction::Eq(a, b),
                    Instruction::Neq(_, b) => Instruction::Neq(a, b),
                    Instruction::LAnd(_, b) => Instruction::LAnd(a, b),
                    Instruction::LOr(_, b) => Instruction::LOr(a, b),
                    Instruction::Pow(_, b) => Instruction::Pow(a, b),
                    Instruction::Shl(_, b) => Instruction::Shl(a, b),
                    Instruction::Shr(_, b) => Instruction::Shr(a, b),
                    Instruction::BAnd(_, b) => Instruction::BAnd(a, b),
                    Instruction::BOr(_, b) => Instruction::BOr(a, b),
                    Instruction::BXor(_, b) => Instruction::BXor(a, b),
                    Instruction::LXor(_, b) => Instruction::LXor(a, b),
                    _ => unreachable!(),
                };
                $new.push(new_ins, *$assign);
                continue;
            }
            (None, Some(b)) => {
                let new_ins = match $instruction.clone() {
                    Instruction::Add(a, _) => Instruction::Add(a, b),
                    Instruction::Sub(a, _) => Instruction::Sub(a, b),
                    Instruction::Mul(a, _) => Instruction::Mul(a, b),
                    Instruction::Div(a, _) => Instruction::Div(a, b),
                    Instruction::Mod(a, _) => Instruction::Mod(a, b),
                    Instruction::Lt(a, _) => Instruction::Lt(a, b),
                    Instruction::Le(a, _) => Instruction::Le(a, b),
                    Instruction::Eq(a, _) => Instruction::Eq(a, b),
                    Instruction::Neq(a, _) => Instruction::Neq(a, b),
                    Instruction::LAnd(a, _) => Instruction::LAnd(a, b),
                    Instruction::LOr(a, _) => Instruction::LOr(a, b),
                    Instruction::Pow(a, _) => Instruction::Pow(a, b),
                    Instruction::Shl(a, _) => Instruction::Shl(a, b),
                    Instruction::Shr(a, _) => Instruction::Shr(a, b),
                    Instruction::BAnd(a, _) => Instruction::BAnd(a, b),
                    Instruction::BOr(a, _) => Instruction::BOr(a, b),
                    Instruction::BXor(a, _) => Instruction::BXor(a, b),
                    Instruction::LXor(a, _) => Instruction::LXor(a, b),
                    _ => unreachable!(),
                };
                $new.push(new_ins, *$assign);
                continue;
            }
            (None, None) => {
                $new.push($instruction.clone(), *$assign);
                continue;
            }
        };
    };
    (BINARY2 $left:ident, $right: ident, $new: ident, $vars: ident, $assign: ident, $instruction: ident) => {{
        let $left = if let Val::Index(index, _) = $left {
            match $vars.get(index) {
                Some(Val::Index(..)) | None => None,
                Some(val) => Some(val.clone()),
            }
        } else {
            Some($left.clone())
        };
        let $right = if let Val::Index(index, _) = $right {
            match $vars.get(index) {
                Some(Val::Index(..)) | None => None,
                Some(val) => Some(val.clone()),
            }
        } else {
            Some($right.clone())
        };
        match ($left, $right) {
            (Some(a), None) => {
                let new_ins = match $instruction.clone() {
                    Instruction::Add(_, b) => Instruction::Add(a, b),
                    Instruction::Sub(_, b) => Instruction::Sub(a, b),
                    Instruction::Mul(_, b) => Instruction::Mul(a, b),
                    Instruction::Div(_, b) => Instruction::Div(a, b),
                    Instruction::Mod(_, b) => Instruction::Mod(a, b),
                    Instruction::Lt(_, b) => Instruction::Lt(a, b),
                    Instruction::Le(_, b) => Instruction::Le(a, b),
                    Instruction::Eq(_, b) => Instruction::Eq(a, b),
                    Instruction::Neq(_, b) => Instruction::Neq(a, b),
                    Instruction::LAnd(_, b) => Instruction::LAnd(a, b),
                    Instruction::LOr(_, b) => Instruction::LOr(a, b),
                    Instruction::Pow(_, b) => Instruction::Pow(a, b),
                    Instruction::Shl(_, b) => Instruction::Shl(a, b),
                    Instruction::Shr(_, b) => Instruction::Shr(a, b),
                    Instruction::BAnd(_, b) => Instruction::BAnd(a, b),
                    Instruction::BOr(_, b) => Instruction::BOr(a, b),
                    Instruction::BXor(_, b) => Instruction::BXor(a, b),
                    Instruction::LXor(_, b) => Instruction::LXor(a, b),
                    _ => unreachable!(),
                };
                $new.push(new_ins, *$assign);
                continue;
            }
            (None, Some(b)) => {
                let new_ins = match $instruction.clone() {
                    Instruction::Add(a, _) => Instruction::Add(a, b),
                    Instruction::Sub(a, _) => Instruction::Sub(a, b),
                    Instruction::Mul(a, _) => Instruction::Mul(a, b),
                    Instruction::Div(a, _) => Instruction::Div(a, b),
                    Instruction::Mod(a, _) => Instruction::Mod(a, b),
                    Instruction::Lt(a, _) => Instruction::Lt(a, b),
                    Instruction::Le(a, _) => Instruction::Le(a, b),
                    Instruction::Eq(a, _) => Instruction::Eq(a, b),
                    Instruction::Neq(a, _) => Instruction::Neq(a, b),
                    Instruction::LAnd(a, _) => Instruction::LAnd(a, b),
                    Instruction::LOr(a, _) => Instruction::LOr(a, b),
                    Instruction::Pow(a, _) => Instruction::Pow(a, b),
                    Instruction::Shl(a, _) => Instruction::Shl(a, b),
                    Instruction::Shr(a, _) => Instruction::Shr(a, b),
                    Instruction::BAnd(a, _) => Instruction::BAnd(a, b),
                    Instruction::BOr(a, _) => Instruction::BOr(a, b),
                    Instruction::BXor(a, _) => Instruction::BXor(a, b),
                    Instruction::LXor(a, _) => Instruction::LXor(a, b),
                    _ => unreachable!(),
                };
                $new.push(new_ins, *$assign);
                continue;
            }
            (None, None) | (Some(_), Some(_)) => {
                $new.push($instruction.clone(), *$assign);
                continue;
            }
        };
    }};
}
