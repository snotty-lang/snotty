#![allow(clippy::unnecessary_operation)]

use super::utils::{Instruction, Instructions, Val};

/// Compiles the 3-address code into brainfuck code.
pub fn transpile(code: &Instructions) -> String {
    use super::goto_add;
    let mut location = 0;
    let mut free_idx = 0;
    let mut bf_code = String::new();
    for (assign, instruction) in &code.0 {
        if let Some(val) = assign {
            goto(&mut bf_code, &mut location, *val);
            free_idx = *val + 1;
        };

        match instruction {
            Instruction::Input => {
                bf_code.push(',');
            }
            Instruction::Print(val) => {
                let start = location;
                bf_code.push_str(">>");
                goto(&mut bf_code, &mut location, free_idx);
                goto_add!(val, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, free_idx, location);
                    goto(&mut bf_code, &mut location, free_idx);
                });
                bf_code.push_str(">+[[-]<[->+<[->+<[->+<[->+<[->+<[->+<[->+<[->+<[->+<[->[-]>>+>+<<<]]]]]]]]]<]>>[>]++++++[-<++++++++>]>>]<<<[.[-]<<<]");
                bf_code.push_str("++++++++++.----------");
                goto(&mut bf_code, &mut location, start);
            }
            Instruction::Ascii(val) => {
                let start = location;
                goto(&mut bf_code, &mut location, free_idx);
                goto_add!(
                    val,
                    &mut bf_code,
                    &mut location,
                    {
                        bf_code.push('.');
                    },
                    {
                        bf_code.push_str(".[-]");
                    }
                );
                goto(&mut bf_code, &mut location, start);
            }
            Instruction::Inc(val) => {
                goto_add!(val, &mut bf_code, &mut location, {});
                bf_code.push('+');
            }
            Instruction::Dec(val) => {
                goto_add!(val, &mut bf_code, &mut location, {});
                bf_code.push('-');
            }
            Instruction::Neg(val) => {
                Val::Num(-val.get_int());
            }
            Instruction::BNot(val) => {
                Val::Num(!val.get_int());
            }
            Instruction::Add(left, right) => {
                let start = location;
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location);
                });
                goto(&mut bf_code, &mut location, start + 1);
                goto_add!(right, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start + 1, location);
                });
                goto(&mut bf_code, &mut location, start + 1);
                bf_code.push_str("[-<+>]<");
                location -= 1;
            }
            Instruction::Sub(left, right) => {
                let start = location;
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location);
                });
                goto(&mut bf_code, &mut location, start + 1);
                goto_add!(right, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start + 1, location);
                });
                goto(&mut bf_code, &mut location, start + 1);
                bf_code.push_str("[-<->]<");
                location -= 1;
            }
            Instruction::Pow(left, Val::Num(2)) => {
                let start = location;
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location);
                    goto(&mut bf_code, &mut location, start);
                });
                bf_code.push_str(">[-]>[-]<<[>+<-]>[-[>+<<++>-]<+>>[<+>-]<]<");
            }
            Instruction::Pow(left, right) => {
                if right.get_int() < 0 {
                    Val::Num(1 / left.get_int().pow(-right.get_int() as u32))
                } else {
                    Val::Num(left.get_int().pow(right.get_int() as u32))
                };
            }
            Instruction::LAnd(left, right) => {
                Val::Bool(left.get_int() != 0 && right.get_int() != 0);
            }
            Instruction::LNot(val) => {
                Val::Bool(val.get_int() == 0);
            }
            Instruction::Mul(left, right) => {
                let start = location;
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location);
                });
                goto(&mut bf_code, &mut location, start + 1);
                goto_add!(right, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start + 1, location);
                });
                goto(&mut bf_code, &mut location, start);

                bf_code.push_str(">>[-]>[-]<<< [>>>+<<<-]>>>[<<[<+>>+<-]>[<+>-]>-]<<<");
            }
            Instruction::Div(left, right) => {
                Val::Num(left.get_int() / right.get_int());
            }
            Instruction::Mod(left, right) => {
                Val::Num(left.get_int() % right.get_int());
            }
            Instruction::Eq(left, right) => {
                Val::Bool(left.get_int() == right.get_int());
            }
            Instruction::Neq(left, right) => {
                Val::Bool(left.get_int() != right.get_int());
            }
            Instruction::Lt(left, right) => {
                Val::Bool(left.get_int() < right.get_int());
            }
            Instruction::Gt(left, right) => {
                Val::Bool(left.get_int() > right.get_int());
            }
            Instruction::Le(left, right) => {
                Val::Bool(left.get_int() <= right.get_int());
            }
            Instruction::Ge(left, right) => {
                Val::Bool(left.get_int() >= right.get_int());
            }
            Instruction::LOr(left, right) => {
                Val::Bool(left.get_int() != 0 || right.get_int() != 0);
            }
            Instruction::Shl(left, right) => {
                Val::Num(left.get_int() << right.get_int());
            }
            Instruction::Shr(left, right) => {
                Val::Num(left.get_int() >> right.get_int());
            }
            Instruction::BAnd(left, right) => {
                Val::Num(left.get_int() & right.get_int());
            }
            Instruction::BOr(left, right) => {
                Val::Num(left.get_int() | right.get_int());
            }
            Instruction::BXor(left, right) => {
                Val::Num(left.get_int() ^ right.get_int());
            }
            Instruction::Copy(from) => {
                if let Val::Index(index, _) = from {
                    goto(&mut bf_code, &mut location, *index);
                }
            }
            // Instruction::If(_, _, _) => {}
            _ => todo!(),
        };

        match assign {
            Some(val) if *val != location => {
                dbg!(val, location);
                copy(&mut bf_code, location, *val, location)
            }
            _ => (),
        };
    }
    bf_code
}

/// Goes from the `from` location to the `to` location
fn goto(bf_code: &mut String, from: &mut usize, to: usize) {
    let diff = *from as i32 - to as i32;
    if diff < 0 {
        bf_code.push_str(&">".repeat(-diff as usize))
    } else {
        bf_code.push_str(&"<".repeat(diff as usize))
    }
    *from = to;
}

/// Goes to the location or adds to the current location
#[macro_export]
macro_rules! goto_add {
    ($val: expr, $bf_code: expr, $current: expr, $block:block) => {
        match $val {
            Val::Num(val) => {
                $bf_code.push_str(&("+".repeat(*val as u32 as usize)));
            }
            Val::Bool(b) => {
                $bf_code.push_str(&("+".repeat(*b as u32 as usize)));
            }
            Val::Index(index, _) => {
                goto($bf_code, $current, *index);
                $block
            }
            Val::None => {}
        }
    };
    ($val: expr, $bf_code: expr, $current: expr, $block:block, $block2: block) => {
        match $val {
            Val::Num(val) => {
                $bf_code.push_str(&("+".repeat(*val as u32 as usize)));
                $block2
            }
            Val::Bool(b) => {
                $bf_code.push_str(&("+".repeat(*b as u32 as usize)));
                $block2
            }
            Val::Index(index, _) => {
                goto($bf_code, $current, *index);
                $block
            }
            Val::None => {}
        }
    };
}

/// Copies the value from the `from` location to the `to` location. Uses the current location as a reference
fn copy(bf_code: &mut String, from: usize, to: usize, mut current: usize) {
    if current == to {
        return;
    }
    let start = current;
    goto(bf_code, &mut current, to);
    bf_code.push_str(">[-]<[-]");
    goto(bf_code, &mut current, from);
    bf_code.push('[');
    goto(bf_code, &mut current, to);
    bf_code.push_str("+>+<");
    goto(bf_code, &mut current, from);
    bf_code.push_str("-]");
    goto(bf_code, &mut current, to);
    bf_code.push_str(">[<");
    goto(bf_code, &mut current, from);
    bf_code.push('+');
    goto(bf_code, &mut current, to);
    bf_code.push_str(">-]<");
    goto(bf_code, &mut current, start);
}
