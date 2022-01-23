use crate::utils::{Instruction, Instructions, Val};

/// Compiles the 3-address code into brainfuck code.
pub fn transpile(code: &Instructions) -> String {
    use super::goto_add;
    let mut location = 0;
    let mut bf_code = String::new();
    for (assign, instruction) in &code.0 {
        let free_idx = assign.1;
        let size = if let Some((val, size)) = assign.0 {
            goto(&mut bf_code, &mut location, val);
            size
        } else {
            1
        };
        let start = location;
        match instruction {
            Instruction::Input => {
                bf_code.push(',');
            }
            Instruction::Print(val) => {
                goto(&mut bf_code, &mut location, free_idx);
                goto_add!(val, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, free_idx, location, free_idx + 1);
                    goto(&mut bf_code, &mut location, free_idx);
                });
                bf_code.push_str(">>++++++++++<<[->+>-[>+>>]>[+[-<+>]>+>>]<<<<<<]>>[-]>>>++++++++++<[->-[>+>>]>[+[-<+>]>+>>]<<<<<]>[-]>>[>++++++[-<++++++++>]<.<<+>+>[-]]<[<[->-<]++++++[->++++++++<]>.[-]]<<++++++[-<++++++++>]<.[-]<<[-<+>]<[-]");
                goto(&mut bf_code, &mut location, start);
            }
            Instruction::Ascii(val) => {
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
                goto_add!(val, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location, free_idx);
                    goto(&mut bf_code, &mut location, start);
                });
                bf_code.push_str(">[-]<[>-<-]>[<->+]<");
            }
            Instruction::BNot(val) => {
                goto_add!(val, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location, free_idx);
                    goto(&mut bf_code, &mut location, start);
                });
                bf_code.push_str(">[-]<[>-<-]>[<->+]<-");
            }
            Instruction::Add(left, right) => {
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location, free_idx);
                });
                goto(&mut bf_code, &mut location, start + size);
                goto_add!(right, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + size,
                        location,
                        free_idx + size,
                    );
                    goto(&mut bf_code, &mut location, start + size);
                });
                bf_code.push_str("[-<+>]<");
                location -= 1;
            }
            Instruction::Sub(left, right) => {
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location, free_idx);
                });
                goto(&mut bf_code, &mut location, start + size);
                goto_add!(right, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + size,
                        location,
                        free_idx + size,
                    );
                    goto(&mut bf_code, &mut location, start + size);
                });
                bf_code.push_str("[-<->]<");
                location -= 1;
            }
            Instruction::Pow(left, Val::Num(2)) => {
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location, free_idx);
                    goto(&mut bf_code, &mut location, start);
                });
                bf_code.push_str(">[-]>[-]<<[>+<-]>[-[>+<<++>-]<+>>[<+>-]<]<");
            }
            Instruction::Pow(left, right) => {
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location, free_idx);
                });
                goto(&mut bf_code, &mut location, start + size);
                goto_add!(right, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + size,
                        location,
                        free_idx + size,
                    );
                });
                goto(&mut bf_code, &mut location, start);
                bf_code.push_str(">>[-]<<[>>+<<-]+>[>>[-]>[-]<<<<[>>>>+<<<<-]>>>>[<<[<<+>>>+<-]>[<+>-]>-]<<<-]>[-]<<");
            }
            Instruction::LAnd(left, right) => {
                goto(&mut bf_code, &mut location, start + size);
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + size,
                        location,
                        free_idx + size,
                    );
                });
                goto(&mut bf_code, &mut location, start + 2 * size);
                goto_add!(right, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + 2 * size,
                        location,
                        free_idx + 2 * size,
                    );
                });
                goto(&mut bf_code, &mut location, start);
                bf_code.push_str("[-]>[>[<<+>>-]<-]>[-]<<");
            }
            Instruction::LNot(val) => {
                goto(&mut bf_code, &mut location, start + size);
                goto_add!(val, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + size,
                        location,
                        free_idx + size,
                    );
                });
                goto(&mut bf_code, &mut location, start);
                bf_code.push_str("+>[-<->]<");
            }
            Instruction::Mul(left, right) => {
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location, free_idx);
                });
                goto(&mut bf_code, &mut location, start + size);
                goto_add!(right, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + size,
                        location,
                        free_idx + size,
                    );
                });
                goto(&mut bf_code, &mut location, start);
                bf_code.push_str(">>[-]>[-]<<< [>>>+<<<-]>>>[<<[<+>>+<-]>[<+>-]>-]<<[-]<");
            }
            Instruction::Div(left, right) => {
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location, free_idx);
                });
                goto(&mut bf_code, &mut location, start + size);
                goto_add!(right, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + size,
                        location,
                        free_idx + size,
                    );
                });
                goto(&mut bf_code, &mut location, start);
                bf_code.push_str("[>>+<<-]>>[<[>>+>+<<<-]>>>[<<<+>>>-]<[>+<<-[>>[-]>+<<<-]>>>[<<<+>>>-]<[<-[<<<->>>[-]]+>-]<-]<<<+>>]<[-]<");
            }
            Instruction::Mod(left, right) => {
                goto(&mut bf_code, &mut location, start + size);
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + size,
                        location,
                        free_idx + size,
                    );
                });
                goto(&mut bf_code, &mut location, start + 2 * size);
                goto_add!(right, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + 2 * size,
                        location,
                        free_idx + 2 * size,
                    );
                });
                goto(&mut bf_code, &mut location, start + size);
                bf_code.push_str("[>->+<[>]>[<+>-]<<[<]>-]>[-]>[-<<<+>>>]<<");
                goto(&mut bf_code, &mut location, start);
            }
            Instruction::Eq(left, right) => {
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location, free_idx);
                });
                goto(&mut bf_code, &mut location, start + size);
                goto_add!(right, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + size,
                        location,
                        free_idx + size,
                    );
                });
                goto(&mut bf_code, &mut location, start);
                bf_code.push_str("[->-<]+>[<->[-]]<");
            }
            Instruction::Neq(left, right) => {
                goto(&mut bf_code, &mut location, start + size);
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + size,
                        location,
                        free_idx + size,
                    );
                });
                goto(&mut bf_code, &mut location, start + 2 * size);
                goto_add!(right, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + 2 * size,
                        location,
                        free_idx + 2 * size,
                    );
                });
                goto(&mut bf_code, &mut location, start + size);
                bf_code.push_str("[->-<]+>[<->[-]]< <+>[-<->]");
                goto(&mut bf_code, &mut location, start);
            }
            Instruction::Lt(left, right) => {
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location, free_idx);
                });
                goto(&mut bf_code, &mut location, start + size);
                goto_add!(right, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + size,
                        location,
                        free_idx + size,
                    );
                });
                goto(&mut bf_code, &mut location, start);
                bf_code.push_str(">>[-]>[-]>[-]+>[-]<<<<[>+>+<<-]>[<+>-]<<[>>+<<-]+>>>[>-]>[<<<<->>[-]>>->]<+<<[>-[>-]>[<<<<->>[-]+>>->]<+< <-]<[-]>[-]>[-]>[-]>[-]<<<<<");
            }
            Instruction::Le(left, right) => {
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location, free_idx);
                });
                goto(&mut bf_code, &mut location, start + size);
                goto_add!(right, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + size,
                        location,
                        free_idx + size,
                    );
                });
                goto(&mut bf_code, &mut location, start);
                bf_code.push_str(">>[-]>[-]>[-]+>[-]<<<<[>+>+<<-]>>[<<+>>-]<<<[>>>+<<<-]>>>[>-]>[<<<<+>>[-]>>->]<+<<[>-[>-]>[<<<<+>>[-]+>>->]<+<<-]<[-]>[-]>[-]>[-]>[-]<<<<<");
            }
            Instruction::LOr(left, right) => {
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location, free_idx);
                });
                goto(&mut bf_code, &mut location, start + size);
                goto_add!(right, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + size,
                        location,
                        free_idx + size,
                    );
                });
                goto(&mut bf_code, &mut location, start);
                bf_code.push_str("[>+<-]>[<+>[-]]<");
            }
            Instruction::Shl(left, right) => {
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location, free_idx);
                });
                goto(&mut bf_code, &mut location, start + size);
                goto_add!(right, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + size,
                        location,
                        free_idx + size,
                    );
                });
                goto(&mut bf_code, &mut location, start);
                bf_code.push_str(">[-<[->>+<<]>>[-<<++>>]<]<");
            }
            Instruction::Copy(from) => {
                goto_add!(from, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location, free_idx);
                    goto(&mut bf_code, &mut location, start);
                });
            }
            Instruction::TernaryIf(cond, left, right) => {
                goto(&mut bf_code, &mut location, start + size);
                goto_add!(cond, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + size,
                        location,
                        free_idx + size,
                    );
                });
                goto(&mut bf_code, &mut location, start);
                bf_code.push_str(">>[-]+>[-]<<[<");
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location, free_idx);
                });
                bf_code.push_str(">>-<[>>+<<-]]>>[<<+>>-]<[<<");
                goto_add!(right, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, start, location, free_idx);
                });
                bf_code.push_str(">>-]<[-]<");
            }
            Instruction::LXor(left, right) => {
                goto(&mut bf_code, &mut location, start + size);
                goto_add!(left, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + size,
                        location,
                        free_idx + size,
                    );
                });
                goto(&mut bf_code, &mut location, start + 2 * size);
                goto_add!(right, &mut bf_code, &mut location, {
                    copy(
                        &mut bf_code,
                        location,
                        start + 2 * size,
                        location,
                        free_idx + 2 * size,
                    );
                });
                goto(&mut bf_code, &mut location, start);
                bf_code.push_str("[-]>[>-<-]>[<<+>>[-]]<<");
            }
            Instruction::While(cond) => {
                goto_add!(cond, &mut bf_code, &mut location, {});
                bf_code.push('[');
            }
            Instruction::EndWhile(cond) => {
                goto_add!(cond, &mut bf_code, &mut location, {});
                bf_code.push(']');
            }
            Instruction::If(cond, mem, else_) => {
                goto(&mut bf_code, &mut location, *mem);
                goto_add!(cond, &mut bf_code, &mut location, {
                    copy(&mut bf_code, location, *mem, location, free_idx);
                    goto(&mut bf_code, &mut location, *mem);
                });
                if *else_ {
                    bf_code.push_str(">+<");
                }
                bf_code.push('[');
            }
            Instruction::EndIf(mem, else_) => {
                goto(
                    &mut bf_code,
                    &mut location,
                    if *else_ { *mem + 1 } else { *mem },
                );
                bf_code.push_str("[-]]");
            }
            Instruction::Else(mem) => {
                goto(&mut bf_code, &mut location, *mem);
                bf_code.push_str(">-<[-]]>[<");
                goto(&mut bf_code, &mut location, *mem);
            }
            Instruction::Clear(from, to) => {
                goto(&mut bf_code, &mut location, *from);
                while location < *to {
                    bf_code.push_str("[-]>");
                    location += 1;
                }
                goto(&mut bf_code, &mut location, start);
            }
            _ => todo!(),
        }
        bf_code.push_str("\n");
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
                $bf_code.push_str("[-]");
                if *val < 0 {
                    $bf_code.push_str(&("-".repeat(*val as u32 as usize)));
                } else {
                    $bf_code.push_str(&("+".repeat(*val as u32 as usize)));
                }
            }
            Val::Char(val) => {
                $bf_code.push_str("[-]");
                $bf_code.push_str(&("+".repeat(*val as u32 as usize)));
            }
            Val::Bool(b) => {
                $bf_code.push_str("[-]");
                $bf_code.push_str(&("+".repeat(*b as u32 as usize)));
            }
            Val::Index(index, _) => {
                goto($bf_code, $current, *index);
                $block
            }
            Val::Pointer(ptr, _) => {
                goto($bf_code, $current, *ptr);
                $block
            }
            Val::None => {}
            Val::Array(_, _) => todo!(),
            Val::Function(_, _, _) => todo!(),
        }
    };
    ($val: expr, $bf_code: expr, $current: expr, $block:block, $block2: block) => {
        match $val {
            Val::Num(val) => {
                $bf_code.push_str("[-]");
                if *val < 0 {
                    $bf_code.push_str(&("-".repeat(*val as u32 as usize)));
                } else {
                    $bf_code.push_str(&("+".repeat(*val as u32 as usize)));
                }
                $block2
            }
            Val::Char(val) => {
                $bf_code.push_str("[-]");
                $bf_code.push_str(&("+".repeat(*val as u32 as usize)));
                $block2
            }
            Val::Bool(b) => {
                $bf_code.push_str("[-]");
                $bf_code.push_str(&("+".repeat(*b as u32 as usize)));
                $block2
            }
            Val::Index(index, _) => {
                goto($bf_code, $current, *index);
                $block
            }
            Val::Pointer(ptr, _) => {
                goto($bf_code, $current, *ptr);
                $block
            }
            Val::None => {}
            Val::Array(_, _) => todo!(),
            Val::Function(_, _, _) => todo!(),
        }
    };
}

/// Copies the value from the `from` location to the `to` location. Uses the current location as a reference
fn copy(bf_code: &mut String, from: usize, to: usize, mut current: usize, free: usize) {
    if from == to {
        return;
    }
    assert!(from != free && to != free, "{} {} {}", from, to, free);
    let start = current;
    goto(bf_code, &mut current, free);
    bf_code.push_str("[-]");
    goto(bf_code, &mut current, to);
    bf_code.push_str("[-]");
    goto(bf_code, &mut current, from);
    bf_code.push('[');
    goto(bf_code, &mut current, to);
    bf_code.push('+');
    goto(bf_code, &mut current, free);
    bf_code.push('+');
    goto(bf_code, &mut current, from);
    bf_code.push_str("-]");
    goto(bf_code, &mut current, free);
    bf_code.push_str("[-");
    goto(bf_code, &mut current, from);
    bf_code.push('+');
    goto(bf_code, &mut current, free);
    bf_code.push(']');
    goto(bf_code, &mut current, start);
}
