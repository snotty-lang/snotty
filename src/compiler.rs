use super::utils::{Instruction, Instructions, Val};

/// Compiles the 3-address code into brainfuck code.
pub fn transpile(code: &Instructions) -> String {
    let mut arr_idx = 0;
    let mut location = 0;
    let mut bf_code = String::new();
    for (_, instruction) in &code.0 {
        match instruction {
            Instruction::Input => {
                bf_code.push(',');
                arr_idx += 1;
                super::check_cmp!(, bf_code, location, arr_idx);
                continue;
            }
            Instruction::Print(val) | Instruction::Ascii(val) => {
                super::check_cmp!(val, bf_code, location);
                bf_code.push_str("[-]");
                bf_code.push_str(&("+".repeat(val.get_int() as usize) + "."));
                super::check_cmp!(, bf_code, location, arr_idx);
                continue;
            }
            Instruction::Pow(left, right) => {
                if right.get_int() < 0 {
                    Val::Num(1 / left.get_int().pow(-right.get_int() as u32))
                } else {
                    Val::Num(left.get_int().pow(right.get_int() as u32))
                }
            }
            Instruction::LAnd(left, right) => {
                Val::Bool(left.get_int() != 0 && right.get_int() != 0)
            }
            Instruction::LNot(val) => Val::Bool(val.get_int() == 0),
            Instruction::Inc(val) => Val::Num(val.get_int() + 1),
            Instruction::Dec(val) => Val::Num(val.get_int() - 1),
            Instruction::Neg(val) => Val::Num(-val.get_int()),
            Instruction::BNot(val) => Val::Num(!val.get_int()),
            Instruction::Add(left, right) => Val::Num(left.get_int() + right.get_int()),
            Instruction::Sub(left, right) => Val::Num(left.get_int() - right.get_int()),
            Instruction::Mul(left, right) => Val::Num(left.get_int() * right.get_int()),
            Instruction::Div(left, right) => Val::Num(left.get_int() / right.get_int()),
            Instruction::Mod(left, right) => Val::Num(left.get_int() % right.get_int()),
            Instruction::Eq(left, right) => Val::Bool(left.get_int() == right.get_int()),
            Instruction::Neq(left, right) => Val::Bool(left.get_int() != right.get_int()),
            Instruction::Lt(left, right) => Val::Bool(left.get_int() < right.get_int()),
            Instruction::Gt(left, right) => Val::Bool(left.get_int() > right.get_int()),
            Instruction::Le(left, right) => Val::Bool(left.get_int() <= right.get_int()),
            Instruction::Ge(left, right) => Val::Bool(left.get_int() >= right.get_int()),
            Instruction::LOr(left, right) => Val::Bool(left.get_int() != 0 || right.get_int() != 0),
            Instruction::Shl(left, right) => Val::Num(left.get_int() << right.get_int()),
            Instruction::Shr(left, right) => Val::Num(left.get_int() >> right.get_int()),
            Instruction::BAnd(left, right) => Val::Num(left.get_int() & right.get_int()),
            Instruction::BOr(left, right) => Val::Num(left.get_int() | right.get_int()),
            Instruction::BXor(left, right) => Val::Num(left.get_int() ^ right.get_int()),
        };

        arr_idx += 1;
        super::check_cmp!(, bf_code, location, arr_idx);
    }
    bf_code
}

#[macro_export]
macro_rules! check_cmp {
    ($val:ident, $bf_code: ident, $location: expr) => {
        if let Val::Index(index) = $val {
            super::check_cmp!(,$bf_code, $location, *index);
        }
    };

    (,$bf_code: ident, $location: expr, $index: expr) => {
        let diff = $location as i32 - $index as i32;
        if diff < 0 {
            $bf_code.push_str(&"<".repeat(-diff as usize))
        } else {
            $bf_code.push_str(&">".repeat(diff as usize))
        }
        $location = $index;
    }
}
