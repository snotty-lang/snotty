use super::utils::{Instructions, Operator, Val};

pub fn transpile(instructions: &Instructions) {
    let mut code = String::new();
    for instruction in &instructions.instructions {
        let left = match instruction.arg1 {
            Val::Num(num) => num,
            Val::Bool(b) => b as i32,
            Val::Index(_) => continue,
        } as usize;

        match instruction.assign {
            Some(Val::Index(index)) => index,
            None => match instruction.op {
                Operator::Print | Operator::Ascii => {
                    code.push_str("[-]");
                    code.push_str(&("+".repeat(left) + "."));
                    continue;
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };

        let right = match instruction.arg2 {
            Some(Val::Num(num)) => num,
            _ => continue,
        } as usize;

        code.push_str("[-]");
        code.push_str(&match instruction.op {
            Operator::Add => "+".repeat(left + right),
            Operator::Sub => "+".repeat(left - right),
            Operator::Mul => "+".repeat(left * right),
            Operator::Div => "+".repeat(left / right),
            Operator::Mod => "+".repeat(left % right),
            // Operator::Neg => -left,
            _ => unreachable!(),
        });
    }
    println!("{}", code);
}
