use super::utils::Instructions;

pub fn transpile(code: &Instructions) {
    for instruction in &code.instructions {
        println!("{}", instruction);
    }
}
