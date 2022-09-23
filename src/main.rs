use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = fs::read_to_string("ez-modules/test.ez")?;
    ezlang::parser::parse(&file).unwrap();
    Ok(())
}
