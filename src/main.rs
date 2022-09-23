use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = fs::read_to_string("ez-modules/test.ez")?;
    if let Err(err) = ezlang::parser::parse(&file) {
        println!("{}", err);
    }
    Ok(())
}
