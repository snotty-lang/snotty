use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = fs::read_to_string("ez-modules/test.ez")?;
    if let Err(err) = snotty::parser::parse(&file) {
        println!("{}", err.with_path("ez-modules/test.ez"));
    }
    Ok(())
}
