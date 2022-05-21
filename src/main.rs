mod cli;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // cli::main();
    use std::fs;
    let output = ezlang::run(
        &fs::read_to_string("ez-modules/test.ez").unwrap(),
        "ez-modules/test.ez".to_string(),
    )?;
    fs::write("output.bf", output).unwrap();
    Ok(())
}
