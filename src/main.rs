use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    match snotty::run("test.snt") {
        Err(err) => println!("{err}"),
        Ok(compiled) => {
            fs::write("compiled.c", compiled)?;
        }
    }
    Ok(())
}
