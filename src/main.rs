use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let contents = fs::read_to_string("test.snt")?;
    match snotty::run("test.snt".to_string(), &contents) {
        Err(errors) => {
            for err in errors {
                println!("{}\n", err)
            }
        }
        Ok(compiled) => {
            fs::write("compiled.c", compiled)?;
        }
    }
    Ok(())
}
