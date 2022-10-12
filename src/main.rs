use std::fs;

fn main() -> Result<(), std::boxed::Box<dyn std::error::Error>> {
    let contents = fs::read_to_string("test.snt")?;
    match snotty::compile("test.snt".to_string(), &contents) {
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
