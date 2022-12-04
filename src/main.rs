use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let contents = fs::read_to_string("test.snt")?;
    match snotty::compile("test.snt".to_string(), &contents) {
        Err(errors) => {
            for err in errors {
                eprintln!("{}\n", err)
            }
        }
        Ok(res) => {
            println!(
                "\n----------------------\nCode executed with output: {}",
                res
            );
        }
    }
    Ok(())
}
