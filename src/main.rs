fn main() -> Result<(), Box<dyn std::error::Error>> {
    if let Err(err) = snotty::run("test.snt") {
        println!("{}", err);
    }
    Ok(())
}
