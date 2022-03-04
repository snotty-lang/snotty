mod cli;

fn main() {
    cli::main();
    // use std::fs;
    // let output = ezlang::run(
    //     &fs::read_to_string("ez-modules/test.ez").unwrap(),
    //     "ez-modules/test.ez".to_string(),
    // )
    // .unwrap();
    // fs::write("output.bf", output).unwrap();
}
