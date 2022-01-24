use std::fs;

fn main() {
    fs::write("db.bf", ezlang::compile("test.ez".to_owned())).unwrap();
}
