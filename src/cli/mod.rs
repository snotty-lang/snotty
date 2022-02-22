mod cmd_args;
use std::{fs, io::ErrorKind, process};

use cmd_args::Args;

pub fn main() {
    let args = match Args::get() {
        Ok(args) => args,
        Err(err) => {
            println!("{}", err);
            process::exit(1);
        }
    };

    let output = ezlang::run(
        &fs::read_to_string(&args.input_file).unwrap_or_else(|e| {
            match e.kind() {
                ErrorKind::NotFound => println!("File not found: {}", args.input_file),
                ErrorKind::PermissionDenied => {
                    println!("Cannot open file '{}': Permission denied", args.input_file)
                }
                _ => println!("An error occured: {}", e),
            }
            process::exit(1);
        }),
        args.input_file,
    )
    .unwrap_or_else(|e| {
        println!("{}", e);
        process::exit(1);
    });

    fs::write(&args.output_file, output).unwrap_or_else(|e| {
        match e.kind() {
            ErrorKind::PermissionDenied => {
                println!("Cannot open file '{}': Permission denied", args.output_file)
            }
            _ => println!("An error occured: {}", e),
        }
        process::exit(1);
    });
}
