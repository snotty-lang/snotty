use std::env;

pub struct Args {
    pub output_file: String,
    pub input_file: String,
}

impl Args {
    pub fn get() -> Result<Args, String> {
        let args = env::args().skip(1);
        let mut output_file = None;
        let mut input_file = None;
        for arg in args {
            match *arg.split('=').collect::<Vec<_>>() {
                ["-o", file] => {
                    if output_file.is_some() {
                        return Err(String::from("Multiple output files specified"));
                    }
                    output_file = Some(file.to_string());
                }
                ["-o"] => return Err(String::from("No output file specified after -o")),
                [file] => {
                    if input_file.is_some() {
                        return Err(String::from("Multiple input files specified"));
                    }
                    input_file = Some(file.to_string());
                }
                _ => return Err(format!("Unknown argument: {}", arg)),
            }
        }

        Ok(Args {
            output_file: output_file.unwrap_or_else(|| String::from("output.bf")),
            input_file: match input_file {
                Some(file) => file,
                None => return Err(String::from("No input file specified")),
            },
        })
    }
}
