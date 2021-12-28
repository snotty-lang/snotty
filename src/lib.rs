//! A language, which doesn't have much. But, It can be compiled to brainfuck.

/// Contains the code transpiler, which generates the Brainfuck code
pub mod compiler;

/// Contains the `evaluate` function, which does constant time evaluation of the code.
pub mod evaluate;

/// Contains the Intermediate code generator
pub mod ir_code;

/// Contains the Lexer struct
pub mod lexer;

/// Contains the Parser struct
pub mod parser;
pub mod utils;

use std::fs;
use std::io::Result as IoResult;

use utils::Error;

/// Reads and parses the passed file, and returns a the generated brainfuck code or an error, if any
/// # Arguments
/// * `contents` - The contents to be parsed
/// # Returns
/// * `Result<String, crate::utils::Error>` - The generated brainfuck code or an error, if any
/// # Examples
/// ```
/// use ezlang;
///
/// let code = ezlang::run("ezout 5 + 7");
/// assert!(code.is_ok());
/// assert_eq!(&code.unwrap(), "[-]+++++++++++++++++++++++++++++++++++++++++++++++++.[-]++++++++++++++++++++++++++++++++++++++++++++++++++.[-]++++++++++.");
/// ```
pub fn run(contents: &str) -> Result<String, Error> {
    let tokens = lexer::lex(contents)?;
    let ast = parser::parse(tokens)?;
    let code = ir_code::generate_code(ast);
    println!("{}", code);
    let code = evaluate::evaluate(&code);
    Ok(compiler::transpile(&code))
}

/// Reads the contents from the passed file
/// # Arguments
/// * `filename` - The file to be read
/// # Returns
/// * `Result<String, io::Error>` - The contents of the file or an error, if any
#[inline(always)]
pub fn get_contents(filename: &str) -> IoResult<String> {
    fs::read_to_string(filename)
}
