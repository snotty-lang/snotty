//! A language, which doesn't have much. But, It can be compiled to brainfuck.
//! To get started, run the following code:
//! ```
//! println!("{}", ezlang::run("ezout 2 + 2").unwrap());
//! ```
//! This should output the following code:
//! ```
//! [-]++++++++++++++++++++++++++++++++++++++++++++++++++++.[-]++++++++++.
//! ```
//!
//! To compile this brainfuck code into machine code, you can use this <a href=https://github.com/Alumin112/BrainFuck-Compiler/>compiler</a>.
//!
//! You can use the official ezlang compiler from <a href=https://github.com/Alumin112/ezlang/>here</a>
//!
//! There is also the ez macro
//! ```
//! use ezlang;
//!
//! ezlang::ez!(
//!     let y = 454 ezout y / 10
//! )
//! ```
//! It is the run function but in macro style

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

/// Optimizes the generated IR code
pub mod ir_optimizer;
pub mod utils;

use std::fs;
use std::process;

use utils::Error;

/// parses the passed ezlang code, and returns a the generated brainfuck code or an error, if any
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
pub fn run(contents: &str, filename: &'static str) -> Result<String, Error> {
    let tokens = lexer::lex(contents, filename)?;
    // println!("{:?}", tokens.iter().map(|x| x.to_string()).collect::<Vec<String>>());
    let ast = parser::parse(tokens)?;
    println!("{}", ast);
    let code = ir_code::generate_code(ast)?;
    println!("{}", code.0);
    // let code = evaluate::evaluate(&code);
    // let code = ir_optimizer::optimize(&code);
    // println!("{}", code);
    Ok(compiler::transpile(&code.0, code.1))
}

/// Reads and compiles the content of the passed file and returns it
/// In case of an error, it exits the program
#[inline(always)]
pub fn compile(filename: &'static str) -> String {
    run(
        &fs::read_to_string(filename).unwrap_or_else(|e| {
            println!("{}", e);
            process::exit(1);
        }),
        filename,
    )
    .unwrap_or_else(|e| {
        println!("{}", e);
        process::exit(1);
    })
}

/// Same as `run`, but a macro
#[macro_export]
macro_rules! ez {
    ($($text: tt)*) => {{
        let mut code = String::new();
        $(
            code.push(' ');
            code.push_str(stringify!($text));
        )*
        ezlang::run(&code)
    }};
}
