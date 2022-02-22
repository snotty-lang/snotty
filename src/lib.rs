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

pub mod core;
pub mod utils;

use std::rc::Rc;

use crate::core::{compiler, ir_code, lexer, parser, preprocessor};
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
pub fn run(contents: &str, filename: String) -> Result<String, Error> {
    let tokens = lexer::lex(contents, Rc::new(filename))?;
    let tokens = preprocessor::preprocess(tokens)?;
    // println!(
    //     "{:?}",
    //     tokens
    //         .iter()
    //         .map(|x| x.to_string())
    //         .collect::<Vec<String>>()
    // );
    let ast = parser::parse(tokens)?;
    println!("{}\n", ast);
    let code = ir_code::generate_code(ast)?;
    println!("{}", code);
    // let code = evaluate::evaluate(&code);
    // let code = ir_optimizer::optimize(&code);
    // println!("{}", code);
    let mut bf_code = compiler::transpile(&code);
    optimize(&mut bf_code);
    Ok(bf_code)
}

/// Optimizes the generated Brainfuck code by removing unnecessary characters
fn optimize(code: &mut String) {
    while code.contains("<>") || code.contains("><") || code.contains("+-") || code.contains("-+") {
        *code = code
            .replace("<>", "")
            .replace("><", "")
            .replace("+-", "")
            .replace("-+", "");
    }
}
