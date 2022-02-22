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

/// Contains the Preprocessor
pub mod preprocessor;

/// Optimizes the generated IR code
pub mod ir_optimizer;
