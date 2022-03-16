//! Includes all the utils functions and structs

/// Contains the Error struct
mod error;

/// Contains the Instruction and Instructions structs
mod instructions;

/// Contains the Node enum
mod node;

/// Contains the Scope struct
mod scope;

/// Contains the Token struct
mod token;

/// Models the memory in brainfuck
mod memory_model;

pub use error::*;
pub use instructions::*;
pub use memory_model::*;
pub use node::*;
pub use scope::*;
pub use token::*;

pub type LexNumber = u16;
pub type ValNumber = i8;
pub const NONE_SIZE: usize = 0;
pub const POINTER_SIZE: usize = 2;
