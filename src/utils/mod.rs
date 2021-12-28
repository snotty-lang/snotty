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

pub use error::*;
pub use instructions::*;
pub use node::*;
pub use scope::*;
pub use token::*;
