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

super::gen!(pub type ValNumber = i16);

#[macro_export]
macro_rules! gen {
    (pub type ValNumber = i16) => {
        pub type ValNumber = i16;
        pub type LexNumber = u8;
    };

    (pub type ValNumber = i32) => {
        pub type ValNumber = i32;
        pub type LexNumber = u16;
    };

    (pub type ValNumber = i64) => {
        pub type ValNumber = i64;
        pub type LexNumber = u32;
    };

    (pub type ValNumber = i128) => {
        pub type ValNumber = i128;
        pub type LexNumber = u64;
    };

    (pub type ValNumber = i8) => {
        pub type ValNumber = i8;
        pub type LexNumber = u8;
    };
}
