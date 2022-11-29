use std::ops::Index;

mod sealed {
    pub trait TupleCode: std::marker::Copy + std::fmt::Debug {}
    pub trait TupleArgument: std::marker::Copy + std::fmt::Debug {}
}

/// An implementation based on https://cs.lmu.edu/~ray/notes/squid/
pub struct Tuple<T: sealed::TupleCode, U: sealed::TupleArgument> {
    code: T,
    len: usize,
    arguments: [Option<U>; 3],
}

impl<T, U> Tuple<T, U>
where
    T: sealed::TupleCode,
    U: sealed::TupleArgument,
{
    pub const fn new(code: T) -> Tuple<T, U> {
        Tuple {
            code,
            len: 0,
            arguments: [None; 3],
        }
    }

    pub const fn push(mut self, arg: U) -> Self {
        self.arguments[self.len] = Some(arg);
        self.len += 1;
        self
    }

    pub const fn code(&self) -> T {
        self.code
    }
}

impl<T, U> Index<usize> for Tuple<T, U>
where
    T: sealed::TupleCode,
    U: sealed::TupleArgument,
{
    type Output = U;
    fn index(&self, index: usize) -> &Self::Output {
        self.arguments.get(index).unwrap().as_ref().unwrap()
    }
}

/// Instruction code for Tuple-Based HIR
#[derive(Clone, Copy, Debug)]
pub enum HIRCode {}
impl sealed::TupleCode for HIRCode {}

/// Argument for Tuple-Based HIR
#[derive(Clone, Copy, Debug)]
pub enum HIRArgument {
    /// Reference to a subroutine in compiler's memory
    Subroutine(usize),

    /// Reference to the location in the list of tuples
    Label(usize),

    /// Literal
    Literal { literal: usize, size: usize },

    /// Variable
    Variable {
        /// location in memory
        location: usize,
        size: usize,
    },

    /// Pointer to a memory location
    MemPtr {
        /// location pointing to
        location: usize,
        size: usize,
    },
}

impl sealed::TupleArgument for HIRArgument {}

/// A subroutine
pub enum Subroutine {
    User {
        /// level of nesting
        level: usize,
        /// parent subroutine
        parent: usize,
        /// local variables declared inside subroutine
        locals: Vec<usize>,
        /// parameters to the subroutine
        parameters: Vec<usize>,
        /// tuple IR
        tuples: Vec<Tuple<HIRCode, HIRArgument>>,
    },
    External {
        /// id of the subroutine
        id: usize,
    },
}
