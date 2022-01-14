use std::fmt;

/// An enum to specify the type of the error.
#[derive(Debug, Clone)]
pub enum ErrorType {
    InvalidLiteral,
    NumberTooLarge,
    SyntaxError,
    UndefinedFunction,
    UndefinedVariable,
    DivisionByZero,
    InvalidReturn,
    TypeError,
    IndexOutOfBounds,
}

/// An error that can occur during the compilation of the source code.
#[derive(Debug, Clone)]
pub struct Error {
    pub error_type: ErrorType,
    pub position: Position,
    pub details: String,
}

impl Error {
    pub fn new(error_type: ErrorType, position: Position, details: String) -> Self {
        Self {
            error_type,
            position,
            details,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} in {} at {}:{} to {}:{} :: {}",
            self.error_type,
            self.position.file,
            self.position.line_start,
            self.position.start,
            self.position.line_end,
            self.position.end,
            self.details
        )
    }
}

/// A position in the source code.
#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    pub line_start: usize,
    pub line_end: usize,
    pub start: usize,
    pub end: usize,
    pub file: &'static str,
}

impl Position {
    pub fn new(line: usize, start: usize, end: usize, file: &'static str) -> Position {
        Position {
            line_start: line,
            line_end: line,
            start,
            end,
            file,
        }
    }
}
