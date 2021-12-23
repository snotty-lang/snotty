use std::fmt;

#[derive(Debug, Clone)]
pub enum ErrorType {
    Lex,
    Parse,
    Interpret,
}

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
            "Error while {:?}ing at line {}, column {} to {} : {}",
            self.error_type,
            self.position.line,
            self.position.start,
            self.position.end,
            self.details
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Position {
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

impl Position {
    pub fn new(line: usize, start: usize, end: usize) -> Position {
        Position { line, start, end }
    }
}
