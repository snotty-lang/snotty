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
