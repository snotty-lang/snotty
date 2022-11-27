use std::fmt::Display;

use crate::{analyzer::value::ValueType, parser::syntax::SyntaxKind, Span};

/// Error
#[derive(Debug, Clone)]
pub struct Error<'source> {
    location: Location,
    source: &'source str,
    kind: ErrorKind,
    variant: ErrorVariant,
    path: String,
}

/// Kind of the error
#[derive(Debug, Clone)]
pub enum ErrorKind {
    UnexpectedSyntax { expected: SyntaxKind },
    UnknownSyntax,
    MissingSemicolon,
    UnsupportedOperation { operation: SyntaxKind },
    ByteOverflow,
    Custom { message: String },
    TypeError { type_: ValueType },
    UndefinedReference,
    UnknownType,
}

/// Location of the error
#[derive(Debug, Clone)]
pub struct Location {
    line: Span,
    column: Span,
}

impl Location {
    pub fn from_span(span: Span, source: &str) -> Location {
        let mut current_loc = 0;
        let mut start = false;
        let mut line = 0..0;
        let mut column = 0..0;

        for (i, source_line) in source.lines().enumerate() {
            current_loc += 1;
            if source_line.len() + current_loc >= span.start && !start {
                line.start = i;
                column.start = span.start + 1 - current_loc;
                start = true;
            }
            if source_line.len() + current_loc >= span.end {
                line.end = i;
                column.end = span.end + 1 - current_loc;
                break;
            }
            current_loc += source_line.len();
        }

        if !start {
            line.start = source.lines().count() - 1;
            column.start = source.lines().last().unwrap_or_default().len();
        }

        if column.is_empty() {
            column.end = column.start + 1;
        }
        Self { column, line }
    }
}

/// Variant of Error: Error or Warning
#[derive(Debug, Clone)]
pub enum ErrorVariant {
    Error,
    Warning,
    // Help,
}

impl<'source> Error<'source> {
    /// Instantiate an error
    #[allow(clippy::self_named_constructors)]
    pub fn error(kind: ErrorKind, span: Span, source: &'source str) -> Error<'source> {
        Self {
            variant: ErrorVariant::Error,
            location: Location::from_span(span, source),
            source,
            kind,
            path: String::from("<program>"),
        }
    }

    /// Instantiate a warning
    pub fn warning(kind: ErrorKind, span: Span, source: &'source str) -> Error<'source> {
        Self {
            variant: ErrorVariant::Warning,
            location: Location::from_span(span, source),
            source,
            kind,
            path: String::from("<program>"),
        }
    }

    pub fn with_path(mut self, path: String) -> Self {
        self.path = path;
        self
    }

    pub fn set_path(&mut self, path: String) {
        self.path = path
    }
}

impl Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (text, color) = match self.variant {
            ErrorVariant::Error => ("error", Color::RED),
            ErrorVariant::Warning => ("warning", Color::ORANGE),
        };

        let start_line = self.source.lines().nth(self.location.line.start).unwrap();
        let blue = Color::BLUE;

        writeln!(
            f,
            "\x1b[1;{color}m{text}\x1b[39m: {}\x1b[22m\n\
             \x1b[{blue}m   --> \x1b[39m{}:{}:{}\n\
             \x1b[{blue}m    |\n{:0>3} |\x1b[39m {}",
            self.kind,
            self.path,
            self.location.line.start + 1,
            self.location.column.start + 1,
            self.location.line.start + 1,
            start_line
        )?;

        match self.location.line.len() {
            0 => writeln!(
                f,
                "\x1b[{}m    |\x1b[{color}m {}{}\x1b[39m",
                Color::BLUE,
                " ".repeat(self.location.column.start),
                "^".repeat(self.location.column.len()),
            )?,
            1 => writeln!(
                f,
                "\x1b[{blue}m    |\x1b[{color}m {}^{}\x1b[39m\n\
                     \x1b[{blue}m    |\n\
                     {:0>3} |\x1b[39m {}\n\
                     \x1b[{blue}m    |\x1b[{color}m {}^\x1b[39m",
                " ".repeat(self.location.column.start),
                "_".repeat(start_line.len() - 1),
                self.location.line.end + 1,
                self.source.lines().nth(self.location.line.end).unwrap(),
                "_".repeat(self.location.column.end - 1),
            )?,

            _ => writeln!(
                f,
                "\x1b[{blue}m    |\x1b[{color}m {}^{}\x1b[39m\n\
                     \x1b[{blue}m    |\n\
                     ... |\x1b[39m ...\n\
                     \x1b[{blue}m    |\n\
                     {:0>3} |\x1b[39m {}\n\
                     \x1b[{blue}m    |\x1b[{color}m {}^\x1b[39m",
                " ".repeat(self.location.column.start),
                "_".repeat(start_line.len() - 1),
                self.location.line.end + 1,
                self.source.lines().nth(self.location.line.end).unwrap(),
                "_".repeat(self.location.column.end - 1),
            )?,
        }
        writeln!(
            f,
            "\x1b[{}m    |\n    =\x1b[39m Man can't even write compilable code",
            Color::BLUE
        )?;
        Ok(())
    }
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::Custom { message } => write!(f, "{}", message),
            ErrorKind::UnknownSyntax => write!(f, "I don't recognize this syntax"),
            ErrorKind::MissingSemicolon => write!(f, "I think you forgot that semicolon there"),
            ErrorKind::UnexpectedSyntax { expected } => {
                write!(f, "Expected to see a `{}` there instead", expected)
            }
            ErrorKind::UnsupportedOperation { operation } => {
                write!(
                    f,
                    "Operation `{}` is not supported between these types",
                    operation
                )
            }
            ErrorKind::ByteOverflow => {
                write!(f, "This shit is too big to fit in a byte")
            }
            ErrorKind::TypeError { type_ } => {
                write!(f, "Did not expect to see a `{type_}` there")
            }
            ErrorKind::UndefinedReference => {
                write!(f, "This thing is not defined")
            }
            ErrorKind::UnknownType => {
                write!(f, "The type of this thing is unknown")
            }
        }
    }
}

/// **Only works in ANSI supported terminals**
struct Color(u8, u8, u8);

impl Color {
    pub const RED: Color = Color(200, 30, 30);
    pub const ORANGE: Color = Color(225, 180, 0);
    pub const BLUE: Color = Color(0, 180, 255);
}

impl Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "38;2;{};{};{}", self.0, self.1, self.2)
    }
}
