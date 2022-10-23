use std::fmt::Display;

use crate::Span;

#[derive(Debug, Clone)]
pub struct Error<'source> {
    location: Location,
    source: &'source str,
    description: String,
    kind: ErrorKind,
    path: String,
}

#[derive(Debug, Clone)]
pub struct Location {
    line: Span,
    column: Span,
}

impl Location {
    pub fn from_span(span: Span, source: &str) -> Location {
        let mut current_loc = 0;
        let mut line_start = None;
        let mut line_end = None;
        let mut column_start = None;
        let mut column_end = None;
        for (i, line) in source.lines().enumerate() {
            if line.len() + current_loc >= span.start && line_start.is_none() {
                line_start = Some(i);
                column_start = Some(span.start - current_loc);
            }
            if line.len() + current_loc >= span.end {
                line_end = Some(i);
                column_end = Some(span.end - current_loc);
                break;
            }
            current_loc += line.len();
        }
        Self {
            column: column_start.unwrap()..column_end.unwrap(),
            line: line_start.unwrap()..line_end.unwrap(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    Error,
    Warning,
    // Help,
}

impl<'source> Error<'source> {
    pub fn new(kind: ErrorKind, span: Span, source: &'source str) -> Error<'source> {
        Self {
            kind,
            location: Location::from_span(span, source),
            source,
            description: String::new(),
            path: String::from("<program>"),
        }
    }

    pub fn with_description(mut self, description: String) -> Self {
        self.description = description;
        self
    }

    pub fn with_path(mut self, path: String) -> Self {
        self.path = path;
        self
    }

    pub fn set_path(&mut self, path: String) {
        self.path = path
    }

    pub fn set_description(&mut self, description: String) {
        self.description = description;
    }
}

impl Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (text, color) = match self.kind {
            ErrorKind::Error => ("error", Color::RED),
            ErrorKind::Warning => ("warning", Color::ORANGE),
        };

        let start_line = self.source.lines().nth(self.location.line.start).unwrap();
        let end_line = self.source.lines().nth(self.location.line.end).unwrap();
        let blue = Color::BLUE;

        writeln!(
            f,
            "\x1b[1;{color}m{text}\x1b[39m: {}\x1b[22m\n\
             \x1b[{blue}m   --> \x1b[39m{}:{}:{}\n\
             \x1b[{blue}m    |\n{:0>3} |\x1b[39m {}",
            self.description,
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
                "".repeat(self.location.column.start),
                "^".repeat(self.location.column.len()),
            )?,
            1 => writeln!(
                f,
                "\x1b[{blue}m    |\x1b[{color}m {}^{}\x1b[39m\n\
                     \x1b[{blue}m    |\n\
                     {:0>3} |\x1b[39m {}\n\
                     \x1b[{blue}m    |\x1b[{color}m {}^\x1b[39m",
                "".repeat(self.location.column.start),
                "_".repeat(start_line.len() - 1),
                self.location.line.end + 1,
                end_line,
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
                "".repeat(self.location.column.start),
                "_".repeat(start_line.len() - 1),
                self.location.line.end + 1,
                end_line,
                "_".repeat(self.location.column.end - 1),
            )?,
        }
        writeln!(
            f,
            "\x1b[{}m    |\n    =\x1b[39m fix ur code lol",
            Color::BLUE
        )?;
        Ok(())
    }
}

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
