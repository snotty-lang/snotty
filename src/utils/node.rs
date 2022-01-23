use std::fmt;

use crate::utils::{Position, Token};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Number,
    Boolean,
    None,
    Char,
    Array(Box<Type>, usize),
    Ref(Box<Type>),
    Function(Vec<Type>, Box<Type>),
}

/// A Node in the AST.
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    /// Condition, Body
    While(Box<Node>, Box<Node>, Position),
    /// Number
    Number(Token),
    /// Boolean
    Boolean(Token),
    /// Operation, left, right
    BinaryOp(Token, Box<Node>, Box<Node>),
    /// Operation, expression
    UnaryOp(Token, Box<Node>),
    /// Variable, Expression, Type
    VarAssign(Token, Box<Node>, Option<Type>),
    /// Variable
    VarAccess(Token),
    /// Variable, Expression
    VarReassign(Token, Box<Node>),
    /// Statements
    Statements(Vec<Node>, Position),
    /// Function, args
    Call(Token, Vec<Node>, Position),
    /// Function, args, body, return type
    FuncDef(Token, Vec<(Token, Type)>, Box<Node>, Type, Position),
    /// Expression
    Return(Box<Node>, Position),
    /// Expressions
    Print(Vec<Node>, Position),
    /// Expressions
    Ascii(Vec<Node>, Position),
    /// Input
    Input(Position),
    /// Expression
    Ref(Box<Node>, Position),
    /// Expression
    Deref(Box<Node>, Position),
    /// Condition, then, else
    Ternary(Box<Node>, Box<Node>, Box<Node>, Position),
    /// Condition, then, else
    If(Box<Node>, Box<Node>, Option<Box<Node>>, Position),
    /// None
    None(Position),
    /// args, body, return type
    Lambda(Vec<(Token, Type)>, Box<Node>, Type, Position),
    /// Char
    Char(Token, Position),
    /// Elements
    Array(Vec<Node>, Position),
    /// Array, index
    Index(Token, Box<Node>, Position),
    /// Array, index, expression
    IndexAssign(Token, Box<Node>, Box<Node>),
    // Pointer, expression
    DerefAssign(Box<Node>, Box<Node>, Position),
    /// Init, Cond, Step, Body
    For(Box<Node>, Box<Node>, Box<Node>, Box<Node>, Position),
}

impl Node {
    pub fn position(&self) -> Position {
        match self {
            Node::Number(token) | Node::Boolean(token) | Node::VarAccess(token) => {
                token.position.clone()
            }
            Node::Ref(.., pos)
            | Node::For(.., pos)
            | Node::Deref(.., pos)
            | Node::While(.., pos)
            | Node::Statements(.., pos)
            | Node::Lambda(.., pos)
            | Node::Call(.., pos)
            | Node::FuncDef(.., pos)
            | Node::Print(.., pos)
            | Node::Ascii(.., pos)
            | Node::If(.., pos)
            | Node::Ternary(.., pos)
            | Node::None(pos)
            | Node::Char(.., pos)
            | Node::DerefAssign(.., pos)
            | Node::Array(.., pos)
            | Node::Index(.., pos)
            | Node::Input(pos) => pos.clone(),
            Node::BinaryOp(_, left, right) => {
                let mut pos = left.position();
                let end_pos = right.position();
                pos.end = end_pos.end;
                pos.line_end = end_pos.line_end;
                pos
            }
            Node::VarReassign(token, expr)
            | Node::VarAssign(token, expr, _)
            | Node::IndexAssign(token, _, expr)
            | Node::UnaryOp(token, expr) => {
                let mut pos = token.position.clone();
                let end_pos = expr.position();
                pos.end = end_pos.end;
                pos.line_end = end_pos.line_end;
                pos
            }
            Node::Return(val, pos) => {
                let mut pos = pos.clone();
                let end_pos = val.position();
                pos.end = end_pos.end;
                pos.line_end = end_pos.line_end;
                pos
            }
        }
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::Number(token) => write!(f, "Number({})", token),
            Node::Boolean(token) => write!(f, "Boolean({})", token),
            Node::VarAccess(token) => write!(f, "Var({})", token),
            Node::BinaryOp(token, left, right) => {
                write!(f, "BinaryOp({} {} {})", left, token, right)
            }
            Node::UnaryOp(token, expr) => write!(f, "{} {}", token, expr),
            Node::VarReassign(token, expr) => write!(f, "Reassign({} = {})", token, expr),
            Node::VarAssign(token, expr, _) => write!(f, "Assign({} = {})", token, expr),
            Node::Statements(statements, _) => {
                write!(
                    f,
                    "{{\n{}\n}}",
                    statements
                        .iter()
                        .map(|n| n.to_string())
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            Node::Call(token, args, _) => {
                write!(
                    f,
                    "{}({})",
                    token,
                    args.iter()
                        .map(|n| n.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Node::FuncDef(token, args, body, ret, _) => {
                write!(
                    f,
                    "{}({}) -> {:?} {}",
                    token,
                    args.iter()
                        .map(|n| format!("{} : {:?}", n.0, n.1))
                        .collect::<Vec<_>>()
                        .join(", "),
                    ret,
                    body
                )
            }
            Node::While(cond, body, _) => {
                write!(f, "while ({}) {}", cond, body)
            }
            Node::Return(expr, _) => {
                write!(f, "Return({})", expr)
            }
            Node::Print(expr, _) => {
                write!(
                    f,
                    "Print({})",
                    expr.iter()
                        .map(|n| n.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Node::Ascii(expr, _) => {
                write!(
                    f,
                    "Ascii({})",
                    expr.iter()
                        .map(|n| n.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Node::Input(_) => {
                write!(f, "input")
            }
            Node::Ref(expr, _) => {
                write!(f, "Ref({})", expr)
            }
            Node::Deref(expr, _) => {
                write!(f, "Deref({})", expr)
            }
            Node::Ternary(cond, then, else_, _) => {
                write!(f, "Ternary({} ? {} : {})", cond, then, else_)
            }
            Node::If(cond, then, else_, _) => {
                write!(f, "If( if {} then {} else {:?})", cond, then, else_)
            }
            Node::None(_) => write!(f, "None"),
            Node::Lambda(args, body, ret, _) => {
                write!(
                    f,
                    "Lambda({} -> {:?} {})",
                    args.iter()
                        .map(|n| format!("{} : {:?}", n.0, n.1))
                        .collect::<Vec<_>>()
                        .join(", "),
                    ret,
                    body
                )
            }
            Node::Char(c, _) => write!(f, "Char({})", c),
            Node::Array(arr, _) => {
                write!(
                    f,
                    "Array({})",
                    arr.iter()
                        .map(|n| n.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Node::Index(arr, idx, _) => {
                write!(f, "Index({}[{}])", arr, idx)
            }
            Node::IndexAssign(arr, idx, expr) => {
                write!(f, "IndexAssign({}[{}] = {})", arr, idx, expr)
            }
            Node::DerefAssign(expr, expr2, _) => {
                write!(f, "DerefAssign({} = {})", expr, expr2)
            }
            Node::For(init, cond, step, body, _) => {
                write!(f, "For(({} ; {} ; {}) : {})", init, cond, step, body)
            }
        }
    }
}
