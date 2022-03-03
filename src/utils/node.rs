use std::fmt::{self, Display};

use super::{Position, Token, TokenType, BOOLEAN_EXCLUSIVE, BOOLEAN_OPERATORS};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Number,
    Boolean,
    None,
    Char,
    Struct(Token),
    Array(Box<Type>, usize),
    Ref(Box<Type>),
}

impl Type {
    pub fn get_result_type(&self, rhs: &Self, op: &Token) -> Option<Self> {
        match (self, rhs) {
            (Self::Number, Self::Number) => {
                if BOOLEAN_OPERATORS.contains(&op.token_type) {
                    Some(Self::Boolean)
                } else if BOOLEAN_EXCLUSIVE.contains(&op.token_type) {
                    None
                } else {
                    Some(Self::Number)
                }
            }
            (Self::Ref(t), Self::Number) | (Self::Number, Self::Ref(t)) => {
                if let TokenType::Add | TokenType::Sub = op.token_type {
                    Some(Self::Ref(t.clone()))
                } else {
                    None
                }
            }
            (Self::Boolean, Self::Boolean) => {
                if BOOLEAN_OPERATORS.contains(&op.token_type)
                    || BOOLEAN_EXCLUSIVE.contains(&op.token_type)
                {
                    Some(Self::Boolean)
                } else {
                    None
                }
            }
            (Self::Char, Self::Char) => {
                if BOOLEAN_OPERATORS.contains(&op.token_type) {
                    Some(Self::Boolean)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn get_result_type_unary(&self, op: &Token) -> Option<Self> {
        match self {
            Self::Number => {
                if op.token_type == TokenType::LNot {
                    None
                } else if [TokenType::Inc, TokenType::Dec].contains(&op.token_type) {
                    Some(Self::None)
                } else {
                    Some(Self::Number)
                }
            }
            Self::Boolean => {
                if op.token_type == TokenType::LNot {
                    Some(Self::Boolean)
                } else {
                    None
                }
            }
            Self::Ref(t) => {
                if let TokenType::Inc | TokenType::Dec = op.token_type {
                    Some(Self::Ref(t.clone()))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn can_be_converted(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (
                Self::Number | Self::Boolean | Self::Char,
                Self::Number | Self::Boolean | Self::Char,
            )
        )
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Number => write!(f, "int"),
            Type::Boolean => write!(f, "bool"),
            Type::None => write!(f, "()"),
            Type::Char => write!(f, "char"),
            Type::Array(t, l) => write!(f, "[{}; {}]", t, l),
            Type::Ref(t) => write!(f, "&{}", t),
            Type::Struct(s) => write!(f, "struct {}", s),
        }
    }
}

/// A Node in the AST.
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    /// Struct, fields
    StructConstructor(Token, Vec<(Token, Node)>, Position),
    /// String
    String(Token),
    /// Condition, Body
    While(Box<Node>, Box<Node>, Position),
    /// Name, Fields
    Struct(Token, Vec<(Token, Type)>, Position),
    /// Number
    Number(Token),
    /// Boolean
    Boolean(Token),
    /// Operation, left, right
    BinaryOp(Token, Box<Node>, Box<Node>, Type),
    /// Operation, expression
    UnaryOp(Token, Box<Node>, Type),
    /// Variable, Expression, Type
    VarAssign(Token, Box<Node>, Type),
    /// Variable, Expression
    StaticVar(Token, Box<Node>),
    /// Variable
    VarAccess(Token, Type),
    /// Variable, Expression
    VarReassign(Token, Box<Node>),
    /// Statements
    Statements(Vec<Node>, Type, Position),
    /// Function, args
    Call(Token, Vec<Node>, Type, Position),
    /// Function, args, body, return type, inline
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
    Ref(Box<Node>, Type, Position),
    /// Expression
    Deref(Box<Node>, Type, Position),
    /// Condition, then, else
    Ternary(Box<Node>, Box<Node>, Box<Node>, Type, Position),
    /// Condition, then, else
    If(Box<Node>, Box<Node>, Option<Box<Node>>, Position),
    /// None
    None(Position),
    /// Char
    Char(Token),
    /// Elements
    Array(Vec<Node>, Type, Position),
    /// Array, index
    Index(Token, Box<Node>, Type, Position),
    /// Array, index, expression
    IndexAssign(Token, Box<Node>, Box<Node>),
    // Pointer, expression
    DerefAssign(Box<Node>, Box<Node>, Position),
    /// Init, Cond, Step, Body
    For(Box<Node>, Box<Node>, Box<Node>, Box<Node>, Position),
    /// Arguments, body
    Expanded(Vec<Node>, Type),
}

impl Node {
    pub fn position(&self) -> Position {
        match self {
            Node::Expanded(_, _) => unreachable!(),
            Node::String(token)
            | Node::Number(token)
            | Node::Char(token)
            | Node::Boolean(token)
            | Node::VarAccess(token, _) => token.position.clone(),
            Node::Ref(.., pos)
            | Node::StructConstructor(.., pos)
            | Node::Struct(.., pos)
            | Node::For(.., pos)
            | Node::Deref(.., pos)
            | Node::While(.., pos)
            | Node::Statements(.., pos)
            | Node::Call(.., pos)
            | Node::FuncDef(.., pos)
            | Node::Print(.., pos)
            | Node::Ascii(.., pos)
            | Node::If(.., pos)
            | Node::Ternary(.., pos)
            | Node::None(pos)
            | Node::DerefAssign(.., pos)
            | Node::Array(.., pos)
            | Node::Index(.., pos)
            | Node::Input(.., pos) => pos.clone(),
            Node::BinaryOp(_, left, right, _) => {
                let mut pos = left.position();
                let end_pos = right.position();
                pos.end = end_pos.end;
                pos.line_end = end_pos.line_end;
                pos
            }
            Node::VarReassign(token, expr)
            | Node::StaticVar(token, expr)
            | Node::VarAssign(token, expr, _)
            | Node::IndexAssign(token, _, expr)
            | Node::UnaryOp(token, expr, _) => {
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

    pub fn get_type(&self) -> Type {
        match self {
            Node::StructConstructor(t, ..) => Type::Struct(t.clone()),
            Node::Array(v, ty, _) => Type::Array(Box::new(ty.clone()), v.len()),
            Node::Return(a, _) => a.get_type(),
            Node::String(s) => {
                if let TokenType::String(ref s) = s.token_type {
                    Type::Array(Box::new(Type::Char), s.len() + 1)
                } else {
                    unreachable!()
                }
            }
            Node::Ref(_, ty, _) => Type::Ref(Box::new(ty.clone())),
            Node::Number(_) => Type::Number,
            Node::Boolean(_) => Type::Boolean,
            Node::Char(_) => Type::Char,
            Node::Input(_) => Type::Char,
            Node::VarAccess(_, ty)
            | Node::UnaryOp(_, _, ty)
            | Node::Deref(_, ty, _)
            | Node::BinaryOp(_, _, _, ty)
            | Node::Call(_, _, ty, _)
            | Node::Ternary(_, _, _, ty, _)
            | Node::Expanded(_, ty)
            | Node::Index(_, _, ty, _) => ty.clone(),
            Node::While(_, _, _)
            | Node::Struct(..)
            | Node::VarAssign(_, _, _)
            | Node::StaticVar(..)
            | Node::VarReassign(_, _)
            | Node::Statements(..)
            | Node::FuncDef(_, _, _, _, _)
            | Node::Print(_, _)
            | Node::Ascii(_, _)
            | Node::If(_, _, _, _)
            | Node::None(_)
            | Node::IndexAssign(_, _, _)
            | Node::DerefAssign(_, _, _)
            | Node::For(_, _, _, _, _) => Type::None,
        }
    }

    pub fn convert(&mut self, t: Type) {
        match (&self, t) {
            (Node::Number(n) | Node::Boolean(n) | Node::Char(n), Type::Char) => {
                *self = Node::Char(n.clone())
            }
            (Node::Number(n) | Node::Boolean(n) | Node::Char(n), Type::Boolean) => {
                *self = Node::Boolean(n.clone())
            }
            (Node::Number(n) | Node::Boolean(n) | Node::Char(n), Type::Number) => {
                *self = Node::Number(n.clone())
            }
            _ => (),
        }
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::StructConstructor(name, fields, _) => {
                write!(f, "{} {{", name)?;
                for (i, (field, val)) in fields.iter().enumerate() {
                    write!(f, "{} {}: ", if i == 0 { "" } else { "," }, field)?;
                    write!(f, "{}", val)?;
                }
                write!(f, "}}")
            }
            Node::String(token) => write!(f, "String({})", token),
            Node::Struct(name, fields, _) => {
                write!(f, "struct {} {{", name)?;
                for (name, ty) in fields {
                    write!(f, " {}: {:?},", name, ty)?;
                }
                write!(f, "}}")
            }
            Node::Number(token) => write!(f, "Number({})", token),
            Node::Boolean(token) => write!(f, "Boolean({})", token),
            Node::VarAccess(token, _) => write!(f, "Var({})", token),
            Node::BinaryOp(token, left, right, _) => {
                write!(f, "BinaryOp({} {} {})", left, token, right)
            }
            Node::UnaryOp(token, expr, _) => write!(f, "UnaryOp({} {})", token, expr),
            Node::VarReassign(token, expr) => write!(f, "Reassign({} = {})", token, expr),
            Node::VarAssign(token, expr, t) => write!(f, "Assign({} : {} = {})", token, t, expr),
            Node::Statements(statements, ..) => {
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
            Node::Call(token, args, _, _) => {
                write!(
                    f,
                    "Call({}({}))",
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
                    "FuncDef({}({}) -> {:?} {})",
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
            Node::Input(..) => {
                write!(f, "input")
            }
            Node::Ref(expr, ..) => {
                write!(f, "Ref({})", expr)
            }
            Node::Deref(expr, ..) => {
                write!(f, "Deref({})", expr)
            }
            Node::Ternary(cond, then, else_, ..) => {
                write!(f, "Ternary({} ? {} : {})", cond, then, else_)
            }
            Node::If(cond, then, Some(else_), ..) => {
                write!(f, "If( if {} then {} else {})", cond, then, else_)
            }
            Node::If(cond, then, None, _) => {
                write!(f, "If( if {} then {})", cond, then)
            }
            Node::None(_) => write!(f, "None"),
            Node::Char(c) => write!(f, "Char({})", c),
            Node::Array(arr, ..) => {
                write!(
                    f,
                    "Array({})",
                    arr.iter()
                        .map(|n| n.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Node::Index(arr, idx, ..) => {
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
            Node::Expanded(nodes, t) => {
                write!(
                    f,
                    "Expanded({} -> {:?})",
                    nodes
                        .iter()
                        .map(|n| n.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    t
                )
            }
            Node::StaticVar(var, expr) => write!(f, "StaticVar({} = {})", var, expr),
        }
    }
}
