use super::{Position, Token};

/// A Node in the AST.
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Number(Token),
    BinaryOp(Token, Box<Node>, Box<Node>),
    UnaryOp(Token, Box<Node>),
    VarAssign(Token, Box<Node>),
    VarAccess(Token),
    VarReassign(Token, Box<Node>),
    Statements(Vec<Node>, Position),
    Call(Box<Node>, Vec<Node>, Position),
    FuncDef(Token, Vec<Token>, Box<Node>, Position),
    Return(Option<Box<Node>>, Position),
    Print(Box<Node>, Position),
    Ascii(Box<Node>, Position),
    Input(Position),
    Ref(Box<Node>, Position),
    Deref(Box<Node>, Position),
    Ternary(Box<Node>, Box<Node>, Box<Node>, Position),
    If(Box<Node>, Box<Node>, Option<Box<Node>>, Position),
    None(Position),
}

impl Node {
    pub fn position(&self) -> Position {
        match self {
            Node::Number(token) | Node::VarAccess(token) => token.position.clone(),
            Node::Ref(.., pos)
            | Node::Deref(.., pos)
            | Node::Statements(.., pos)
            | Node::Call(.., pos)
            | Node::FuncDef(.., pos)
            | Node::Print(.., pos)
            | Node::Ascii(.., pos)
            | Node::If(.., pos)
            | Node::Ternary(.., pos)
            | Node::Return(.., pos)
            | Node::None(pos)
            | Node::Input(pos) => pos.clone(),
            Node::UnaryOp(op, expr) => {
                let mut pos = op.position.clone();
                let end_pos = expr.position();
                pos.end = end_pos.end;
                pos.line_end = end_pos.line_end;
                pos
            }
            Node::BinaryOp(_, left, right) => {
                let mut pos = left.position();
                let end_pos = right.position();
                pos.end = end_pos.end;
                pos.line_end = end_pos.line_end;
                pos
            }
            Node::VarAssign(token, expr) => {
                let mut pos = token.position.clone();
                let end_pos = expr.position();
                pos.end = end_pos.end;
                pos.line_end = end_pos.line_end;
                pos
            }
            Node::VarReassign(token, expr) => {
                let mut pos = token.position.clone();
                let end_pos = expr.position();
                pos.end = end_pos.end;
                pos.line_end = end_pos.line_end;
                pos
            }
        }
    }
}
