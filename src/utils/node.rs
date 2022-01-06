use super::{error::Position, token::Token};

/// A Node in the AST.
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Number(Token),
    BinaryOp(Token, Box<Node>, Box<Node>),
    UnaryOp(Token, Box<Node>),
    VarAssign(Token, Box<Node>),
    VarAccess(Token),
    VarReassign(Token, Box<Node>),
    Statements(Vec<Node>),
    Call(Box<Node>, Vec<Node>),
    FuncDef(Token, Vec<Token>, Box<Node>),
    Return(Token, Option<Box<Node>>),
    Print(Box<Node>),
    Ascii(Box<Node>),
    Input(Token),
    If(Box<Node>, Box<Node>, Option<Box<Node>>),
    None(Token),
}

impl Node {
    pub fn position(&self) -> Position {
        match self {
            Node::Number(token) => token.position,
            Node::BinaryOp(token, _, right) => {
                let right_pos = right.position();
                let mut new_pos = token.position;
                new_pos.line_end = right_pos.line_end;
                new_pos.end = right_pos.end;
                new_pos
            }
            Node::UnaryOp(token, expr)
            | Node::VarAssign(token, expr)
            | Node::VarReassign(token, expr) => {
                let expr_pos = expr.position();
                let mut new_pos = token.position;
                new_pos.line_end = expr_pos.line_end;
                new_pos.end = expr_pos.end;
                new_pos
            }
            Node::VarAccess(token) => token.position,
            Node::Statements(nodes) => {
                let mut pos = Position::new(0, 0, 0);
                for node in nodes {
                    let node_pos = node.position();
                    if node_pos.line_start < pos.line_start {
                        pos.line_start = node_pos.line_start;
                    }
                    if node_pos.line_end > pos.line_end {
                        pos.line_end = node_pos.line_end;
                    }
                    if node_pos.end > pos.end {
                        pos.end = node_pos.end;
                    }
                    if node_pos.start < pos.start {
                        pos.start = node_pos.start;
                    }
                }
                pos
            }
            Node::Call(node, args) => {
                let mut node_pos = node.position();
                let arg_end = args.last();
                if let Some(arg) = arg_end {
                    let arg_pos = arg.position();
                    node_pos.line_end = arg_pos.line_end;
                    node_pos.end = arg_pos.end;
                }
                node_pos
            }
            Node::FuncDef(token, _, body) => {
                let body_pos = body.position();
                let mut new_pos = token.position;
                new_pos.line_end = body_pos.line_end;
                new_pos.end = body_pos.end;
                new_pos
            }
            Node::Return(token, expr) => {
                if let Some(expr) = expr {
                    let expr_pos = expr.position();
                    let mut new_pos = token.position;
                    new_pos.line_end = expr_pos.line_end;
                    new_pos.end = expr_pos.end;
                    new_pos
                } else {
                    token.position
                }
            }
            Node::Print(node) => node.position(),
            Node::Ascii(node) => node.position(),
            Node::If(cond, then, else_) => {
                let mut pos = cond.position();
                if let Some(else_) = else_ {
                    let else_pos = else_.position();
                    pos.line_end = else_pos.line_end;
                    pos.end = else_pos.end;
                } else {
                    let then_pos = then.position();
                    pos.line_end = then_pos.line_end;
                    pos.end = then_pos.end;
                }
                pos
            }
            Node::None(token) => token.position,
            Node::Input(token) => token.position,
        }
    }
}
