use super::utils::{Error, ErrorType, Node, Number, Position, ReturnVal, Token, TokenType};

type InterpretResult = Result<ReturnVal, Error>;

pub struct Interpreter;

impl Interpreter {
    pub fn visit(node: Node) -> InterpretResult {
        match node {
            Node::Number(token) => Self::visit_number(token),
            Node::VarAccess(token) => Self::visit_var_access(token),
            Node::BinOp(token, left, right) => Self::visit_bin_op(token, *left, *right),
            Node::VarAssignNode(token, expr) => Self::visit_var_assign(token, *expr),
            Node::Call(func, args) => Self::visit_call(*func, args),
        }
    }

    fn visit_bin_op(token: Token, left: Node, right: Node) -> InterpretResult {
        let left = Self::visit(left)?.get_number().unwrap();
        let right = Self::visit(right)?.get_number().unwrap();
        let result = match token.token_type {
            TokenType::Plus => left.add(right),
            TokenType::Minus => left.subtract(right),
            _ => unreachable!(),
        };
        Ok(ReturnVal::Number(result))
    }

    fn visit_var_assign(token: Token, expr: Node) -> InterpretResult {
        Ok(ReturnVal::None)
    }

    fn visit_var_access(token: Token) -> InterpretResult {
        Ok(ReturnVal::None)
    }

    fn visit_number(token: Token) -> InterpretResult {
        if let TokenType::Number(num) = token.token_type {
            return Ok(ReturnVal::Number(Number(num)));
        }
        unreachable!()
    }

    fn visit_call(func: Node, args: Vec<Node>) -> InterpretResult {
        Ok(ReturnVal::None)
    }
}
