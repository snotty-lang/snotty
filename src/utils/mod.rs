mod error;
mod node;
mod scope;
mod token;

pub use error::*;
pub use node::*;
pub use scope::*;
pub use token::*;

// #[derive(Debug, PartialEq, Clone)]
// pub struct Variable {
//     pub name: String,
//     pub value: Type,
//     pub token: Token,
//     pub node: Box<Node>,
// }

// impl Variable {
//     pub fn new(token: Token, node: Box<Node>) -> Self {
//         let value = match *node {
//             Node::Number(n) => Type::Number(n),
//             Node::BinaryOp(_, _, _) => todo!(),
//             Node::UnaryOp(_, _) => todo!(),
//             Node::VarAccess(_) => todo!(),
//             Node::Statements(_) => todo!(),
//             Node::Call(_, _) => todo!(),
//             Node::FuncDef(_, _, _, _) => todo!(),
//             Node::Return(_, _) => todo!(),
//             _ => unreachable!()
//         };
//         if let TokenType::Identifier(name) = token.token_type {
//             Self {
//                 name,
//                 value,
//                 token,
//                 node,
//             }
//         } else {
//             unreachable!();
//         }
//     }
// }

// #[derive(Debug, PartialEq, Clone)]
// pub enum Type {
//     Number(Token)
// }
