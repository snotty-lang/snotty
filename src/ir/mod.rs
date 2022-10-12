pub mod instruction;
pub mod value;

use self::instruction::Instruction;
use self::value::Memory;

#[derive(Debug, Clone)]
pub struct IR {
    pub memory_used: Memory,
    pub code: Vec<Instruction>,
    pub fxs: Vec<Vec<Instruction>>,
}

// #[macro_export]
// macro_rules! error {
//     ($pair: expr => $message: expr) => {
//         Err($crate::parser::Error::new_from_span(
//             pest::error::ErrorVariant::CustomError { message: $message },
//             $pair.as_span(),
//         ))
//     };

//     (E $pair: expr => $message: expr) => {
//         $crate::parser::Error::new_from_span(
//             pest::error::ErrorVariant::CustomError { message: $message },
//             $pair.as_span(),
//         )
//     };

//     (R $pair: expr => $message: expr) => {
//         return Err($crate::parser::Error::new_from_span(
//             pest::error::ErrorVariant::CustomError { message: $message },
//             $pair.as_span(),
//         ))
//     };

//     (S $span: expr => $message: expr) => {
//         Err($crate::parser::Error::new_from_span(
//             pest::error::ErrorVariant::CustomError { message: $message },
//             $span,
//         ))
//     };

//     (ES $span: expr => $message: expr) => {
//         $crate::parser::Error::new_from_span(
//             pest::error::ErrorVariant::CustomError { message: $message },
//             $span,
//         )
//     };

//     (RS $span: expr => $message: expr) => {
//         return Err($crate::parser::Error::new_from_span(
//             pest::error::ErrorVariant::CustomError { message: $message },
//             $span,
//         ))
//     };
// }
