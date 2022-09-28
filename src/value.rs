use std::fmt;

use crate::parser::{scope::Scope, Error, Rule};
use pest::iterators::Pair;

#[derive(Clone, PartialEq)]
pub enum Value {
    Byte(u8),
    Bool(bool),
    None,
    Ref(Box<Value>),
    Pointer(usize, ValueKind),
    Memory(usize, ValueKind),
}

impl Value {
    pub fn kind(&self) -> ValueKind {
        match self {
            Value::Byte(_) => ValueKind::Byte,
            Value::Bool(_) => ValueKind::Boolean,
            Value::None => ValueKind::None,
            Value::Ref(val) => ValueKind::Ref(Box::new(val.kind())),
            Value::Pointer(_, t) => ValueKind::Pointer(Box::new(t.clone())),
            Value::Memory(_, t) => t.clone(),
        }
    }

    pub fn get_size(&self) -> usize {
        self.kind().get_size()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueKind {
    None,
    Byte,
    Boolean,
    Ref(Box<ValueKind>),
    Pointer(Box<ValueKind>),
    DataBox(usize, Vec<ValueKind>),
    Function(usize, Vec<ValueKind>, Box<ValueKind>),
}

impl ValueKind {
    pub fn from_pair<'a>(mut pair: Pair<'a, Rule>, scope: &Scope<'a>) -> Result<Self, Error> {
        match pair.as_rule() {
            Rule::expr => ValueKind::from_pair(pair.into_inner().next().unwrap(), scope),
            Rule::number => Ok(ValueKind::Byte),
            Rule::boolean => Ok(ValueKind::Boolean),
            Rule::none => Ok(ValueKind::None),
            // Rule::ident => scope
            //     .map
            //     .get(pair.as_str())
            //     .ok_or(Error::UndefinedReference(pair))
            //     .and_then(|(p, i)| {
            //         if !matches!(p.as_rule(), Rule::expr | Rule::function) {
            //             Ok(scope.code.borrow()[*i].kind())
            //         } else {
            //             Err(Error::TypeError(p.clone()))
            //         }
            //     }),
            // Rule::function => {
            //     let mut iter = pair.into_inner();
            //     drop(iter.next());
            //     let mut ret_kind = ValueKind::None;
            //     let mut params = Vec::new();
            //     while let Some(next) = iter.peek() {
            //         match next.as_rule() {
            //             Rule::ident => {
            //                 drop(iter.next());
            //                 params.push(ValueKind::from_pair(iter.next().unwrap(), scope)?);
            //             }
            //             Rule::kind => ret_kind = ValueKind::from_pair(iter.next().unwrap(), scope)?,
            //             Rule::stmt => break,
            //             _ => unreachable!(),
            //         }
            //     }
            //     println!("{}", ret_kind);
            //     // Ok(ValueKind::Function(
            //     //     scope.code.borrow().len(),
            //     //     params,
            //     //     Box::new(ret_kind),
            //     // ))
            //     todo!()
            // }
            // Rule::databox => {
            //     let mut iter = pair.into_inner();
            //     drop(iter.next());
            //     let mut fields = Vec::new();
            //     while iter.next().is_some() {
            //         fields.push(ValueKind::from_pair(iter.next().unwrap(), scope)?);
            //     }
            //     // Ok(ValueKind::DataBox(scope.code.borrow().len(), fields));
            //     todo!()
            // }
            Rule::kind => {
                let kind = if let Some(kind) = pair.clone().into_inner().flatten().next() {
                    kind
                } else {
                    pair.clone()
                };
                let mut kind = match kind.as_str().trim() {
                    "byte" => ValueKind::Byte,
                    "bool" => ValueKind::Boolean,
                    ";" => ValueKind::None,
                    _ => ValueKind::from_pair(kind, scope)?,
                };

                while let Some(inner) = pair.into_inner().next() {
                    kind = ValueKind::Ref(Box::new(kind));
                    pair = inner;
                }
                Ok(kind)
            }
            _ => unreachable!(),
        }
    }

    pub fn get_size(&self) -> usize {
        match self {
            ValueKind::None => 0,
            ValueKind::Byte => std::mem::size_of::<u8>(),
            ValueKind::Boolean => 1,
            ValueKind::Ref(t) => t.get_size(),
            ValueKind::DataBox(.., s) => s.iter().map(|t| t.get_size()).sum(),
            ValueKind::Function(..) => 0,
            ValueKind::Pointer(_) => 2,
        }
    }
}

impl fmt::Display for ValueKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ValueKind::Ref(t) => write!(f, "&{}", t),
            ValueKind::None => write!(f, ";"),
            ValueKind::Byte => write!(f, "byte"),
            ValueKind::Boolean => write!(f, "bool"),
            ValueKind::DataBox(t, ..) => write!(f, "ez {{{}}}", t),
            ValueKind::Function(t, ..) => write!(f, "ez ({})", t),
            ValueKind::Pointer(t) => write!(f, "*{}", t),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::None => write!(f, ";"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Byte(num) => write!(f, "{}", num),
            Value::Ref(ptr) => write!(f, "&{}", ptr),
            Value::Memory(mem, _) => write!(f, "<{}>", mem),
            Value::Pointer(v, t) => write!(f, "<*{}>{{{}}}", t, v),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
