use std::fmt;

use crate::parser::{scope::Scope, Error, Rule};
use pest::iterators::Pair;

#[derive(Clone, PartialEq)]
pub enum Value {
    Byte(u8),
    None,
    Ref(Box<Value>),
    Pointer(usize, Kind),
    Memory(usize, Kind),
}

impl Value {
    pub fn kind(&self) -> Kind {
        match self {
            Value::Byte(_) => Kind::Byte,
            Value::None => Kind::None,
            Value::Ref(val) => Kind::Ref(Box::new(val.kind())),
            Value::Pointer(_, t) => Kind::Pointer(Box::new(t.clone())),
            Value::Memory(_, t) => t.clone(),
        }
    }

    pub fn get_size(&self) -> usize {
        self.kind().get_size()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    None,
    Byte,
    Ref(Box<Kind>),
    Pointer(Box<Kind>),
    DataBox(usize, Vec<Kind>),
    Function(usize, Vec<Kind>, Box<Kind>),
}

impl Kind {
    pub fn from_pair<'a>(mut pair: Pair<'a, Rule>, scope: &Scope<'a>) -> Result<Self, Error> {
        match pair.as_rule() {
            Rule::expr => Kind::from_pair(pair.into_inner().next().unwrap(), scope),
            Rule::number | Rule::boolean | Rule::char => Ok(Kind::Byte),
            Rule::none => Ok(Kind::None),
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
                    "byte" => Kind::Byte,
                    ";" => Kind::None,
                    _ => Kind::from_pair(kind, scope)?,
                };

                while let Some(inner) = pair.into_inner().next() {
                    kind = Kind::Ref(Box::new(kind));
                    pair = inner;
                }
                Ok(kind)
            }
            _ => unreachable!(),
        }
    }

    pub fn get_size(&self) -> usize {
        match self {
            Kind::None => 0,
            Kind::Byte => 1,
            Kind::Ref(t) => t.get_size(),
            Kind::DataBox(.., s) => s.iter().map(|t| t.get_size()).sum(),
            Kind::Function(..) => 0,
            Kind::Pointer(_) => 2,
        }
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Kind::Ref(t) => write!(f, "&{}", t),
            Kind::None => write!(f, ";"),
            Kind::Byte => write!(f, "byte"),
            Kind::DataBox(t, ..) => write!(f, "ez {{{}}}", t),
            Kind::Function(t, ..) => write!(f, "ez ({})", t),
            Kind::Pointer(t) => write!(f, "*{}", t),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::None => write!(f, ";"),
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
