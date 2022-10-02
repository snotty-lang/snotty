use std::fmt;

use crate::parser::{analyzer::Analyzer, Error, Memory, Rule};
use pest::iterators::Pair;

#[derive(Clone, PartialEq, Eq)]
pub enum Value {
    Byte(u8),
    None,
    Ref(Box<Value>),
    Pointer(Memory, Kind),
    Memory(Memory, Kind),
    Function(Kind),
    DataBox(Vec<Value>, Kind),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Value {
    pub fn kind(&self) -> Kind {
        match self {
            Value::Byte(_) => Kind::Byte,
            Value::None => Kind::None,
            Value::Ref(val) => Kind::Ref(Box::new(val.kind())),
            Value::Pointer(_, t) => Kind::Pointer(Box::new(t.clone())),
            Value::Memory(_, t) => t.clone(),
            Value::Function(t) => t.clone(),
            Value::DataBox(_, t) => t.clone(),
        }
    }

    pub fn get_size(&self) -> usize {
        self.kind().get_size()
    }

    pub fn deref_ref(self, derefs: usize, refs: &mut usize) -> Option<Value> {
        if derefs == 0 {
            return Some(self);
        }
        match self {
            Value::Ref(t) => {
                *refs += 1;
                t.deref_ref(derefs - 1, refs)
            }
            Value::Pointer(i, t) => Some(Value::Pointer(i, t.deref_ref(derefs - 1, refs)?)),
            Value::Memory(i, t) => Some(Value::Memory(i, t.deref_ref(derefs - 1, refs)?)),
            _ => None,
        }
    }

    pub fn offset_memory(&mut self, offset: Memory) {
        match self {
            Value::Ref(t) => t.offset_memory(offset),
            Value::Pointer(m, _) | Value::Memory(m, _) => *m += offset,
            Value::DataBox(_, _) => todo!(),
            _ => (),
        }
    }
}

#[derive(Clone, PartialEq, Default, Eq)]
pub enum Kind {
    #[default]
    None,
    Byte,
    Ref(Box<Kind>),
    Pointer(Box<Kind>),
    DataBox(usize, Vec<Kind>),
    Function(FunctionType, Vec<Kind>, Box<Kind>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionType {
    PtrToFx(Memory, Memory),
    PtrToMem(Memory),
}

impl fmt::Debug for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Kind {
    pub fn from_pair<'a>(
        pair: Pair<'a, Rule>,
        analyzer: &Analyzer<'a>,
        fx_mem: Memory,
    ) -> Result<Self, Error> {
        match pair.as_rule() {
            Rule::kind => Ok(match pair.as_str().trim().as_bytes() {
                b"byte" => Kind::Byte,
                b";" => Kind::None,
                [b'&', ..] => Kind::Ref(Box::new(Kind::from_pair(
                    pair.into_inner().next().unwrap(),
                    analyzer,
                    fx_mem,
                )?)),
                [b'*', ..] => Kind::Pointer(Box::new(Kind::from_pair(
                    pair.into_inner().next().unwrap(),
                    analyzer,
                    fx_mem,
                )?)),
                [b'f', b'x', x, ..] if !x.is_ascii_alphanumeric() => {
                    let mut args = Vec::new();
                    let mut ret = Kind::None;
                    for pair in pair.into_inner() {
                        match pair.as_rule() {
                            Rule::kind => args.push(Kind::from_pair(pair, analyzer, fx_mem)?),
                            Rule::fx_ret => {
                                ret = Kind::from_pair(
                                    pair.into_inner().next().unwrap(),
                                    analyzer,
                                    fx_mem,
                                )?
                            }
                            _ => unreachable!(),
                        }
                    }
                    Kind::Function(FunctionType::PtrToMem(fx_mem), args, Box::new(ret))
                }
                _ => todo!(),
            }),
            _ => unreachable!(),
        }
    }

    pub fn get_size(&self) -> usize {
        match self {
            Kind::None => 0,
            Kind::Byte => 1,
            Kind::Ref(t) => t.get_size(),
            Kind::DataBox(.., s) => s.iter().map(|t| t.get_size()).sum(),
            Kind::Function(..) => std::mem::size_of::<Memory>(),
            Kind::Pointer(_) => std::mem::size_of::<Memory>(),
        }
    }

    pub fn deref_ref(self, derefs: usize, refs: &mut usize) -> Option<Kind> {
        if derefs == 0 {
            return Some(self);
        }
        match self {
            Kind::Ref(t) => {
                *refs += 1;
                t.deref_ref(derefs - 1, refs)
            }
            Kind::Pointer(t) => Some(Kind::Pointer(Box::new(t.deref_ref(derefs - 1, refs)?))),
            _ => None,
        }
    }

    pub fn dereferenced(self, derefs: usize) -> Option<Kind> {
        if derefs == 0 {
            return Some(self);
        }
        match self {
            Kind::Ref(t) => t.dereferenced(derefs - 1),
            Kind::Pointer(t) => t.dereferenced(derefs - 1),
            _ => None,
        }
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Kind::Ref(t) => write!(f, "&{}", t),
            Kind::None => write!(f, ";"),
            Kind::Byte => write!(f, "byte"),
            Kind::DataBox(t, ..) => write!(f, "box {{{}}}", t),
            Kind::Function(ptr, t, r) => write!(f, "({ptr:?}) fx ({:?}) -> {}", t, r),
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
            Value::Memory(mem, t) => write!(f, "<{}>{{{}}}", t, mem),
            Value::Pointer(v, t) => write!(f, "<*{}>{{*{}}}", t, v),
            Value::Function(t) => write!(f, "{}", t),
            Value::DataBox(f_, t) => write!(f, "<{}>{{{:?}}}", t, f_),
        }
    }
}
