use std::fmt::{self, Debug, Display};

pub type Memory = u16;

#[derive(Clone, PartialEq, Eq)]
pub enum BaseValue {
    Byte(u8),
    None,
    Ref(Box<Value>),
    Pointer(Memory),
    Memory(Memory),
    Function(Memory, Memory),
    // DataBox(Vec<BaseValue>),
}

impl Debug for BaseValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl BaseValue {
    pub fn offset_memory(&mut self, offset: Memory) {
        match self {
            BaseValue::Ref(t) => t.offset_memory(offset),
            BaseValue::Pointer(m) | BaseValue::Memory(m) => *m += offset,
            // BaseValue::DataBox(_) => todo!(),
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
    Function(Vec<Kind>, Box<Kind>),
}

impl Debug for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Kind {
    // pub fn from_pair<'a>(pair: Pair<'a, Rule>, analyzer: &Analyzer<'a>) -> Result<Self, Error> {
    //     match pair.as_rule() {
    //         Rule::kind => Ok(match pair.as_str().trim().as_bytes() {
    //             b"byte" => Kind::Byte,
    //             b";" => Kind::None,
    //             [b'&', ..] => Kind::Ref(Box::new(Kind::from_pair(
    //                 pair.into_inner().next().unwrap(),
    //                 analyzer,
    //             )?)),
    //             [b'*', ..] => Kind::Pointer(Box::new(Kind::from_pair(
    //                 pair.into_inner().next().unwrap(),
    //                 analyzer,
    //             )?)),
    //             [b'f', b'x', x, ..] if !x.is_ascii_alphanumeric() => {
    //                 let mut args = Vec::new();
    //                 let mut ret = Kind::None;
    //                 for pair in pair.into_inner() {
    //                     match pair.as_rule() {
    //                         Rule::kind => args.push(Kind::from_pair(pair, analyzer)?),
    //                         Rule::fx_ret => {
    //                             ret = Kind::from_pair(pair.into_inner().next().unwrap(), analyzer)?
    //                         }
    //                         _ => unreachable!(),
    //                     }
    //                 }
    //                 Kind::Function(args, Box::new(ret))
    //             }
    //             _ => todo!(),
    //         }),
    //         _ => unreachable!(),
    //     }
    // }

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
}

impl Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Kind::Ref(t) => write!(f, "&{}", t),
            Kind::None => write!(f, ";"),
            Kind::Byte => write!(f, "byte"),
            Kind::DataBox(t, ..) => write!(f, "box {{{}}}", t),
            Kind::Function(t, r) => {
                write!(f, "fx (")?;
                match &**t {
                    [] => (),
                    [x, y @ ..] => {
                        write!(f, "{}", x)?;
                        for x in y {
                            write!(f, ", {}", x)?
                        }
                    }
                }
                write!(f, ") -> {}", r)?;
                Ok(())
            }
            Kind::Pointer(t) => write!(f, "*{}", t),
        }
    }
}

impl Display for BaseValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BaseValue::None => write!(f, ";"),
            BaseValue::Byte(num) => write!(f, "{}", num),
            BaseValue::Ref(ptr) => write!(f, "&{}", ptr),
            BaseValue::Memory(mem) => write!(f, "{{{}}}", mem),
            BaseValue::Pointer(v) => write!(f, "{{*{}}}", v),
            BaseValue::Function(a, _) => write!(f, "fx {{{}}}", a),
            // BaseValue::DataBox(f_) => write!(f, "box {{{:?}}}", f_),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Value {
    pub kind: Kind,
    pub value: BaseValue,
    pub properties: ValueProperties,
}

impl Value {
    pub const fn kind(&self) -> &Kind {
        &self.kind
    }

    pub const fn byte(byte: u8) -> Self {
        Self {
            kind: Kind::Byte,
            value: BaseValue::Byte(byte),
            properties: ValueProperties::default(),
        }
    }

    pub const fn none() -> Self {
        Self {
            kind: Kind::None,
            value: BaseValue::None,
            properties: ValueProperties::default(),
        }
    }

    pub fn reference(to: Value) -> Self {
        Self {
            kind: Kind::Ref(Box::new(to.kind().clone())),
            value: BaseValue::Ref(Box::new(to)),
            properties: ValueProperties::default(),
        }
    }

    pub fn function(fx: Memory, mem: Memory, params: Vec<Kind>, ret: Kind) -> Self {
        Self {
            kind: Kind::Function(params, Box::new(ret)),
            value: BaseValue::Function(fx, mem),
            properties: ValueProperties::default(),
        }
    }

    pub fn databox() -> Self {
        todo!()
    }

    pub fn pointer(memory: Memory, kind: Kind) -> Self {
        Self {
            kind: Kind::Pointer(Box::new(kind)),
            value: BaseValue::Pointer(memory),
            properties: ValueProperties::default(),
        }
    }

    pub const fn memory(memory: Memory, kind: Kind) -> Self {
        Self {
            kind,
            value: BaseValue::Memory(memory),
            properties: ValueProperties::default(),
        }
    }

    pub const fn with_properties(mut self, properties: ValueProperties) -> Self {
        self.properties = properties;
        self
    }

    pub fn offset_memory(&mut self, offset: Memory) {
        match &mut self.value {
            BaseValue::Ref(t) => t.offset_memory(offset),
            BaseValue::Pointer(m) | BaseValue::Memory(m) => *m += offset,
            // BaseValue::DataBox(_) => todo!(),
            _ => (),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{}>:<{}>", self.kind, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueProperties {
    pub is_const: bool,
}

impl ValueProperties {
    pub const fn default() -> ValueProperties {
        ValueProperties { is_const: false }
    }
}

impl Default for ValueProperties {
    fn default() -> ValueProperties {
        ValueProperties::default()
    }
}
