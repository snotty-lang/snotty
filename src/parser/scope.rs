use std::collections::HashMap;

use pest::iterators::Pair;

use crate::{
    parser::{error::Error, Rule},
    value::ValueKind,
};

pub struct Scope<'a> {
    memory: HashMap<&'a str, (Pair<'a, Rule>, ValueKind)>,
}

impl<'a> Scope<'a> {
    pub fn new() -> Scope<'a> {
        Scope {
            memory: HashMap::new(),
        }
    }

    pub fn push(&mut self, pair: Pair<'a, Rule>) -> Result<(), Error<'a>> {
        match pair.as_rule() {
            Rule::EOI => Ok(()),
            Rule::stmt => self.push(pair.into_inner().next().unwrap()),
            Rule::scope => {
                for stmt in pair.into_inner() {
                    self.push(stmt)?;
                }
                Ok(())
            }
            Rule::expr | Rule::print => Ok(()),
            Rule::databox => todo!(),
            Rule::function => todo!(),
            Rule::assign | Rule::static_assign => {
                let mut iter = pair.clone().into_inner();
                let ident = iter.next().unwrap().as_str();
                let mut next = iter.next().unwrap();
                let kind = if next.as_rule() == Rule::kind {
                    let k = Some(ValueKind::from_pair(next, &self.memory)?);
                    next = iter.next().unwrap();
                    k
                } else {
                    None
                };
                let value_kind = ValueKind::from_pair(next, &self.memory)?;
                if matches!(kind, Some(kind) if kind != value_kind) {
                    return Err(Error::TypeError(pair));
                }

                self.memory.insert(ident, (pair, value_kind));

                Ok(())
            }
            _ => unreachable!(),
        }
    }
}
