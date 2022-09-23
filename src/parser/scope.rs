use std::collections::HashMap;

use pest::iterators::Pair;

use crate::{
    parser::{error::Error, Rule},
    value::ValueKind,
};

#[derive(Debug, Default)]
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
            Rule::databox => {
                let ident = pair.clone().into_inner().next().unwrap().as_str();
                self.memory.insert(
                    ident,
                    (pair.clone(), ValueKind::from_pair(pair, &self.memory)?),
                );
                Ok(())
            }
            Rule::function => {
                let ident = pair.clone().into_inner().next().unwrap().as_str();
                self.memory.insert(
                    ident,
                    (pair.clone(), ValueKind::from_pair(pair, &self.memory)?),
                );
                Ok(())
            }
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
            Rule::reassign => {
                let mut iter = pair.clone().into_inner();
                let ident = iter.next().unwrap();
                let kind = if let Some((_, k)) = self.memory.get(ident.as_str()) {
                    k.clone()
                } else {
                    return Err(Error::UndefinedReference(ident));
                };
                let value_kind = ValueKind::from_pair(iter.next().unwrap(), &self.memory)?;
                if kind != value_kind {
                    return Err(Error::TypeError(pair));
                }

                Ok(())
            }
            Rule::if_stmt => {
                let mut iter = pair.into_inner();
                let cond_pair = iter.next().unwrap();
                let cond = ValueKind::from_pair(cond_pair.clone(), &self.memory)?;
                if cond != ValueKind::Boolean {
                    return Err(Error::TypeError(cond_pair));
                }
                self.push(iter.next().unwrap())?;
                if let Some(next) = iter.next() {
                    self.push(next)?;
                }
                Ok(())
            }
            Rule::while_stmt => {
                let mut iter = pair.into_inner();
                let cond_pair = iter.next().unwrap();
                let cond = ValueKind::from_pair(cond_pair.clone(), &self.memory)?;
                if cond != ValueKind::Boolean {
                    return Err(Error::TypeError(cond_pair));
                }
                self.push(iter.next().unwrap())
            }
            Rule::for_stmt => {
                let mut iter = pair.into_inner();
                self.push(iter.next().unwrap())?;
                let cond_pair = iter.next().unwrap();
                let cond = ValueKind::from_pair(cond_pair.clone(), &self.memory)?;
                if cond != ValueKind::Boolean {
                    return Err(Error::TypeError(cond_pair));
                }
                self.push(iter.next().unwrap())?;
                self.push(iter.next().unwrap())
            }
            Rule::expr | Rule::print | Rule::return_stmt => {
                ValueKind::from_pair(pair.into_inner().next().unwrap(), &self.memory).map(|_| ())
            }
            _ => unreachable!(),
        }
    }
}
