use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
};

use pest::iterators::Pair;

use crate::{
    instruction::Instruction,
    parser::{error::Error, Rule},
    value::{Value, ValueKind},
};

#[derive(Debug, Default)]
pub struct Scope<'a> {
    // pub(crate) kind: HashMap<&'a str, ValueKind>,
    pub(crate) map: HashMap<&'a str, Value>,
    code: RefCell<Vec<Instruction>>,
    memory_cell: usize,
}

impl<'a> Scope<'a> {
    pub fn new() -> Scope<'a> {
        Scope {
            map: HashMap::new(),
            code: RefCell::new(Vec::new()),
            memory_cell: 0,
        }
    }

    pub fn new_from(memory: RefCell<Vec<Instruction>>) -> Scope<'a> {
        Scope {
            map: HashMap::new(),
            code: memory,
            memory_cell: 0,
        }
    }

    pub fn push(&mut self, pair: Pair<'a, Rule>) -> Result<(), Error<'a>> {
        match pair.as_rule() {
            Rule::EOI => Ok(()),
            Rule::stmt => self
                .from_pair(pair.into_inner().next().unwrap())
                .map(|_| ()),
            Rule::scope => {
                let mut new = Scope::new_from(self.code.clone());
                for stmt in pair.into_inner() {
                    new.push(stmt)?;
                }
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn from_pair(&mut self, mut pair: Pair<'a, Rule>) -> Result<Value, Error<'a>> {
        if pair.as_rule() == Rule::expr {
            pair = pair.into_inner().next().unwrap();
        }
        match pair.as_rule() {
            Rule::expr => self.from_pair(pair.into_inner().next().unwrap()),
            Rule::number => Ok(Value::Num(pair.as_str().parse().unwrap())),
            Rule::boolean => Ok(Value::Bool(pair.as_str().parse().unwrap())),
            Rule::none => Ok(Value::None),
            Rule::char => Ok(Value::Char(pair.as_str().as_bytes()[0])),
            Rule::stmt => self.from_pair(pair.into_inner().next().unwrap()),
            Rule::scope => {
                let mut new = Scope::new_from(self.code.clone());
                for stmt in pair.into_inner() {
                    new.push(stmt)?;
                }
                Ok(Value::None)
            }
            Rule::type_cast => {
                let mut iter = pair.clone().into_inner();
                let kind = ValueKind::from_pair(iter.next().unwrap(), self)?;
                let expr = self.from_pair(iter.next().unwrap())?;
                match (expr, kind) {
                    (expr, kind) if expr.kind() == kind => Ok(expr),
                    (Value::Memory(i, t1), t2) if t1.get_size() == t2.get_size() => {
                        Ok(Value::Memory(i, t2))
                    }
                    _ => Err(Error::TypeError(pair)),
                }
            }
            Rule::ident => self
                .map
                .get(pair.as_str())
                .ok_or(Error::UndefinedReference(pair))
                .cloned(),
            // .and_then(|&val| {
            //     if !matches!(p.as_rule(), Rule::expr) {
            //         Ok(val)
            //     } else {
            //         Err(Error::TypeError(p.clone()))
            //     }
            // }),
            Rule::ternary => {
                let mut iter = pair.into_inner();
                let cond_pair = iter.next().unwrap();
                let cond = self.from_pair(cond_pair.clone())?;
                if cond.kind() != ValueKind::Boolean {
                    return Err(Error::TypeError(cond_pair));
                }
                let then = self.from_pair(iter.next().unwrap())?;
                let kind = then.kind();
                let otherwise_pair = iter.next().unwrap();
                let otherwise = self.from_pair(otherwise_pair.clone())?;
                if otherwise.kind() != kind {
                    return Err(Error::TypeError(otherwise_pair));
                }

                let mem = self.memory_cell;
                self.memory_cell += 1;
                self.code.borrow_mut().push(Instruction::TernaryIf(cond, then, otherwise));
                Ok(Value::Memory(mem, kind))
            }
            Rule::increment | Rule::decrement => {
                // let kind = Value::from_pair(pair.clone().into_inner().next().unwrap(), memory)?;
                // match kind.kind() {
                //     ValueKind::Number | ValueKind::Boolean | ValueKind::Char => Ok(kind),
                //     _ => Err(Error::TypeError(pair)),
                // };
                todo!()
            }
            // Rule::databox => {
            //     let ident = pair.clone().into_inner().next().unwrap().as_str();
            //     self.map.insert(
            //         ident,
            //         (pair.clone(), ValueKind::from_pair(pair, self)?),
            //     );
            //     Ok(Value::None)
            // }
            // Rule::function => {
            //     let ident = pair.clone().into_inner().next().unwrap().as_str();
            //     self.map.insert(
            //         ident,
            //         (pair.clone(), ValueKind::from_pair(pair, self)?),
            //     );
            //     Ok(Value::None)
            // }
            Rule::assign | Rule::static_assign => {
                let mut iter = pair.clone().into_inner();
                let ident = iter.next().unwrap().as_str();
                let mut next = iter.next().unwrap();
                let kind = if next.as_rule() == Rule::kind {
                    let k = Some(ValueKind::from_pair(next, self)?);
                    next = iter.next().unwrap();
                    k
                } else {
                    None
                };
                let value = self.from_pair(next)?;
                if matches!(kind, Some(kind) if kind != value.kind()) {
                    return Err(Error::TypeError(pair));
                }

                match value {
                    Value::Memory(_, ValueKind::Ref(..)) => {
                        self.map.insert(ident, value);
                    }
                    Value::Memory(_, t) => {
                        let size = t.get_size();
                        self.code.borrow_mut().push(Instruction::Copy(Value::Memory(
                            self.memory_cell,
                            t.clone(),
                        )));
                        self.map.insert(ident, Value::Memory(self.memory_cell, t));
                        self.memory_cell += size;
                    }
                    Value::Ref(i, t) => {
                        self.map
                            .insert(ident, Value::Memory(i, ValueKind::Ref(Box::new(t))));
                    }
                    val => {
                        let t = val.kind();
                        let size = t.get_size();
                        self.code.borrow_mut().push(Instruction::Copy(val));
                        self.map.insert(ident, Value::Memory(self.memory_cell, t));
                        self.memory_cell += size;
                    }
                }

                Ok(Value::None)
            }
            Rule::reassign => {
                let mut iter = pair.clone().into_inner();
                let ident = iter.next().unwrap();
                let value = self.from_pair(iter.next().unwrap())?;
                let mut entry = if let Entry::Occupied(e) = self.map.entry(ident.as_str()) {
                    e
                } else {
                    return Err(Error::UndefinedReference(ident));
                };
                if entry.get().kind() != value.kind() {
                    return Err(Error::TypeError(pair));
                }

                match value {
                    Value::Memory(_, ValueKind::Ref(..)) => {
                        entry.insert(value);
                    }
                    Value::Memory(_, t) => {
                        let size = t.get_size();
                        self.code.borrow_mut().push(Instruction::Copy(Value::Memory(
                            self.memory_cell,
                            t.clone(),
                        )));
                        entry.insert(Value::Memory(self.memory_cell, t));
                        self.memory_cell += size;
                    }
                    Value::Ref(i, t) => {
                        entry.insert(Value::Memory(i, ValueKind::Ref(Box::new(t))));
                    }
                    val => {
                        let t = val.kind();
                        let size = t.get_size();
                        self.code.borrow_mut().push(Instruction::Copy(val));
                        entry.insert(Value::Memory(self.memory_cell, t));
                        self.memory_cell += size;
                    }
                }

                Ok(Value::None)
            }
            Rule::if_stmt => {
                let mut iter = pair.into_inner();
                let cond_pair = iter.next().unwrap();
                let cond = self.from_pair(cond_pair.clone())?;
                if cond.kind() != ValueKind::Boolean {
                    return Err(Error::TypeError(cond_pair));
                }
                let then = iter.next().unwrap();
                let otherwise = iter.next();
                let is_otherwise = otherwise.is_some();

                let mem = self.memory_cell;
                self.code.borrow_mut().push(Instruction::If(cond, mem, is_otherwise));
                self.memory_cell += 2;
                
                if self.from_pair(then.clone())? != Value::None {
                    return Err(Error::TypeError(then));
                }
                if let Some(otherwise) = otherwise {
                    self.code.borrow_mut().push(Instruction::Else(mem));
                    if self.from_pair(otherwise.clone())? != Value::None {
                        return Err(Error::TypeError(otherwise));
                    }
                }
                self.code.borrow_mut().push(Instruction::EndIf(mem, is_otherwise));
                Ok(Value::None)
            }
            Rule::while_stmt => {
                let mut iter = pair.into_inner();
                let cond_pair = iter.next().unwrap();
                let mut cond = self.from_pair(cond_pair.clone())?;
                if cond.kind() != ValueKind::Boolean {
                    return Err(Error::TypeError(cond_pair));
                }
                let size = cond.get_size();
                self.code.borrow_mut().push(Instruction::Copy(cond));
                cond = Value::Memory(self.memory_cell, ValueKind::Boolean);
                self.memory_cell += size;

                self.code.borrow_mut().push(Instruction::While(cond.clone()));

                let stmt = iter.next().unwrap();
                if self.from_pair(stmt.clone())? != Value::None {
                    return Err(Error::TypeError(stmt));
                }
                if let Value::Memory(_i, _t) = &cond {
                    let new_cond = self.from_pair(cond_pair)?;
                    if new_cond != cond {
                        self.code.borrow_mut().push(Instruction::Copy(new_cond));
                    }
                }
                Ok(Value::None)
            }
            Rule::for_stmt => {
                let mut iter = pair.into_inner();
                self.push(iter.next().unwrap())?;
                let cond_pair = iter.next().unwrap();
                let cond = ValueKind::from_pair(cond_pair.clone(), self)?;
                if cond != ValueKind::Boolean {
                    return Err(Error::TypeError(cond_pair));
                }
                self.push(iter.next().unwrap())?;
                self.push(iter.next().unwrap())?;
                Ok(Value::None)
            }
            Rule::print | Rule::return_stmt => {
                ValueKind::from_pair(pair.into_inner().next().unwrap(), self)?;
                Ok(Value::None)
            }
            _ => unreachable!(),
        }
    }
}
