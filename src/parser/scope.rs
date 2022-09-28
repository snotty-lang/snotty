use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
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
    code: Rc<RefCell<Vec<Instruction>>>,
    loc: usize,
}

impl<'a> Scope<'a> {
    pub fn new() -> Scope<'a> {
        Self::default()
    }

    pub fn code(self) -> Rc<RefCell<Vec<Instruction>>> {
        self.code
    }

    pub fn new_from(memory: Rc<RefCell<Vec<Instruction>>>) -> Scope<'a> {
        Scope {
            map: HashMap::new(),
            code: memory,
            loc: 0,
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

    #[allow(clippy::wrong_self_convention)]
    fn from_pair(&mut self, pair: Pair<'a, Rule>) -> Result<Value, Error<'a>> {
        match pair.as_rule() {
            Rule::expr => self.from_pair(pair.into_inner().next().unwrap()),
            Rule::number => Ok(Value::Byte(pair.as_str().parse().unwrap())),
            Rule::boolean => Ok(Value::Bool(pair.as_str().parse().unwrap())),
            Rule::none => Ok(Value::None),
            Rule::char => Ok(Value::Byte(pair.as_str().as_bytes()[0])),
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
                .get(pair.as_str().trim())
                .ok_or(Error::UndefinedReference(pair))
                .cloned(),
            Rule::unop_expr => {
                let mut iter = pair.clone().into_inner();
                let op = iter.next().unwrap();
                let expr = self.from_pair(iter.next().unwrap())?;
                match op.as_str().trim() {
                    "&" => Ok(Value::Ref(Box::new(expr))),
                    "*" => match expr {
                        Value::Pointer(_, ref t) => {
                            let loc = self.loc;
                            self.loc += 1;
                            let t = t.clone();
                            self.code.borrow_mut().push(Instruction::Deref(expr, loc));
                            Ok(Value::Memory(loc, t))
                        }
                        Value::Ref(t) => Ok(*t),
                        _ => Err(Error::TypeError(pair)),
                    },
                    "-" => {
                        let kind = expr.kind();
                        if kind != ValueKind::Byte {
                            return Err(Error::TypeError(pair));
                        }
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.borrow_mut().push(Instruction::Neg(expr, loc));
                        Ok(Value::Memory(loc, kind))
                    }
                    "!" => {
                        let kind = expr.kind();
                        if !matches!(kind, ValueKind::Boolean | ValueKind::Byte) {
                            return Err(Error::TypeError(pair));
                        }
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.borrow_mut().push(Instruction::Not(expr, loc));
                        Ok(Value::Memory(loc, kind))
                    }
                    _ => unreachable!(),
                }
            }
            // .and_then(|&val| {
            //     if !matches!(p.as_rule(), Rule::expr) {
            //         Ok(val)
            //     } else {
            //         Err(Error::TypeError(p.clone()))
            //     }
            // }),
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

                let loc = self.loc;
                self.loc += 1;

                self.code
                    .borrow_mut()
                    .push(Instruction::TernaryIf(cond, then, otherwise, loc));
                Ok(Value::Memory(loc, kind))
            }
            Rule::increment => {
                let value = self.from_pair(pair.clone().into_inner().next().unwrap())?;
                match value.kind() {
                    ValueKind::Byte | ValueKind::Boolean => {
                        self.code.borrow_mut().push(Instruction::Inc(value));
                        Ok(Value::None)
                    }
                    _ => Err(Error::TypeError(pair)),
                }
            }
            Rule::decrement => {
                let value = self.from_pair(pair.clone().into_inner().next().unwrap())?;
                match value.kind() {
                    ValueKind::Byte | ValueKind::Boolean => {
                        self.code.borrow_mut().push(Instruction::Dec(value));
                        Ok(Value::None)
                    }
                    _ => Err(Error::TypeError(pair)),
                }
            }
            Rule::assign | Rule::static_assign => {
                let mut iter = pair.clone().into_inner();
                drop(iter.next()); // TODO
                let ident = iter.next().unwrap().as_str().trim();
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
                    Value::Memory(i, t) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .borrow_mut()
                            .push(Instruction::Copy(Value::Memory(i, t.clone()), loc));
                        self.map.insert(ident, Value::Memory(loc, t));
                    }
                    Value::Ref(val) => {
                        if let Value::Memory(i, _) = &*val {
                            self.map.insert(
                                ident,
                                Value::Memory(*i, ValueKind::Ref(Box::new(val.kind()))),
                            );
                        } else {
                            self.map
                                .insert(ident, Value::Ref(Box::new(Value::Ref(val))));
                        }
                    }
                    val => {
                        let t = val.kind();
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.borrow_mut().push(Instruction::Copy(val, loc));
                        self.map.insert(ident, Value::Memory(loc, t));
                    }
                }

                Ok(Value::None)
            }
            Rule::reassign => {
                let mut iter = pair.clone().into_inner();
                let ident = iter.next().unwrap();
                let value = self.from_pair(iter.next().unwrap())?;
                let mut entry = if let Entry::Occupied(e) = self.map.entry(ident.as_str().trim()) {
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
                    Value::Memory(i, t) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .borrow_mut()
                            .push(Instruction::Copy(Value::Memory(i, t.clone()), loc));
                        entry.insert(Value::Memory(loc, t));
                    }
                    Value::Ref(val) => {
                        if let Value::Memory(i, _) = &*val {
                            entry.insert(Value::Memory(*i, ValueKind::Ref(Box::new(val.kind()))));
                        } else {
                            entry.insert(Value::Ref(Box::new(Value::Ref(val))));
                        }
                    }
                    val => {
                        let t = val.kind();
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.borrow_mut().push(Instruction::Copy(val, loc));
                        entry.insert(Value::Memory(loc, t));
                    }
                }

                Ok(Value::None)
            }
            Rule::if_stmt => {
                let mut iter = pair.into_inner();
                drop(iter.next());
                let cond_pair = iter.next().unwrap();
                let cond = self.from_pair(cond_pair.clone())?;
                if cond.kind() != ValueKind::Boolean {
                    return Err(Error::TypeError(cond_pair));
                }
                let then = iter.next().unwrap();
                drop(iter.next());
                let otherwise = iter.next();
                let is_otherwise = otherwise.is_some();

                let loc = self.loc;
                self.loc += 1;
                self.code
                    .borrow_mut()
                    .push(Instruction::If(cond, loc, is_otherwise));

                if self.from_pair(then.clone())? != Value::None {
                    return Err(Error::TypeError(then));
                }
                if let Some(otherwise) = otherwise {
                    self.code.borrow_mut().push(Instruction::Else(loc));
                    if self.from_pair(otherwise.clone())? != Value::None {
                        return Err(Error::TypeError(otherwise));
                    }
                }
                self.code
                    .borrow_mut()
                    .push(Instruction::EndIf(loc, is_otherwise));
                Ok(Value::None)
            }
            Rule::while_stmt => {
                let mut iter = pair.into_inner();
                drop(iter.next());
                let cond_pair = iter.next().unwrap();
                let mut cond = self.from_pair(cond_pair.clone())?;
                if cond.kind() != ValueKind::Boolean {
                    return Err(Error::TypeError(cond_pair));
                }
                let loc = self.loc;
                self.loc += 1;
                self.code.borrow_mut().push(Instruction::Copy(cond, loc));
                cond = Value::Memory(loc, ValueKind::Boolean);

                self.code
                    .borrow_mut()
                    .push(Instruction::While(cond.clone()));

                let stmt = iter.next().unwrap();
                if self.from_pair(stmt.clone())? != Value::None {
                    return Err(Error::TypeError(stmt));
                }

                if let Value::Memory(i, _) = &cond {
                    let new_cond = self.from_pair(cond_pair)?;
                    if new_cond != cond {
                        self.code.borrow_mut().push(Instruction::Copy(new_cond, *i));
                    }
                }
                Ok(Value::None)
            }
            Rule::for_stmt => {
                let mut iter = pair.into_inner();
                drop(iter.next());
                let init = iter.next().unwrap();
                let cond_pair = iter.next().unwrap();
                let step = iter.next().unwrap();
                let body = iter.next().unwrap();

                if self.from_pair(init.clone())? != Value::None {
                    return Err(Error::TypeError(init));
                }

                let mut cond = self.from_pair(cond_pair.clone())?;
                if cond.kind() != ValueKind::Boolean {
                    return Err(Error::TypeError(cond_pair));
                }

                let loc = self.loc;
                self.loc += 1;
                self.code.borrow_mut().push(Instruction::Copy(cond, loc));
                cond = Value::Memory(loc, ValueKind::Boolean);

                self.code
                    .borrow_mut()
                    .push(Instruction::While(cond.clone()));

                if self.from_pair(body.clone())? != Value::None {
                    return Err(Error::TypeError(body));
                }
                if self.from_pair(step.clone())? != Value::None {
                    return Err(Error::TypeError(step));
                }

                if let Value::Memory(i, _) = &cond {
                    let new_cond = self.from_pair(cond_pair)?;
                    if new_cond != cond {
                        self.code.borrow_mut().push(Instruction::Copy(new_cond, *i));
                    }
                }

                Ok(Value::None)
            }
            Rule::out => {
                let expr = self.from_pair(pair.clone().into_inner().nth(1).unwrap())?;
                match expr.kind() {
                    ValueKind::Byte => self.code.borrow_mut().push(Instruction::Out(expr)),
                    ValueKind::Boolean => {
                        for &c in pair.as_str().as_bytes() {
                            self.code
                                .borrow_mut()
                                .push(Instruction::Out(Value::Byte(c)))
                        }
                    }
                    _ => return Err(Error::TypeError(pair)),
                }
                Ok(Value::None)
            }
            Rule::input => {
                let loc = self.loc;
                self.loc += 1;
                self.code.borrow_mut().push(Instruction::Input(loc));
                Ok(Value::Memory(loc, ValueKind::Byte))
            }
            _ => unreachable!("{pair}"),
        }
    }
}
