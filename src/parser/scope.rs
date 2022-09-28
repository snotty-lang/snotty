use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

use pest::iterators::Pair;

use crate::{
    error,
    instruction::Instruction,
    parser::{Error, Rule},
    value::{Kind, Value},
};

#[derive(Debug, Default)]
pub struct Scope<'a> {
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

    pub fn push(&mut self, pair: Pair<'a, Rule>) -> Result<(), Error> {
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
    fn from_pair(&mut self, pair: Pair<'a, Rule>) -> Result<Value, Error> {
        match pair.as_rule() {
            Rule::expr => self.from_pair(pair.into_inner().next().unwrap()),
            Rule::number => Ok(Value::Byte(pair.as_str().parse().unwrap())),
            Rule::boolean => Ok(Value::Byte((pair.as_str().trim() == "true").into())),
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
                let kind = Kind::from_pair(iter.next().unwrap(), self)?;
                let expr = self.from_pair(iter.next().unwrap())?;
                match (expr, kind) {
                    (expr, kind) if expr.kind() == kind => Ok(expr),
                    (Value::Memory(i, t1), t2) if t1.get_size() == t2.get_size() => {
                        Ok(Value::Memory(i, t2))
                    }
                    (expr, kind) => {
                        error!(pair => format!("Cannot cast a <{}> into a <{}>", expr.kind(), kind))
                    }
                }
            }
            Rule::ident => self
                .map
                .get(pair.as_str())
                .ok_or_else(
                    || error!(E pair => format!("Cannot find {} in current scope", pair.as_str())),
                )
                .cloned(),
            Rule::unop_expr => {
                let mut iter = pair.clone().into_inner();
                let op = iter.next().unwrap();
                let expr = self.from_pair(iter.next().unwrap())?;
                match op.as_str() {
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
                        _ => error!(pair => format!("Cannot dereference a <{}>", expr.kind())),
                    },
                    "-" => {
                        let kind = expr.kind();
                        if kind != Kind::Byte {
                            error!(R pair => format!("Cannot negate a <{}>", kind));
                        }
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.borrow_mut().push(Instruction::Neg(expr, loc));
                        Ok(Value::Memory(loc, kind))
                    }
                    "!" => {
                        let kind = expr.kind();
                        if kind != Kind::Byte {
                            error!(R pair => format!("Cannot not a <{}>", kind));
                        }
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.borrow_mut().push(Instruction::Not(expr, loc));
                        Ok(Value::Memory(loc, kind))
                    }
                    _ => unreachable!(),
                }
            }
            Rule::binop_expr => {
                let mut iter = pair.clone().into_inner();
                let left = self.from_pair(iter.next().unwrap())?;
                let op = iter.next().unwrap();
                let right = self.from_pair(iter.next().unwrap())?;
                match (left.kind(), op.as_str(), right.kind()) {
                    (Kind::Byte, "+", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .borrow_mut()
                            .push(Instruction::Add(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "-", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .borrow_mut()
                            .push(Instruction::Sub(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "*", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .borrow_mut()
                            .push(Instruction::Mul(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "/", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .borrow_mut()
                            .push(Instruction::Div(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "%", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .borrow_mut()
                            .push(Instruction::Mod(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "|", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .borrow_mut()
                            .push(Instruction::Or(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "^", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .borrow_mut()
                            .push(Instruction::Xor(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "&", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .borrow_mut()
                            .push(Instruction::And(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, ">>", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .borrow_mut()
                            .push(Instruction::Shr(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "<<", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .borrow_mut()
                            .push(Instruction::Shl(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "**", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .borrow_mut()
                            .push(Instruction::Pow(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (a, "==", b) if a == b => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .borrow_mut()
                            .push(Instruction::Eq(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (a, "!=", b) if a == b => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .borrow_mut()
                            .push(Instruction::Neq(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "<", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .borrow_mut()
                            .push(Instruction::Lt(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "<=", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .borrow_mut()
                            .push(Instruction::Le(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, ">", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .borrow_mut()
                            .push(Instruction::Gt(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, ">=", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .borrow_mut()
                            .push(Instruction::Ge(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (l, op, r) => {
                        error!(pair => format!("Cannot apply operator {} to a <{}> and a <{}>", op, l, r))
                    }
                }
            }
            Rule::ternary => {
                let mut iter = pair.clone().into_inner();
                let cond_pair = iter.next().unwrap();
                let cond = self.from_pair(cond_pair.clone())?;
                if cond.kind() != Kind::Byte {
                    error!(R cond_pair => format!("Condition in a ternary expression must be a <byte>, and not a <{}>", cond.kind()));
                }
                let then = self.from_pair(iter.next().unwrap())?;
                let kind = then.kind();
                let otherwise_pair = iter.next().unwrap();
                let otherwise = self.from_pair(otherwise_pair.clone())?;
                if otherwise.kind() != kind {
                    error!(R pair => format!("Both branches of the ternary expression don't match! One returns a <{}> while other returns a <{}>", kind, otherwise.kind()));
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
                if value.kind() != Kind::Byte {
                    error!(R pair => format!("Cannot increment a <{}>", value.kind()));
                }
                self.code.borrow_mut().push(Instruction::Inc(value));
                Ok(Value::None)
            }
            Rule::decrement => {
                let value = self.from_pair(pair.clone().into_inner().next().unwrap())?;
                if value.kind() != Kind::Byte {
                    error!(R pair => format!("Cannot decrement a <{}>", value.kind()));
                }
                self.code.borrow_mut().push(Instruction::Dec(value));
                Ok(Value::None)
            }
            Rule::assign => {
                let mut iter = pair.clone().into_inner();
                drop(iter.next());
                let ident = iter.next().unwrap().as_str();
                let mut next = iter.next().unwrap();
                let kind = if next.as_rule() == Rule::kind {
                    let k = Some(Kind::from_pair(next, self)?);
                    next = iter.next().unwrap();
                    k
                } else {
                    None
                };
                let value = self.from_pair(next)?;
                if matches!(kind, Some(ref kind) if *kind != value.kind()) {
                    error!(R pair => format!("{} should be a <{}> but it is a <{}>", ident, kind.unwrap(), value.kind()));
                }

                match value {
                    Value::Memory(_, Kind::Ref(..)) => {
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
                            self.map
                                .insert(ident, Value::Memory(*i, Kind::Ref(Box::new(val.kind()))));
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
                let mut entry = if let Entry::Occupied(e) = self.map.entry(ident.as_str()) {
                    e
                } else {
                    error!(R ident => format!("Cannot find {} in current scope", ident.as_str()));
                };
                if entry.get().kind() != value.kind() {
                    error!(R pair => format!("{} is a <{}> but is assigned to a <{}>", ident.as_str(), entry.get().kind(), value.kind()));
                }

                match value {
                    Value::Memory(_, Kind::Ref(..)) => {
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
                            entry.insert(Value::Memory(*i, Kind::Ref(Box::new(val.kind()))));
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
                if cond.kind() != Kind::Byte {
                    error!(R cond_pair => format!("Condition in an if statement must be a <byte>, and not a <{}>", cond.kind()));
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

                self.from_pair(then.clone())?;
                if let Some(otherwise) = otherwise {
                    self.code.borrow_mut().push(Instruction::Else(loc));
                    self.from_pair(otherwise.clone())?;
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
                if cond.kind() != Kind::Byte {
                    error!(R cond_pair => format!("Condition in a while statement must be a <byte>, and not a <{}>", cond.kind()));
                }
                let loc = self.loc;
                self.loc += 1;
                self.code.borrow_mut().push(Instruction::Copy(cond, loc));
                cond = Value::Memory(loc, Kind::Byte);

                self.code
                    .borrow_mut()
                    .push(Instruction::While(cond.clone()));

                let stmt = iter.next().unwrap();
                self.from_pair(stmt.clone())?;

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

                self.from_pair(init.clone())?;

                let mut cond = self.from_pair(cond_pair.clone())?;
                if cond.kind() != Kind::Byte {
                    error!(R cond_pair => format!("Condition in a for statement must be a <byte>, and not a <{}>", cond.kind()));
                }

                let loc = self.loc;
                self.loc += 1;
                self.code.borrow_mut().push(Instruction::Copy(cond, loc));
                cond = Value::Memory(loc, Kind::Byte);

                self.code
                    .borrow_mut()
                    .push(Instruction::While(cond.clone()));

                self.from_pair(body.clone())?;
                self.from_pair(step.clone())?;

                if let Value::Memory(i, _) = &cond {
                    let new_cond = self.from_pair(cond_pair)?;
                    if new_cond != cond {
                        self.code.borrow_mut().push(Instruction::Copy(new_cond, *i));
                    }
                }

                Ok(Value::None)
            }
            Rule::out => {
                let expr_pair = pair.clone().into_inner().nth(1).unwrap();
                let expr = self.from_pair(expr_pair.clone())?;
                if expr.kind() == Kind::Byte {
                    self.code.borrow_mut().push(Instruction::Out(expr));
                } else {
                    error!(R expr_pair => format!("Cannot write a <{}> to stdout", expr.kind()))
                }
                Ok(Value::None)
            }
            Rule::input => {
                let loc = self.loc;
                self.loc += 1;
                self.code.borrow_mut().push(Instruction::Input(loc));
                Ok(Value::Memory(loc, Kind::Byte))
            }
            _ => unreachable!("{pair}"),
        }
    }
}
