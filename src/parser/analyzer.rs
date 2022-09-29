use std::collections::{hash_map::Entry, HashMap};

use pest::iterators::Pair;

use crate::{
    error,
    parser::{
        instruction::Instruction,
        value::{Kind, Value},
        Error, Rule,
    },
};

#[derive(Debug)]
pub struct Analyzer<'a> {
    map: Vec<HashMap<&'a str, Value>>,
    code: Vec<Instruction>,
    loc: usize,
}

impl<'a> Default for Analyzer<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Analyzer<'a> {
    pub fn new() -> Analyzer<'a> {
        Analyzer {
            map: vec![HashMap::new()],
            code: Vec::new(),
            loc: 0,
        }
    }

    #[inline]
    pub fn get(&self, ident: &'a str) -> Option<Value> {
        self.map
            .iter()
            .rev()
            .find_map(|map| map.get(ident))
            .cloned()
    }

    #[inline]
    pub fn insert(&mut self, ident: &'a str, value: Value) {
        self.map.last_mut().unwrap().insert(ident, value);
    }

    pub fn code(self) -> Vec<Instruction> {
        self.code
    }

    pub fn push(&mut self, pair: Pair<'a, Rule>) -> Result<(), Error> {
        match pair.as_rule() {
            Rule::EOI => Ok(()),
            _ => self
                .analyze_pair(pair.into_inner().next().unwrap())
                .map(|_| ()),
        }
    }

    fn analyze_pair(&mut self, pair: Pair<'a, Rule>) -> Result<Value, Error> {
        match pair.as_rule() {
            Rule::expr => self.analyze_pair(pair.into_inner().next().unwrap()),
            Rule::number => Ok(Value::Byte(pair.as_str().parse().unwrap())),
            Rule::boolean => Ok(Value::Byte((pair.as_str().trim() == "true").into())),
            Rule::none => Ok(Value::None),
            Rule::char => Ok(Value::Byte(pair.as_str().as_bytes()[0])),
            Rule::pointer => {
                let value = self.analyze_pair(pair.into_inner().next().unwrap())?;
                match value {
                    Value::Memory(i, t) => Ok(Value::Pointer(i, t)),
                    _ => {
                        let loc = self.loc;
                        self.loc += 1;
                        let kind = value.kind();
                        self.code.push(Instruction::Copy(value, loc));
                        Ok(Value::Pointer(loc, kind))
                    }
                }
            }
            Rule::stmt => self.analyze_pair(pair.into_inner().next().unwrap()),
            Rule::scope => {
                self.map.push(HashMap::new());
                let prev = self.loc;
                for stmt in pair.into_inner() {
                    self.push(stmt)?;
                }
                self.code.push(Instruction::Clear(prev, self.loc));
                self.loc = prev;
                self.map.pop();
                Ok(Value::None)
            }
            Rule::type_cast => {
                let mut iter = pair.clone().into_inner();
                let kind = Kind::from_pair(iter.next().unwrap(), self)?;
                let expr = self.analyze_pair(iter.next().unwrap())?;
                let expr_kind = expr.kind();

                fn cast(
                    expr: Value,
                    kind: Kind,
                    push: &mut impl FnMut(Value) -> usize,
                ) -> Option<Value> {
                    match (expr, kind) {
                        (expr, kind) if expr.kind() == kind => Some(expr),
                        (Value::Memory(i, _), t2) => Some(Value::Memory(i, t2)),
                        (Value::Pointer(i, _), Kind::Pointer(t2)) => Some(Value::Pointer(i, *t2)),
                        (Value::Byte(i), Kind::Pointer(t)) => Some(Value::Pointer(i as usize, *t)),
                        (Value::Ref(t), Kind::Pointer(kind)) if t.kind() == *kind => {
                            let loc = match &*t {
                                Value::Memory(i, _) => Some(*i),
                                _ => None,
                            };
                            let val = cast(*t, *kind.clone(), push)?;
                            Some(Value::Pointer(loc.unwrap_or_else(|| (push)(val)), *kind))
                        }
                        (Value::Ref(t), kind) if t.get_size() == kind.get_size() => {
                            cast(*t, kind, push)
                        }
                        (expr, Kind::Ref(t)) if expr.get_size() == t.get_size() => {
                            Some(Value::Ref(Box::new(cast(expr, *t, push)?)))
                        }
                        (expr, Kind::Pointer(t)) if expr.get_size() == t.get_size() => match expr {
                            Value::Memory(i, t) => Some(Value::Pointer(i, t)),
                            _ => Some(Value::Pointer(push(expr), *t)),
                        },
                        _ => None,
                    }
                }

                let push = &mut |val: Value| {
                    let loc = self.loc;
                    self.loc += 1;
                    self.code.push(Instruction::Copy(val, loc));
                    loc
                };

                cast(expr, kind.clone(), push).ok_or_else(
                    || error!(E pair => format!("Cannot cast a <{}> into a <{}>", expr_kind, kind)),
                )
            }
            Rule::ident => self.get(pair.as_str()).ok_or_else(
                || error!(E pair => format!("Cannot find {} in current scope", pair.as_str())),
            ),
            Rule::index => {
                let mut iter = pair.into_inner();
                let expr_pair = iter.next().unwrap();
                let index_pair = iter.next().unwrap();
                let expr = self.analyze_pair(expr_pair.clone())?;
                let index = self.analyze_pair(index_pair.clone())?;
                let expr_kind = expr.kind();
                let index_kind = index.kind();
                let kind = if let Kind::Pointer(k) = expr_kind.clone() {
                    *k
                } else {
                    error!(R expr_pair => format!("Cannot index a <{}>", expr_kind));
                };
                if index_kind != Kind::Byte {
                    error!(R expr_pair => format!("Cannot index a <{}> with a <{}>", index_kind, index_kind));
                }
                let loc = self.loc;
                self.loc += 2;
                self.code.push(Instruction::Add(expr, index, loc));
                self.code
                    .push(Instruction::Deref(Value::Memory(loc, expr_kind), loc + 1));
                Ok(Value::Memory(loc + 1, kind))
            }
            Rule::array => {
                let mut iter = pair.clone().into_inner();
                let kind = Kind::from_pair(iter.next().unwrap(), self)?;
                let loc = self.loc;
                for (i, element) in iter.enumerate() {
                    let element = self.analyze_pair(element)?;
                    if element.kind() != kind {
                        error!(R pair => format!("Found a <{}> in an array of <{}>s", element.kind(), kind));
                    }
                    self.code.push(Instruction::Copy(element, loc + i));
                    self.loc += 1;
                }
                Ok(Value::Pointer(loc, kind))
            }
            Rule::string => {
                let loc = self.loc;
                for element in pair.into_inner() {
                    let element = self.analyze_pair(element)?;
                    self.code.push(Instruction::Copy(element, self.loc));
                    self.loc += 1;
                }
                self.code.push(Instruction::Copy(Value::Byte(0), self.loc));
                self.loc += 1;
                Ok(Value::Pointer(loc, Kind::Byte))
            }
            Rule::unop_expr => {
                let mut iter = pair.clone().into_inner();
                let op = iter.next().unwrap();
                let expr = self.analyze_pair(iter.next().unwrap())?;
                match op.as_str() {
                    "&" => Ok(Value::Ref(Box::new(expr))),
                    "*" => match expr {
                        Value::Pointer(_, ref t) => {
                            let loc = self.loc;
                            self.loc += 1;
                            let t = t.clone();
                            self.code.push(Instruction::Deref(expr, loc));
                            Ok(Value::Memory(loc, t))
                        }
                        Value::Memory(_, Kind::Pointer(ref t)) => {
                            let loc = self.loc;
                            self.loc += 1;
                            let t = *t.clone();
                            self.code.push(Instruction::Deref(expr, loc));
                            Ok(Value::Memory(loc, t))
                        }
                        Value::Memory(i, Kind::Ref(t)) => Ok(Value::Memory(i, *t)),
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
                        self.code.push(Instruction::Neg(expr, loc));
                        Ok(Value::Memory(loc, kind))
                    }
                    "!" => {
                        let kind = expr.kind();
                        if kind != Kind::Byte {
                            error!(R pair => format!("Cannot not a <{}>", kind));
                        }
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Not(expr, loc));
                        Ok(Value::Memory(loc, kind))
                    }
                    _ => unreachable!(),
                }
            }
            Rule::binop_expr => {
                let mut iter = pair.clone().into_inner();
                let left = self.analyze_pair(iter.next().unwrap())?;
                let op = iter.next().unwrap();
                let right = self.analyze_pair(iter.next().unwrap())?;
                match (left.kind(), op.as_str(), right.kind()) {
                    (Kind::Byte, "+", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Add(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "-", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Sub(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (t @ Kind::Pointer(..), "+", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Add(left, right, loc));
                        Ok(Value::Memory(loc, t))
                    }
                    (t @ Kind::Pointer(..), "-", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Sub(left, right, loc));
                        Ok(Value::Memory(loc, t))
                    }
                    (Kind::Byte, "*", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Mul(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "/", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Div(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "%", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Mod(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "|", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Or(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "^", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Xor(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "&", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::And(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, ">>", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Shr(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "<<", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Shl(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "**", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Pow(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (a, "==", b) if a == b => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Eq(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (a, "!=", b) if a == b => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Neq(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "<", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Lt(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "<=", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Le(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, ">", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Gt(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, ">=", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Ge(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Pointer(..), "<", Kind::Pointer(..)) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Lt(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Pointer(..), "<=", Kind::Pointer(..)) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Le(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Pointer(..), ">", Kind::Pointer(..)) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Gt(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Pointer(..), ">=", Kind::Pointer(..)) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code.push(Instruction::Ge(left, right, loc));
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
                let cond = self.analyze_pair(cond_pair.clone())?;
                if cond.kind() != Kind::Byte {
                    error!(R cond_pair => format!("Condition in a ternary expression must be a <byte>, and not a <{}>", cond.kind()));
                }
                let then = self.analyze_pair(iter.next().unwrap())?;
                let kind = then.kind();
                let otherwise_pair = iter.next().unwrap();
                let otherwise = self.analyze_pair(otherwise_pair.clone())?;
                if otherwise.kind() != kind {
                    error!(R pair => format!("Both branches of the ternary expression don't match! One returns a <{}> while other returns a <{}>", kind, otherwise.kind()));
                }

                let loc = self.loc;
                self.loc += 1;

                self.code
                    .push(Instruction::TernaryIf(cond, then, otherwise, loc));
                Ok(Value::Memory(loc, kind))
            }
            Rule::increment => {
                let value = self.analyze_pair(pair.clone().into_inner().next().unwrap())?;
                if !matches!(value.kind(), Kind::Byte | Kind::Pointer(..)) {
                    error!(R pair => format!("Cannot increment a <{}>", value.kind()));
                }
                self.code.push(Instruction::Inc(value));
                Ok(Value::None)
            }
            Rule::decrement => {
                let value = self.analyze_pair(pair.clone().into_inner().next().unwrap())?;
                if !matches!(value.kind(), Kind::Byte | Kind::Pointer(..)) {
                    error!(R pair => format!("Cannot decrement a <{}>", value.kind()));
                }
                self.code.push(Instruction::Dec(value));
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
                let value = self.analyze_pair(next)?;
                if matches!(kind, Some(ref kind) if *kind != value.kind()) {
                    error!(R pair => format!("{} should be a <{}> but it is a <{}>", ident, kind.unwrap(), value.kind()));
                }

                if ident == "_" {
                    return Ok(Value::None);
                }

                match value {
                    Value::Memory(_, Kind::Ref(..)) => {
                        self.insert(ident, value);
                    }
                    Value::Memory(i, t) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.code
                            .push(Instruction::Copy(Value::Memory(i, t.clone()), loc));
                        self.insert(ident, Value::Memory(loc, t));
                    }
                    Value::Ref(val) => {
                        if let Value::Memory(i, _) = &*val {
                            self.insert(ident, Value::Memory(*i, Kind::Ref(Box::new(val.kind()))));
                        } else {
                            self.insert(ident, Value::Ref(Box::new(Value::Ref(val))));
                        }
                    }
                    val => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert(ident, Value::Memory(loc, val.kind()));
                        self.code.push(Instruction::Copy(val, loc));
                    }
                }

                Ok(Value::None)
            }
            Rule::reassign => {
                // TODO
                let mut iter = pair.clone().into_inner();
                let ident = iter.next().unwrap();
                let op = iter.next().unwrap(); // TODO
                let value = self.analyze_pair(iter.next().unwrap())?;
                let mut entry = if let Some(e) = self.map.iter_mut().rev().find_map(|map| match map
                    .entry(ident.as_str())
                {
                    Entry::Occupied(e) => Some(e),
                    _ => None,
                }) {
                    e
                } else {
                    error!(R ident => format!("Cannot find {} in current scope", ident.as_str()));
                };

                let prev = entry.get().clone();
                let value = Self::reassign_op(
                    &mut self.loc,
                    &mut self.code,
                    &prev,
                    value,
                    op,
                    &pair,
                    ident.as_str(),
                )?;

                match prev {
                    Value::Memory(_, Kind::Ref(..)) => {
                        entry.insert(value);
                    }
                    Value::Memory(i, _) => {
                        self.code.push(Instruction::Copy(value, i));
                    }
                    Value::Ref(ref val) => {
                        if let Value::Memory(i, _) = &**val {
                            entry.insert(Value::Memory(*i, Kind::Ref(Box::new(val.kind()))));
                        } else {
                            entry.insert(Value::Ref(Box::new(prev)));
                        }
                    }
                    _ => unreachable!(),
                }

                Ok(Value::None)
            }
            Rule::deref_assign => {
                // TODO
                let mut iter = pair.clone().into_inner();
                let derefered = iter.next().unwrap();
                let mut derefered_iter = derefered.clone().into_inner();
                let derefs = derefered_iter.next().unwrap().as_str().len();
                let expr_pair = derefered_iter.next().unwrap();
                let value = self.analyze_pair(iter.next().unwrap())?;
                let expr = self.analyze_pair(expr_pair)?;

                fn derefed(k: Kind, derefs: usize) -> Option<Kind> {
                    if derefs == 0 {
                        return Some(k);
                    }
                    match k {
                        Kind::Pointer(k) | Kind::Ref(k) => derefed(*k, derefs - 1),
                        _ => None,
                    }
                }

                if let Some(k) = derefed(expr.kind(), derefs) {
                    if k != value.kind() {
                        error!(R pair => format!("{} is a <{}> but is assigned to a <{}>", derefered.as_str(), k, value.kind()));
                    }
                } else {
                    error!(R derefered => format!("Cannot dereference a <{}> {} times", expr.kind(), derefs));
                }

                self.code
                    .push(Instruction::DerefAssign(expr, derefs, value));

                Ok(Value::None)
            }
            Rule::index_assign => {
                // TODO
                let mut iter = pair.into_inner();
                let indexed_pair = iter.next().unwrap();
                let mut indexed_iter = indexed_pair.clone().into_inner();
                let expr_pair = indexed_iter.next().unwrap();
                let index_pair = indexed_iter.next().unwrap();
                let expr = self.analyze_pair(expr_pair.clone())?;
                let index = self.analyze_pair(index_pair.clone())?;
                let expr_kind = expr.kind();
                let index_kind = index.kind();
                let kind = if let Kind::Pointer(k) = expr_kind.clone() {
                    *k
                } else {
                    error!(R expr_pair => format!("Cannot index a <{}>", expr_kind));
                };
                if index_kind != Kind::Byte {
                    error!(R expr_pair => format!("Cannot index a <{}> with a <{}>", index_kind, index_kind));
                }
                let loc = self.loc;
                self.loc += 2;
                self.code.push(Instruction::Add(expr, index, loc));
                self.code
                    .push(Instruction::Deref(Value::Memory(loc, expr_kind), loc + 1));
                Ok(Value::Memory(loc + 1, kind))
            }
            Rule::if_stmt => {
                let mut iter = pair.into_inner();
                drop(iter.next());
                let cond_pair = iter.next().unwrap();
                let cond = self.analyze_pair(cond_pair.clone())?;
                if cond.kind() != Kind::Byte {
                    error!(R cond_pair => format!("Condition in an if statement must be a <byte>, and not a <{}>", cond.kind()));
                }
                let then = iter.next().unwrap();
                drop(iter.next());
                let otherwise = iter.next();
                let is_otherwise = otherwise.is_some();

                let loc = self.loc;
                self.loc += 1;
                self.code.push(Instruction::If(cond, loc, is_otherwise));

                self.analyze_pair(then.clone())?;
                if let Some(otherwise) = otherwise {
                    self.code.push(Instruction::Else(loc));
                    self.analyze_pair(otherwise.clone())?;
                }
                self.code.push(Instruction::EndIf(loc, is_otherwise));
                Ok(Value::None)
            }
            Rule::while_stmt => {
                let mut iter = pair.into_inner();
                drop(iter.next());
                let cond_pair = iter.next().unwrap();
                let mut cond = self.analyze_pair(cond_pair.clone())?;
                if cond.kind() != Kind::Byte {
                    error!(R cond_pair => format!("Condition in a while statement must be a <byte>, and not a <{}>", cond.kind()));
                }
                let loc = self.loc;
                self.loc += 1;
                self.code.push(Instruction::Copy(cond, loc));
                cond = Value::Memory(loc, Kind::Byte);

                self.code.push(Instruction::While(cond.clone()));

                let stmt = iter.next().unwrap();
                self.analyze_pair(stmt.clone())?;

                if let Value::Memory(i, _) = &cond {
                    let new_cond = self.analyze_pair(cond_pair)?;
                    if new_cond != cond {
                        self.code.push(Instruction::Copy(new_cond, *i));
                    }
                }
                self.code.push(Instruction::EndWhile(cond));
                Ok(Value::None)
            }
            Rule::for_stmt => {
                let mut iter = pair.into_inner();
                drop(iter.next());
                let init = iter.next().unwrap();
                let cond_pair = iter.next().unwrap();
                let step = iter.next().unwrap();
                let body = iter.next().unwrap();

                self.analyze_pair(init.clone())?;

                let mut cond = self.analyze_pair(cond_pair.clone())?;
                if cond.kind() != Kind::Byte {
                    error!(R cond_pair => format!("Condition in a for statement must be a <byte>, and not a <{}>", cond.kind()));
                }

                let loc = self.loc;
                self.loc += 1;
                self.code.push(Instruction::Copy(cond, loc));
                cond = Value::Memory(loc, Kind::Byte);

                self.code.push(Instruction::While(cond.clone()));

                self.analyze_pair(body.clone())?;
                self.analyze_pair(step.clone())?;

                if let Value::Memory(i, _) = &cond {
                    let new_cond = self.analyze_pair(cond_pair)?;
                    if new_cond != cond {
                        self.code.push(Instruction::Copy(new_cond, *i));
                    }
                }
                self.code.push(Instruction::EndWhile(cond));
                Ok(Value::None)
            }
            Rule::out => {
                let expr_pair = pair.clone().into_inner().nth(1).unwrap();
                let expr = self.analyze_pair(expr_pair.clone())?;
                if expr.kind() == Kind::Byte {
                    self.code.push(Instruction::Out(expr));
                } else {
                    error!(R expr_pair => format!("Cannot write a <{}> to stdout", expr.kind()))
                }
                Ok(Value::None)
            }
            Rule::input => {
                let loc = self.loc;
                self.loc += 1;
                self.code.push(Instruction::Input(loc));
                Ok(Value::Memory(loc, Kind::Byte))
            }
            _ => unreachable!("{pair}"),
        }
    }

    fn reassign_op(
        loc: &mut usize,
        code: &mut Vec<Instruction>,
        prev: &Value,
        value: Value,
        op: Pair<'a, Rule>,
        pair: &Pair<'a, Rule>,
        ident: &str,
    ) -> Result<Value, Error> {
        Ok(match (prev.kind(), op.as_str().as_bytes(), value.kind()) {
            (Kind::Byte, b"+=", Kind::Byte) => {
                let new_loc = *loc;
                *loc += 1;
                code.push(Instruction::Add(prev.clone(), value, new_loc));
                Value::Memory(new_loc, Kind::Byte)
            }
            (Kind::Byte, b"-=", Kind::Byte) => {
                let new_loc = *loc;
                *loc += 1;
                code.push(Instruction::Sub(prev.clone(), value, new_loc));
                Value::Memory(new_loc, Kind::Byte)
            }
            (t @ Kind::Pointer(..), b"+=", Kind::Byte) => {
                let new_loc = *loc;
                *loc += 1;
                code.push(Instruction::Add(prev.clone(), value, new_loc));
                Value::Memory(new_loc, t)
            }
            (t @ Kind::Pointer(..), b"-=", Kind::Byte) => {
                let new_loc = *loc;
                *loc += 1;
                code.push(Instruction::Sub(prev.clone(), value, new_loc));
                Value::Memory(new_loc, t)
            }
            (Kind::Byte, b"*=", Kind::Byte) => {
                let new_loc = *loc;
                *loc += 1;
                code.push(Instruction::Mul(prev.clone(), value, new_loc));
                Value::Memory(new_loc, Kind::Byte)
            }
            (Kind::Byte, b"/=", Kind::Byte) => {
                let new_loc = *loc;
                *loc += 1;
                code.push(Instruction::Div(prev.clone(), value, new_loc));
                Value::Memory(new_loc, Kind::Byte)
            }
            (Kind::Byte, b"%=", Kind::Byte) => {
                let new_loc = *loc;
                *loc += 1;
                code.push(Instruction::Mod(prev.clone(), value, new_loc));
                Value::Memory(new_loc, Kind::Byte)
            }
            (Kind::Byte, b"|=", Kind::Byte) => {
                let new_loc = *loc;
                *loc += 1;
                code.push(Instruction::Or(prev.clone(), value, new_loc));
                Value::Memory(new_loc, Kind::Byte)
            }
            (Kind::Byte, b"^=", Kind::Byte) => {
                let new_loc = *loc;
                *loc += 1;
                code.push(Instruction::Xor(prev.clone(), value, new_loc));
                Value::Memory(new_loc, Kind::Byte)
            }
            (Kind::Byte, b"&=", Kind::Byte) => {
                let new_loc = *loc;
                *loc += 1;
                code.push(Instruction::And(prev.clone(), value, new_loc));
                Value::Memory(new_loc, Kind::Byte)
            }
            (Kind::Byte, b">>=", Kind::Byte) => {
                let new_loc = *loc;
                *loc += 1;
                code.push(Instruction::Shr(prev.clone(), value, new_loc));
                Value::Memory(new_loc, Kind::Byte)
            }
            (Kind::Byte, b"<<=", Kind::Byte) => {
                let new_loc = *loc;
                *loc += 1;
                code.push(Instruction::Shl(prev.clone(), value, new_loc));
                Value::Memory(new_loc, Kind::Byte)
            }
            (Kind::Byte, b"**=", Kind::Byte) => {
                let new_loc = *loc;
                *loc += 1;
                code.push(Instruction::Pow(prev.clone(), value, new_loc));
                Value::Memory(new_loc, Kind::Byte)
            }
            (a, b"=", b) => {
                if a == b {
                    value
                } else {
                    error!(R pair => format!("{} is a <{}> but is assigned to a <{}>", ident, a, b));
                }
            }
            (l, op, r) => {
                let op = std::str::from_utf8(op.split_last().unwrap().1).unwrap();
                error!(R pair => format!("Cannot apply operator {} to a <{}> and a <{}>", op, l, r))
            }
        })
    }
}
