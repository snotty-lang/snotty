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

type Sharable<T> = Rc<RefCell<T>>;

#[derive(Debug, Default)]
pub struct Scope<'a> {
    map: Sharable<Vec<HashMap<&'a str, Value>>>,
    code: Sharable<Vec<Instruction>>,
    loc: usize,
}

impl<'a> Scope<'a> {
    pub fn new() -> Scope<'a> {
        Self::default()
    }

    #[inline]
    fn insert_instruction(&self, instruction: Instruction) {
        self.code.borrow_mut().push(instruction)
    }

    #[inline]
    pub fn get(&self, ident: &'a str) -> Option<Value> {
        self.map
            .borrow()
            .iter()
            .rev()
            .find_map(|map| map.get(ident))
            .cloned()
    }

    #[inline]
    pub fn insert(&self, ident: &'a str, value: Value) {
        self.map
            .borrow_mut()
            .last_mut()
            .unwrap()
            .insert(ident, value);
    }

    pub fn code(self) -> Sharable<Vec<Instruction>> {
        self.code
    }

    fn destroy(&mut self) {
        self.map.borrow_mut().pop();
    }

    pub fn child(&self) -> Scope<'a> {
        let map = self.map.clone();
        map.borrow_mut().push(HashMap::new());
        Scope {
            map,
            code: self.code.clone(),
            loc: self.loc,
        }
    }

    pub fn push(&mut self, pair: Pair<'a, Rule>) -> Result<(), Error> {
        match pair.as_rule() {
            Rule::EOI => Ok(()),
            Rule::stmt => self
                .from_pair(pair.into_inner().next().unwrap())
                .map(|_| ()),
            Rule::scope => {
                let mut child = self.child();
                for stmt in pair.into_inner() {
                    child.push(stmt)?;
                }
                child.destroy();
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
                let mut child = self.child();
                for stmt in pair.into_inner() {
                    child.push(stmt)?;
                }
                child.destroy();
                Ok(Value::None)
            }
            Rule::type_cast => {
                let mut iter = pair.clone().into_inner();
                let kind = Kind::from_pair(iter.next().unwrap(), self)?;
                let expr = self.from_pair(iter.next().unwrap())?;
                fn cast(pair: Pair<Rule>, expr: Value, kind: Kind) -> Result<Value, Error> {
                    match (expr, kind) {
                        (expr, kind) if expr.kind() == kind => Ok(expr),
                        (Value::Memory(i, t1), t2) if t1.get_size() == t2.get_size() => {
                            Ok(Value::Memory(i, t2))
                        }
                        (Value::Ref(t), kind) if t.get_size() == kind.get_size() => {
                            cast(pair, *t, kind)
                        }
                        (expr, Kind::Ref(t)) if expr.get_size() == t.get_size() => {
                            Ok(Value::Ref(Box::new(cast(pair, expr, *t)?)))
                        }
                        (Value::Pointer(i, t1), Kind::Pointer(t2))
                            if t1.get_size() == t2.get_size() =>
                        {
                            Ok(Value::Pointer(i, *t2))
                        }
                        (Value::Byte(i), Kind::Pointer(t)) => Ok(Value::Pointer(i as usize, *t)),
                        (expr, kind) => {
                            error!(pair => format!("Cannot cast a <{}> into a <{}>", expr.kind(), kind))
                        }
                    }
                }
                cast(pair, expr, kind)
            }
            Rule::ident => self.get(pair.as_str()).ok_or_else(
                || error!(E pair => format!("Cannot find {} in current scope", pair.as_str())),
            ),
            Rule::array => {
                let mut iter = pair.clone().into_inner();
                let kind = Kind::from_pair(iter.next().unwrap(), self)?;
                let loc = self.loc;
                for (i, element) in iter.enumerate() {
                    let element = self.from_pair(element)?;
                    if element.kind() != kind {
                        error!(R pair => format!("Found a <{}> in an array of <{}>s", element.kind(), kind));
                    }
                    self.insert_instruction(Instruction::Copy(element, loc + i));
                    self.loc += 1;
                }
                Ok(Value::Pointer(loc, kind))
            }
            Rule::string => {
                let loc = self.loc;
                for element in pair.into_inner() {
                    let element = self.from_pair(element)?;
                    self.insert_instruction(Instruction::Copy(element, self.loc));
                    self.loc += 1;
                }
                self.insert_instruction(Instruction::Copy(Value::Byte(0), self.loc));
                self.loc += 1;
                Ok(Value::Pointer(loc, Kind::Byte))
            }
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
                            self.insert_instruction(Instruction::Deref(expr, loc));
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
                        self.insert_instruction(Instruction::Neg(expr, loc));
                        Ok(Value::Memory(loc, kind))
                    }
                    "!" => {
                        let kind = expr.kind();
                        if kind != Kind::Byte {
                            error!(R pair => format!("Cannot not a <{}>", kind));
                        }
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Not(expr, loc));
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
                        self.insert_instruction(Instruction::Add(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "-", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Sub(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (t @ Kind::Pointer(..), "+", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Add(left, right, loc));
                        Ok(Value::Memory(loc, t))
                    }
                    (t @ Kind::Pointer(..), "-", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Sub(left, right, loc));
                        Ok(Value::Memory(loc, t))
                    }
                    (Kind::Byte, "*", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Mul(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "/", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Div(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "%", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Mod(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "|", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Or(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "^", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Xor(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "&", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::And(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, ">>", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Shr(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "<<", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Shl(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "**", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Pow(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (a, "==", b) if a == b => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Eq(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (a, "!=", b) if a == b => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Neq(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "<", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Lt(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "<=", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Le(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, ">", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Gt(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, ">=", Kind::Byte) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Ge(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Pointer(..), "<", Kind::Pointer(..)) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Lt(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Pointer(..), "<=", Kind::Pointer(..)) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Le(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Pointer(..), ">", Kind::Pointer(..)) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Gt(left, right, loc));
                        Ok(Value::Memory(loc, Kind::Byte))
                    }
                    (Kind::Pointer(..), ">=", Kind::Pointer(..)) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Ge(left, right, loc));
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

                self.insert_instruction(Instruction::TernaryIf(cond, then, otherwise, loc));
                Ok(Value::Memory(loc, kind))
            }
            Rule::increment => {
                let value = self.from_pair(pair.clone().into_inner().next().unwrap())?;
                if !matches!(value.kind(), Kind::Byte | Kind::Pointer(..)) {
                    error!(R pair => format!("Cannot increment a <{}>", value.kind()));
                }
                self.insert_instruction(Instruction::Inc(value));
                Ok(Value::None)
            }
            Rule::decrement => {
                let value = self.from_pair(pair.clone().into_inner().next().unwrap())?;
                if !matches!(value.kind(), Kind::Byte | Kind::Pointer(..)) {
                    error!(R pair => format!("Cannot decrement a <{}>", value.kind()));
                }
                self.insert_instruction(Instruction::Dec(value));
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
                        self.insert(ident, value);
                    }
                    Value::Memory(i, t) => {
                        let loc = self.loc;
                        self.loc += 1;
                        self.insert_instruction(Instruction::Copy(
                            Value::Memory(i, t.clone()),
                            loc,
                        ));
                        self.insert(ident, Value::Memory(loc, t));
                    }
                    Value::Ref(val) => {
                        if let Value::Memory(i, _) = &*val {
                            self.insert(ident, Value::Memory(*i, Kind::Ref(Box::new(val.kind()))));
                        } else {
                            self.insert(ident, Value::Ref(Box::new(Value::Ref(val))));
                        }
                    }
                    _ => {
                        self.insert(ident, value);
                    }
                }

                Ok(Value::None)
            }
            Rule::reassign => {
                let mut iter = pair.clone().into_inner();
                let ident = iter.next().unwrap();
                let value = self.from_pair(iter.next().unwrap())?;
                let mut borrow = self.map.borrow_mut();
                let mut entry = if let Some(e) =
                    borrow
                        .iter_mut()
                        .rev()
                        .find_map(|map| match map.entry(ident.as_str()) {
                            Entry::Occupied(e) => Some(e),
                            _ => None,
                        }) {
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
                        self.insert_instruction(Instruction::Copy(
                            Value::Memory(i, t.clone()),
                            loc,
                        ));
                        entry.insert(Value::Memory(loc, t));
                    }
                    Value::Ref(val) => {
                        if let Value::Memory(i, _) = &*val {
                            entry.insert(Value::Memory(*i, Kind::Ref(Box::new(val.kind()))));
                        } else {
                            entry.insert(Value::Ref(Box::new(Value::Ref(val))));
                        }
                    }
                    _ => {
                        entry.insert(value);
                    }
                }

                Ok(Value::None)
            }
            Rule::deref_assign => {
                let mut iter = pair.clone().into_inner();
                let mut ident = iter.next().unwrap();
                let mut derefs = 0;
                while ident.as_rule() == Rule::deref_sign {
                    derefs += 1;
                    ident = iter.next().unwrap();
                }
                println!("{derefs}"); // !TODO
                let value = self.from_pair(iter.next().unwrap())?;
                let old = if let Some(v) = self.get(ident.as_str()) {
                    v
                } else {
                    error!(R ident => format!("Cannot find {} in current scope", ident.as_str()));
                };

                match old.kind() {
                    Kind::Pointer(t) | Kind::Ref(t) => {
                        if *t != value.kind() {
                            error!(R pair => format!("{} is a <{}> but is assigned to a <{}>", ident.as_str(), old.kind(), value.kind()));
                        }
                        self.insert_instruction(Instruction::DerefAssign(old, value))
                    }
                    k => {
                        error!(R ident => format!("Cannot dereference a <{}>", k));
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
                self.insert_instruction(Instruction::If(cond, loc, is_otherwise));

                self.from_pair(then.clone())?;
                if let Some(otherwise) = otherwise {
                    self.insert_instruction(Instruction::Else(loc));
                    self.from_pair(otherwise.clone())?;
                }
                self.insert_instruction(Instruction::EndIf(loc, is_otherwise));
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
                self.insert_instruction(Instruction::Copy(cond, loc));
                cond = Value::Memory(loc, Kind::Byte);

                self.insert_instruction(Instruction::While(cond.clone()));

                let stmt = iter.next().unwrap();
                self.from_pair(stmt.clone())?;

                if let Value::Memory(i, _) = &cond {
                    let new_cond = self.from_pair(cond_pair)?;
                    if new_cond != cond {
                        self.insert_instruction(Instruction::Copy(new_cond, *i));
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
                self.insert_instruction(Instruction::Copy(cond, loc));
                cond = Value::Memory(loc, Kind::Byte);

                self.insert_instruction(Instruction::While(cond.clone()));

                self.from_pair(body.clone())?;
                self.from_pair(step.clone())?;

                if let Value::Memory(i, _) = &cond {
                    let new_cond = self.from_pair(cond_pair)?;
                    if new_cond != cond {
                        self.insert_instruction(Instruction::Copy(new_cond, *i));
                    }
                }

                Ok(Value::None)
            }
            Rule::out => {
                let expr_pair = pair.clone().into_inner().nth(1).unwrap();
                let expr = self.from_pair(expr_pair.clone())?;
                if expr.kind() == Kind::Byte {
                    self.insert_instruction(Instruction::Out(expr));
                } else {
                    error!(R expr_pair => format!("Cannot write a <{}> to stdout", expr.kind()))
                }
                Ok(Value::None)
            }
            Rule::input => {
                let loc = self.loc;
                self.loc += 1;
                self.insert_instruction(Instruction::Input(loc));
                Ok(Value::Memory(loc, Kind::Byte))
            }
            _ => unreachable!("{pair}"),
        }
    }
}
