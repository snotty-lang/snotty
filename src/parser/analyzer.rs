use std::{collections::HashMap, fs};

use pest::{iterators::Pair, Parser};

use crate::{
    error, format_error,
    parser::{
        instruction::Instruction,
        value::{BaseValue, Kind, Value},
        Error, Memory, Rule, SnottyParser, IR,
    },
};

#[derive(Debug, Clone, Copy)]
struct ProgramMemory {
    used: Memory,
    cleared: Memory,
}

#[derive(Debug)]
pub struct Analyzer<'a> {
    included: Vec<String>,
    fxs: Vec<Vec<Instruction>>,
    map: Vec<HashMap<&'a str, Value>>,
    code: Vec<Instruction>,
    loc: Memory,
    memory: ProgramMemory,
    errors: Vec<Error>,
}

impl<'a> Analyzer<'a> {
    pub fn new(file: String) -> Analyzer<'a> {
        Analyzer {
            fxs: vec![],
            included: vec![file],
            map: vec![HashMap::new()],
            code: Vec::new(),
            loc: 0,
            memory: ProgramMemory {
                used: 0,
                cleared: 0,
            },
            errors: vec![],
        }
    }

    pub fn into_ir(self) -> Result<IR, Vec<Error>> {
        if self.errors.is_empty() {
            Ok(IR {
                memory_used: self.memory.used,
                code: self.code,
                fxs: self.fxs,
            })
        } else {
            Err(self.errors)
        }
    }

    #[inline]
    pub fn get(&self, ident: &'a str) -> Option<&Value> {
        self.map.iter().rev().find_map(|map| map.get(ident))
    }

    #[inline]
    pub fn get_mut(&mut self, ident: &'a str) -> Option<&mut Value> {
        self.map.iter_mut().rev().find_map(|map| map.get_mut(ident))
    }

    #[inline]
    pub fn insert(&mut self, ident: &'a str, value: Value) {
        self.map.last_mut().unwrap().insert(ident, value);
    }

    #[inline]
    pub fn memory(&mut self) -> Memory {
        let curr = self.loc;
        self.loc += 1;
        if self.memory.cleared.checked_sub(1).is_none() {
            self.memory.used += 1;
        }
        curr
    }

    #[inline]
    pub fn alloc(&mut self, mem: Memory) -> Memory {
        let curr = self.loc;
        self.loc += mem;
        self.inc_mem(mem);
        curr
    }

    #[inline]
    fn inc_mem(&mut self, mem: Memory) {
        if self.memory.cleared >= mem {
            self.memory.cleared -= mem;
        } else {
            self.memory.used += mem - self.memory.cleared;
            self.memory.cleared = 0;
        }
    }

    pub fn push(&mut self, pair: Pair<'a, Rule>) {
        match pair.as_rule() {
            Rule::EOI => (),
            _ => {
                if let Err(err) = self.analyze_pair(pair.into_inner().next().unwrap()) {
                    self.errors.push(if err.path().is_none() {
                        err.with_path(self.included.last().unwrap())
                    } else {
                        err
                    })
                }
            }
        }
    }

    fn analyze_pair(&mut self, pair: Pair<'a, Rule>) -> Result<Value, Error> {
        match pair.as_rule() {
            Rule::expr => self.analyze_pair(pair.into_inner().next().unwrap()),
            Rule::number => Ok(Value::byte(
                pair.as_str()
                    .parse()
                    .map_err(|_| error!(E pair => format!("Byte overflow")))?,
            )),
            Rule::boolean => Ok(Value::byte((pair.as_str().trim() == "true").into())),
            Rule::none => Ok(Value::none()),
            Rule::char => Ok(Value::byte({
                let span = pair.as_span();
                Self::make_char(pair.into_inner().next().unwrap().as_str().as_bytes())
                    .ok_or_else(|| error!(ES span => format!("Byte overflow")))?
            })),
            Rule::pointer => {
                let value = self.analyze_pair(pair.into_inner().next().unwrap())?;
                match &value.value {
                    BaseValue::Memory(i) => Ok(Value::pointer(*i, value.kind)),
                    BaseValue::None => Ok(Value::pointer(0, Kind::None)),
                    _ => {
                        let loc = self.memory();
                        let kind = value.kind().clone();
                        self.code.push(Instruction::Copy(value, loc));
                        Ok(Value::pointer(loc, kind))
                    }
                }
            }
            Rule::stmt => self.analyze_pair(pair.into_inner().next().unwrap()),
            Rule::scope => {
                self.map.push(HashMap::new());
                let prev = self.loc;
                for stmt in pair.into_inner() {
                    self.push(stmt);
                }
                let diff = self.loc - prev;
                if self.loc != prev {
                    self.code.push(Instruction::Clear(prev, self.loc));
                }
                self.memory.cleared += diff;
                self.loc = prev;
                self.map.pop();
                Ok(Value::none())
            }
            Rule::type_cast => {
                let span = pair.as_span();
                let mut iter = pair.into_inner();
                let kind = Kind::from_pair(iter.next().unwrap(), self)?;
                let expr = self.analyze_pair(iter.next().unwrap())?;
                let expr_kind = expr.kind().clone();

                fn cast(
                    expr: Value,
                    kind: Kind,
                    push: &mut impl FnMut(Value) -> Memory,
                ) -> Option<Value> {
                    if *expr.kind() == kind {
                        return Some(expr);
                    }
                    match (&expr.value, kind) {
                        (BaseValue::Memory(i), t2) => Some(Value::memory(*i, t2)),
                        (BaseValue::Pointer(i), Kind::Pointer(t2)) => Some(Value::pointer(*i, *t2)),
                        (BaseValue::Byte(i), Kind::Pointer(k)) => {
                            Some(Value::pointer(*i as Memory, *k))
                        }
                        (BaseValue::Ref(k), Kind::Pointer(kind)) if k.kind() == &*kind => {
                            let loc = match &(*k).value {
                                BaseValue::Memory(i) => Some(*i),
                                _ => None,
                            };
                            let val = cast(*k.clone(), *kind.clone(), push)?;
                            Some(Value::pointer(loc.unwrap_or_else(|| (push)(val)), *kind))
                        }
                        (BaseValue::Ref(k), kind) if k.kind().get_size() == kind.get_size() => {
                            cast(*k.clone(), kind, push)
                        }
                        (_, Kind::Ref(k)) if expr.kind().get_size() == k.get_size() => {
                            Some(Value::reference(cast(expr, *k, push)?))
                        }
                        (val, Kind::Pointer(k)) if expr.kind().get_size() == k.get_size() => {
                            match val {
                                BaseValue::Memory(i) => Some(Value::pointer(*i, expr.kind)),
                                _ => Some(Value::pointer(push(expr), *k)),
                            }
                        }
                        _ => None,
                    }
                }

                let push = &mut |val: Value| {
                    let loc = self.memory();
                    self.code.push(Instruction::Copy(val, loc));
                    loc
                };

                cast(expr, kind.clone(), push).ok_or_else(
                    || error!(ES span => format!("Cannot cast a <{}> into a <{}>", expr_kind, kind)),
                )
            }
            Rule::ident => self
                .get(pair.as_str())
                .ok_or_else(
                    || error!(E pair => format!("Cannot find {} in current scope", pair.as_str())),
                )
                .cloned(),
            Rule::string => {
                let loc = self.loc;
                let span = pair.as_span();
                for element in pair.into_inner() {
                    let byte = Self::make_char(element.as_str().as_bytes())
                        .ok_or_else(|| error!(ES span => format!("Byte overflow")))?;
                    let mem = self.memory();
                    self.code.push(Instruction::Copy(Value::byte(byte), mem));
                }
                let mem = self.memory();
                self.code.push(Instruction::Copy(Value::byte(0), mem));
                Ok(Value::pointer(loc, Kind::Byte))
            }
            Rule::file => {
                let span = pair.as_span();
                let mut iter = pair.into_inner();
                drop(iter.next());
                let file = iter
                    .next()
                    .unwrap()
                    .into_inner()
                    .map(|element| {
                        Self::make_char(element.as_str().as_bytes())
                            .ok_or_else(|| error!(ES span => format!("Byte overflow")))
                            .map(|a| a as char)
                    })
                    .try_fold(String::new(), |mut acc, e| {
                        acc.push(e?);
                        Ok(acc)
                    })?;

                if self.included.contains(&file) {
                    return Ok(Value::none());
                }

                match fs::read_to_string(&file) {
                    Err(err) => error!(S span => format!("Cannot include file: {err}")),
                    Ok(program) => {
                        let program = SnottyParser::parse(Rule::program, &program)
                            .map_err(|err| format_error(err.with_path(&file)))?;
                        let mut included = self.included.clone();
                        included.push(file);

                        let mut new = Analyzer {
                            fxs: vec![],
                            included,
                            map: vec![HashMap::new()],
                            code: Vec::new(),
                            loc: self.loc,
                            memory: self.memory,
                            errors: vec![],
                        };

                        for code in program {
                            new.push(code);
                        }
                        self.code.extend(new.code);
                        self.errors.extend(new.errors);
                        self.fxs.extend(new.fxs);
                        self.memory = new.memory;
                        Ok(Value::none())
                    }
                }
            }
            Rule::unop_expr => {
                let span = pair.as_span();
                let mut iter = pair.into_inner();
                let op = iter.next().unwrap();
                let expr = self.analyze_pair(iter.next().unwrap())?;
                match op.as_str() {
                    "&" => Ok(Value::reference(expr)),
                    "*" => match (&expr.value, &expr.kind) {
                        (BaseValue::Pointer(_), Kind::None) => {
                            error!(S span => "Cannot dereference a <*;>".to_string())
                        }
                        (BaseValue::Pointer(i), Kind::Pointer(ref k)) => {
                            Ok(Value::memory(*i, *k.clone()))
                        }
                        (BaseValue::Memory(_), Kind::Pointer(ref k)) => {
                            let loc = self.memory();
                            let k = *k.clone();
                            self.code.push(Instruction::Deref(expr, loc));
                            Ok(Value::memory(loc, k))
                        }
                        (BaseValue::Memory(i), Kind::Ref(k)) => Ok(Value::memory(*i, *k.clone())),
                        (BaseValue::Ref(k), _) => Ok(*k.clone()),
                        _ => error!(S span => format!("Cannot dereference a <{}>", expr.kind())),
                    },
                    "-" => {
                        let kind = expr.kind().clone();
                        if kind != Kind::Byte {
                            error!(RS span => format!("Cannot negate a <{}>", kind));
                        }
                        let loc = self.memory();
                        self.code.push(Instruction::Neg(expr, loc));
                        Ok(Value::memory(loc, kind))
                    }
                    "!" => {
                        let kind = expr.kind().clone();
                        if kind != Kind::Byte {
                            error!(RS span => format!("Cannot not a <{}>", kind));
                        }
                        let loc = self.memory();
                        self.code.push(Instruction::Not(expr, loc));
                        Ok(Value::memory(loc, kind))
                    }
                    _ => unreachable!(),
                }
            }
            Rule::binop_expr => {
                let span = pair.as_span();
                let mut iter = pair.into_inner();
                let left = self.analyze_pair(iter.next().unwrap())?;
                let op = iter.next().unwrap();
                let right = self.analyze_pair(iter.next().unwrap())?;
                match (left.kind(), op.as_str(), right.kind()) {
                    (Kind::Byte, "+", Kind::Byte) => {
                        let loc = self.memory();
                        self.code.push(Instruction::Add(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "-", Kind::Byte) => {
                        let loc = self.memory();
                        self.code.push(Instruction::Sub(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (Kind::Pointer(..), "+", Kind::Byte) => {
                        let k = left.kind().clone();
                        let loc = self.memory();
                        self.code.push(Instruction::Add(left, right, loc));
                        Ok(Value::memory(loc, k))
                    }
                    (Kind::Pointer(..), "-", Kind::Byte) => {
                        let k = left.kind().clone();
                        let loc = self.memory();
                        self.code.push(Instruction::Sub(left, right, loc));
                        Ok(Value::memory(loc, k))
                    }
                    (Kind::Byte, "*", Kind::Byte) => {
                        let loc = self.memory();
                        self.code.push(Instruction::Mul(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "/", Kind::Byte) => {
                        let loc = self.memory();
                        self.code.push(Instruction::Div(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "%", Kind::Byte) => {
                        let loc = self.memory();
                        self.code.push(Instruction::Mod(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "|", Kind::Byte) => {
                        let loc = self.memory();
                        self.code.push(Instruction::Or(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "^", Kind::Byte) => {
                        let loc = self.memory();
                        self.code.push(Instruction::Xor(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "&", Kind::Byte) => {
                        let loc = self.memory();
                        self.code.push(Instruction::And(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, ">>", Kind::Byte) => {
                        let loc = self.memory();
                        self.code.push(Instruction::Shr(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "<<", Kind::Byte) => {
                        let loc = self.memory();
                        self.code.push(Instruction::Shl(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (a, "==", b) if a == b => {
                        let loc = self.memory();
                        self.code.push(Instruction::Eq(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (a, "!=", b) if a == b => {
                        let loc = self.memory();
                        self.code.push(Instruction::Neq(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "<", Kind::Byte) => {
                        let loc = self.memory();
                        self.code.push(Instruction::Lt(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, "<=", Kind::Byte) => {
                        let loc = self.memory();
                        self.code.push(Instruction::Le(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, ">", Kind::Byte) => {
                        let loc = self.memory();
                        self.code.push(Instruction::Gt(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (Kind::Byte, ">=", Kind::Byte) => {
                        let loc = self.memory();
                        self.code.push(Instruction::Ge(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (Kind::Pointer(..), "<", Kind::Pointer(..)) => {
                        let loc = self.memory();
                        self.code.push(Instruction::Lt(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (Kind::Pointer(..), "<=", Kind::Pointer(..)) => {
                        let loc = self.memory();
                        self.code.push(Instruction::Le(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (Kind::Pointer(..), ">", Kind::Pointer(..)) => {
                        let loc = self.memory();
                        self.code.push(Instruction::Gt(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (Kind::Pointer(..), ">=", Kind::Pointer(..)) => {
                        let loc = self.memory();
                        self.code.push(Instruction::Ge(left, right, loc));
                        Ok(Value::memory(loc, Kind::Byte))
                    }
                    (l, op, r) => {
                        error!(S span => format!("Cannot apply operator {} to a <{}> and a <{}>", op, l, r))
                    }
                }
            }
            Rule::ternary => {
                let span = pair.as_span();
                let mut iter = pair.into_inner();
                drop(iter.next());
                let cond_pair = iter.next().unwrap();
                let cond = self.analyze_pair(cond_pair.clone())?;
                if *cond.kind() != Kind::Byte {
                    error!(R cond_pair => format!("Condition in a ternary expression must be a <byte>, and not a <{}>", cond.kind()));
                }
                let then = self.analyze_pair(iter.next().unwrap())?;
                let kind = then.kind().clone();
                let otherwise_pair = iter.next().unwrap();
                let otherwise = self.analyze_pair(otherwise_pair.clone())?;
                if *otherwise.kind() != kind {
                    error!(RS span => format!("The branches of the ternary expression don'k match. One returns a <{}> while other returns a <{}>", kind, otherwise.kind()));
                }

                let loc = self.memory();

                self.code
                    .push(Instruction::TernaryIf(cond, then, otherwise, loc));
                Ok(Value::memory(loc, kind))
            }
            Rule::increment => {
                let span = pair.as_span();
                let value = self.analyze_pair(pair.into_inner().next().unwrap())?;
                if !matches!(value.kind(), Kind::Byte | Kind::Pointer(..)) {
                    error!(RS span => format!("Cannot increment a <{}>", value.kind()));
                }
                self.code.push(Instruction::Inc(value));
                Ok(Value::none())
            }
            Rule::decrement => {
                let span = pair.as_span();
                let value = self.analyze_pair(pair.into_inner().next().unwrap())?;
                if !matches!(value.kind(), Kind::Byte | Kind::Pointer(..)) {
                    error!(RS span => format!("Cannot decrement a <{}>", value.kind()));
                }
                self.code.push(Instruction::Dec(value));
                Ok(Value::none())
            }
            Rule::assign => {
                let mut iter = pair.into_inner();
                drop(iter.next());
                let ident = iter.next().unwrap().as_str();
                let value = self.analyze_pair(iter.next().unwrap())?;

                if ident == "_" {
                    return Ok(Value::none());
                }

                match (&value.value, value.kind()) {
                    (BaseValue::Memory(_), Kind::Ref(..) | Kind::Function(..))
                    | (BaseValue::Function(..), _) => {
                        self.insert(ident, value);
                    }
                    (BaseValue::Memory(i), k) => {
                        let loc = self.memory();
                        self.code
                            .push(Instruction::Copy(Value::memory(*i, k.clone()), loc));
                        self.insert(ident, Value::memory(loc, value.kind));
                    }
                    (BaseValue::Ref(val), _) => {
                        if let BaseValue::Memory(i) = &(*val).value {
                            self.insert(ident, Value::memory(*i, value.kind));
                        } else {
                            self.insert(ident, Value::reference(value));
                        }
                    }
                    (_, kind) => {
                        let loc = self.memory();
                        self.insert(ident, Value::memory(loc, kind.clone()));
                        self.code.push(Instruction::Copy(value, loc));
                    }
                }

                Ok(Value::none())
            }
            Rule::reassign => {
                let mut iter = pair.clone().into_inner();
                let lhs = iter.next().unwrap();
                let mut lhs_iter = lhs.clone().into_inner();
                let mut prev_pair = lhs_iter.next().unwrap();
                let mut derefs = 0;
                if prev_pair.as_rule() == Rule::deref_signs {
                    derefs = prev_pair.as_str().len();
                    prev_pair = lhs_iter.next().unwrap();
                }
                let op = iter.next().unwrap();
                let new = self.analyze_pair(iter.next().unwrap())?;
                let prev = self.analyze_pair(prev_pair.clone())?;

                fn deref(analyzer: &mut Analyzer, expr: Value, derefs: usize) -> Option<Value> {
                    if derefs == 0 {
                        return Some(expr);
                    }
                    match (&expr.value, &expr.kind) {
                        (BaseValue::Ref(v), _) => deref(analyzer, *v.clone(), derefs - 1),
                        (BaseValue::Memory(i), Kind::Ref(k)) => {
                            deref(analyzer, Value::memory(*i, *k.clone()), derefs - 1)
                        }
                        (BaseValue::Pointer(_), k) => {
                            let k = k.clone();
                            let loc = analyzer.loc;
                            analyzer.loc += 1;
                            analyzer.code.push(Instruction::Deref(expr, loc));
                            deref(analyzer, Value::memory(loc, k), derefs - 1)
                        }
                        (BaseValue::Memory(_), Kind::Pointer(ref k)) => {
                            let k = *k.clone();
                            let loc = analyzer.loc;
                            analyzer.loc += 1;
                            analyzer.code.push(Instruction::Deref(expr, loc));
                            deref(analyzer, Value::memory(loc, k), derefs - 1)
                        }
                        _ => None,
                    }
                }

                let new = if op.as_str() != "=" {
                    let kind = prev.kind();
                    let derefed_prev = if let Some(v) = deref(self, prev.clone(), derefs) {
                        if v.kind() != new.kind() {
                            error!(R pair => format!("{} is a <{}> but is assigned to a <{}>", lhs.as_str(), v.kind(), new.kind()));
                        }
                        v
                    } else {
                        match kind {
                            Kind::Pointer(_) | Kind::Ref(_) => {
                                error!(R lhs => format!("Cannot dereference a <{}> {} times", kind, derefs));
                            }
                            _ => {
                                error!(R lhs => format!("Cannot dereference a <{}>", kind));
                            }
                        }
                    };

                    match (derefed_prev.kind(), op.as_str(), new.kind()) {
                        (Kind::Byte, "+=", Kind::Byte) => {
                            let loc = self.memory();
                            self.code.push(Instruction::Add(derefed_prev, new, loc));
                            Value::memory(loc, Kind::Byte)
                        }
                        (Kind::Byte, "-=", Kind::Byte) => {
                            let loc = self.memory();
                            self.code.push(Instruction::Sub(derefed_prev, new, loc));
                            Value::memory(loc, Kind::Byte)
                        }
                        (k @ Kind::Pointer(..), "+=", Kind::Byte) => {
                            let k = k.clone();
                            let loc = self.memory();
                            self.code.push(Instruction::Add(derefed_prev, new, loc));
                            Value::memory(loc, k)
                        }
                        (k @ Kind::Pointer(..), "-=", Kind::Byte) => {
                            let k = k.clone();
                            let loc = self.memory();
                            self.code.push(Instruction::Sub(derefed_prev, new, loc));
                            Value::memory(loc, k)
                        }
                        (Kind::Byte, "*=", Kind::Byte) => {
                            let loc = self.memory();
                            self.code.push(Instruction::Mul(derefed_prev, new, loc));
                            Value::memory(loc, Kind::Byte)
                        }
                        (Kind::Byte, "/=", Kind::Byte) => {
                            let loc = self.memory();
                            self.code.push(Instruction::Div(derefed_prev, new, loc));
                            Value::memory(loc, Kind::Byte)
                        }
                        (Kind::Byte, "%=", Kind::Byte) => {
                            let loc = self.memory();
                            self.code.push(Instruction::Mod(derefed_prev, new, loc));
                            Value::memory(loc, Kind::Byte)
                        }
                        (Kind::Byte, "|=", Kind::Byte) => {
                            let loc = self.memory();
                            self.code.push(Instruction::Or(derefed_prev, new, loc));
                            Value::memory(loc, Kind::Byte)
                        }
                        (Kind::Byte, "^=", Kind::Byte) => {
                            let loc = self.memory();
                            self.code.push(Instruction::Xor(derefed_prev, new, loc));
                            Value::memory(loc, Kind::Byte)
                        }
                        (Kind::Byte, "&=", Kind::Byte) => {
                            let loc = self.memory();
                            self.code.push(Instruction::And(derefed_prev, new, loc));
                            Value::memory(loc, Kind::Byte)
                        }
                        (Kind::Byte, ">>=", Kind::Byte) => {
                            let loc = self.memory();
                            self.code.push(Instruction::Shr(derefed_prev, new, loc));
                            Value::memory(loc, Kind::Byte)
                        }
                        (Kind::Byte, "<<=", Kind::Byte) => {
                            let loc = self.memory();
                            self.code.push(Instruction::Shl(derefed_prev, new, loc));
                            Value::memory(loc, Kind::Byte)
                        }
                        (l, op, r) => {
                            let op =
                                std::str::from_utf8(op.as_bytes().split_last().unwrap().1).unwrap();
                            error!(R pair => format!("Cannot apply operator {} to a <{}> and a <{}>", op, l, r))
                        }
                    }
                } else {
                    let derefed_kind = if let Some(k) = prev.kind().clone().dereferenced(derefs) {
                        if k != *new.kind() {
                            error!(R pair => format!("{} is a <{}> but is assigned to a <{}>", lhs.as_str(), k, new.kind()));
                        }
                        k
                    } else {
                        let kind = prev.kind();
                        match kind {
                            Kind::Pointer(_) | Kind::Ref(_) => {
                                error!(R lhs => format!("Cannot dereference a <{}> {} times", kind, derefs));
                            }
                            _ => {
                                error!(R lhs => format!("Cannot dereference a <{}>", kind));
                            }
                        }
                    };
                    if derefed_kind == *new.kind() {
                        new
                    } else {
                        error!(R pair => format!("{} is a <{}> but is assigned to a <{}>", lhs, derefed_kind, new.kind()));
                    }
                };

                match (derefs, prev_pair.as_rule()) {
                    (0, Rule::ident) => match (&prev.value, prev.kind()) {
                        (BaseValue::Memory(_), Kind::Ref(..)) => {
                            self.insert(prev_pair.as_str(), new);
                        }
                        (BaseValue::Memory(i), _) => {
                            self.code.push(Instruction::Copy(new, *i));
                        }
                        (BaseValue::Ref(ref val), kind) => {
                            if let BaseValue::Memory(i) = &val.value {
                                self.insert(prev_pair.as_str(), Value::memory(*i, kind.clone()));
                            } else {
                                self.insert(prev_pair.as_str(), Value::reference(prev));
                            }
                        }
                        _ => unreachable!(),
                    },
                    _ => {
                        let mut refs = 0;
                        let derefed = prev.deref_ref(derefs, &mut refs).unwrap();
                        match (&derefed.value, &derefed.kind) {
                            (BaseValue::Pointer(_), _)
                            | (BaseValue::Memory(_), Kind::Pointer(_))
                                if derefs != refs =>
                            {
                                self.code.push(Instruction::DerefAssign(
                                    derefed,
                                    derefs - refs,
                                    new,
                                ))
                            }
                            (BaseValue::Memory(i), _) => self.code.push(Instruction::Copy(new, *i)),
                            _ => error!(R pair => format!("Cannot assign to {}", lhs.as_str())),
                        }
                    }
                }

                Ok(Value::none())
            }
            Rule::if_stmt => {
                let mut iter = pair.into_inner();
                drop(iter.next());
                let cond_pair = iter.next().unwrap();
                let cond = self.analyze_pair(cond_pair.clone())?;
                if *cond.kind() != Kind::Byte {
                    error!(R cond_pair => format!("Condition in an if statement must be a <byte>, and not a <{}>", cond.kind()));
                }
                let then = iter.next().unwrap();
                let otherwise = iter.next();
                let is_otherwise = otherwise.is_some();

                let if_stmt = self.code.len();
                self.code.push(Instruction::If(cond, if_stmt, is_otherwise));

                self.analyze_pair(then.clone())?;
                if let Some(otherwise) = otherwise {
                    self.code.push(Instruction::Else(if_stmt));
                    self.analyze_pair(if otherwise.as_rule() == Rule::otherwise {
                        iter.next().unwrap()
                    } else {
                        otherwise
                    })?;
                }
                self.code.push(Instruction::EndIf(if_stmt, is_otherwise));
                Ok(Value::none())
            }
            Rule::while_stmt => {
                let mut iter = pair.into_inner();
                drop(iter.next());
                let cond_pair = iter.next().unwrap();
                let mut cond = self.analyze_pair(cond_pair.clone())?;
                if *cond.kind() != Kind::Byte {
                    error!(R cond_pair => format!("Condition in a while statement must be a <byte>, and not a <{}>", cond.kind()));
                }
                let loc = self.memory();
                self.code.push(Instruction::Copy(cond, loc));
                cond = Value::memory(loc, Kind::Byte);

                self.code.push(Instruction::While(cond.clone()));

                let stmt = iter.next().unwrap();
                self.analyze_pair(stmt.clone())?;

                if let BaseValue::Memory(i) = &cond.value {
                    let new_cond = self.analyze_pair(cond_pair)?;
                    if new_cond != cond {
                        self.code.push(Instruction::Copy(new_cond, *i));
                    }
                }
                self.code.push(Instruction::EndWhile(cond));
                Ok(Value::none())
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
                if *cond.kind() != Kind::Byte {
                    error!(R cond_pair => format!("Condition in a for statement must be a <byte>, and not a <{}>", cond.kind()));
                }

                let loc = self.memory();
                self.code.push(Instruction::Copy(cond, loc));
                cond = Value::memory(loc, Kind::Byte);

                self.code.push(Instruction::While(cond.clone()));

                self.analyze_pair(body.clone())?;
                self.analyze_pair(step.clone())?;

                if let BaseValue::Memory(i) = &cond.value {
                    let new_cond = self.analyze_pair(cond_pair)?;
                    if new_cond != cond {
                        self.code.push(Instruction::Copy(new_cond, *i));
                    }
                }
                self.code.push(Instruction::EndWhile(cond));
                Ok(Value::none())
            }
            Rule::out => {
                let expr_pair = pair.into_inner().nth(1).unwrap();
                let expr = self.analyze_pair(expr_pair.clone())?;
                if *expr.kind() == Kind::Byte {
                    self.code.push(Instruction::Out(expr));
                } else {
                    error!(R expr_pair => format!("Cannot write a <{}> to stdout", expr.kind()))
                }
                Ok(Value::none())
            }
            Rule::input => {
                let loc = self.memory();
                self.code.push(Instruction::Input(loc));
                Ok(Value::memory(loc, Kind::Byte))
            }
            Rule::function => {
                let mut iter = pair.into_inner();
                drop(iter.next());
                let ident = iter.next().unwrap().as_str();
                let code = std::mem::take(&mut self.code);
                let mut params = Vec::new();
                let mut ret = Kind::None;
                let loc = std::mem::take(&mut self.loc);

                let mut map = HashMap::new();
                while let Some(pair) = iter.next() {
                    match pair.as_rule() {
                        Rule::ident => {
                            let ident = pair.as_str();
                            let kind = Kind::from_pair(iter.next().unwrap(), self)?;
                            map.insert(ident, Value::memory(self.loc, kind.clone()));
                            self.loc += 1;
                            params.push(kind);
                        }
                        Rule::fx_ret => {
                            ret = Kind::from_pair(pair.into_inner().next().unwrap(), self)?;
                            map.insert("$ret", Value::memory(self.loc, ret.clone()));
                            self.loc += 1;
                        }
                        Rule::stmt => {
                            let id = self.fxs.len();
                            let start = self.loc;
                            self.insert(ident, Value::function(id as u16, 0, params, ret));
                            self.map.push(map);
                            self.push(pair);
                            self.map.pop();
                            let end = self.loc;
                            if let Some(Value {
                                value: BaseValue::Function(_, mem),
                                ..
                            }) = self.get_mut(ident)
                            {
                                *mem = end - start;
                            }
                            self.loc = loc;
                            self.fxs.push(std::mem::replace(&mut self.code, code));
                            return Ok(Value::none());
                        }
                        _ => unreachable!(),
                    }
                }

                unreachable!();
            }
            Rule::function_expr => {
                let mut iter = pair.into_inner();
                drop(iter.next());
                let code = std::mem::take(&mut self.code);
                let mut params = Vec::new();
                let mut ret = Kind::None;
                let loc = std::mem::take(&mut self.loc);

                let mut map = HashMap::new();
                while let Some(pair) = iter.next() {
                    match pair.as_rule() {
                        Rule::ident => {
                            let ident = pair.as_str();
                            let kind = Kind::from_pair(iter.next().unwrap(), self)?;
                            map.insert(ident, Value::memory(self.loc, kind.clone()));
                            self.loc += 1;
                            params.push(kind);
                        }
                        Rule::fx_ret => {
                            ret = Kind::from_pair(pair.into_inner().next().unwrap(), self)?;
                            map.insert("$ret", Value::memory(self.loc, ret.clone()));
                            self.loc += 1;
                        }
                        Rule::stmt => {
                            let id = self.fxs.len();
                            let start = self.loc;
                            self.map.push(map);
                            self.push(pair);
                            self.map.pop();
                            let end = self.loc;
                            self.loc = loc;
                            self.fxs.push(std::mem::replace(&mut self.code, code));
                            return Ok(Value::function(id as u16, end - start, params, ret));
                        }
                        _ => unreachable!("{pair}"),
                    }
                }

                unreachable!();
            }
            Rule::return_stmt => {
                let span = pair.as_span();
                let pair = pair.into_inner().nth(1).unwrap();
                let expr = self.analyze_pair(pair)?;
                match self.get("$ret") {
                    Some(Value {
                        kind,
                        value: BaseValue::Memory(m),
                        ..
                    }) => {
                        let expr_kind = expr.kind();
                        if kind != expr_kind {
                            error!(RS span => format!("cannot return a <{}> in a fx that returns a <{}>", expr_kind, kind));
                        }
                        self.code.push(Instruction::Copy(expr, *m));
                        Ok(Value::none())
                    }
                    Some(_) => unreachable!(),
                    None => error!(S span => format!("cannot return outside an fx")),
                }
            }
            Rule::call => {
                let span = pair.as_span();
                let mut iter = pair.into_inner();
                let fx = self.analyze_pair(iter.next().unwrap())?;
                let args = iter.collect::<Vec<_>>();
                let mut out = Value::none();
                match (fx.value, fx.kind) {
                    (BaseValue::Function(a, mem), Kind::Function(params, ret))
                        if a as usize != self.fxs.len() =>
                    {
                        let fx = self.fxs[a as usize].clone();
                        if args.len() != params.len() {
                            error!(RS span => format!("The fx takes {} arguments, while {} were passed", params.len(), args.len()));
                        }

                        let args = args.into_iter().zip(params).map(|(pair, kind)| {
                            let span = pair.as_span();
                            let expr = self.analyze_pair(pair)?;
                            let expr_kind = expr.kind();
                            if *expr_kind != kind {
                                error!(RS span => format!("The fx expected a <{}>, but <{}> was passed instead", kind, expr_kind));
                            }
                            Ok(expr)
                        }).collect::<Vec<_>>();

                        let start = self.loc;
                        let arg_mem = self.alloc(args.len() as Memory);
                        for (i, expr) in args.into_iter().enumerate() {
                            self.code
                                .push(Instruction::Copy(expr?, arg_mem + i as Memory));
                        }

                        if *ret != Kind::None {
                            out = Value::memory(self.memory(), *ret);
                        }

                        for code in fx {
                            self.code.push(code.with_offset(start));
                        }
                        self.loc += mem;
                        self.inc_mem(mem);
                    }

                    (BaseValue::Function(i, _), Kind::Function(params, ret))
                    | (BaseValue::Memory(i), Kind::Function(params, ret)) => {
                        if args.len() != params.len() {
                            error!(RS span => format!("The fx takes {} arguments, while {} were passed", params.len(), args.len()));
                        }
                        let args = args.into_iter().zip(params).map(|(pair, kind)| {
                            let span = pair.as_span();
                            let expr = self.analyze_pair(pair)?;
                            let expr_kind = expr.kind();
                            if *expr_kind != kind {
                                error!(RS span => format!("The fx expected a <{}>, but <{}> was passed instead", kind, expr_kind));
                            }
                            Ok(expr)
                        }).collect::<Vec<_>>();

                        let start = self.loc;
                        let mem = self.alloc(args.len() as Memory);
                        for (i, expr) in args.into_iter().enumerate() {
                            self.code.push(Instruction::Copy(expr?, mem + i as Memory));
                        }

                        if *ret != Kind::None {
                            out = Value::memory(self.memory(), *ret);
                        }
                        self.code.push(Instruction::Call(i, start));
                    }

                    (_, k) => error!(RS span => format!("Cannot call a <{}>", k)),
                }
                Ok(out)
            }
            _ => unreachable!("{pair}"),
        }
    }

    fn make_char(c: &[u8]) -> Option<u8> {
        match c {
            [b'\\', b'u', hex @ ..] => {
                let mut output = 0u8;
                for (i, &digit) in hex.iter().rev().enumerate() {
                    output = output.checked_add(
                        match digit {
                            b'0'..=b'9' => digit - b'0',
                            b'a'..=b'f' => digit - b'a' + 10,
                            b'A'..=b'F' => digit - b'A' + 10,
                            _ => unreachable!(),
                        }
                        .checked_mul(16u8.checked_pow(i as u32)?)?,
                    )?;
                }
                Some(output)
            }
            &[b'\\', oct1, oct2, oct3] => {
                (oct3 - b'0' + (oct2 - b'0') * 8).checked_add((oct1 - b'0').checked_mul(64)?)
            }
            [b'\\', x] => Some(match x {
                b'\\' => 0x5C,
                b'n' => 0x0A,
                b'a' => 0x07,
                b'b' => 0x08,
                b'e' => 0x1B,
                b'f' => 0x0C,
                b'r' => 0x0D,
                b'k' => 0x09,
                b'v' => 0x0B,
                b'\'' => 0x27,
                b'\"' => 0x22,
                _ => unreachable!(),
            }),
            &[x] => Some(x),
            _ => unreachable!(),
        }
    }
}
