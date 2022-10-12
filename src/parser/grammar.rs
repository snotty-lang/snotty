use super::{token::Token, Span};

use patricia_tree::PatriciaMap;

pub type SpannedItem = (Item, Span);
pub type SpannedRule = (Rule, Span);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Rule {
    Value,
    Expr,
    Pointer,
    TypeCast,
    Kind,
    CSA,
    CSK,
    Ternary,
    UnOpExpr,
    BinOpExpr,
    Call,

    ParseError,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    Token(Token),
    Rule(Rule),
}

impl Item {
    pub fn into_rule(self) -> Option<Rule> {
        match self {
            Item::Rule(n) => Some(n),
            _ => None,
        }
    }

    pub fn as_u8(&self) -> u8 {
        match self {
            Item::Rule(rule) => *rule as u8 | 1 << 7,
            Item::Token(token) => *token as u8,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Grammar {
    grammar: PatriciaMap<Rule>,
}

impl Grammar {
    pub fn new() -> Self {
        Self {
            grammar: PatriciaMap::new(),
        }
    }

    pub fn push<const T: usize>(mut self, rule: [Item; T], result: Rule) -> Self {
        assert!(T > 0, "Cannot have a 0 length rule");
        self.grammar.insert(rule.map(|s| Item::as_u8(&s)), result);
        self
    }

    pub fn get(&self, rule: &[Item]) -> Option<Rule> {
        self.grammar
            .get::<&[u8]>(&rule.iter().map(Item::as_u8).collect::<Vec<_>>())
            .cloned()
    }

    pub fn reduce(&self, buffer: &mut Vec<SpannedItem>) -> Option<SpannedRule> {
        let new_buffer = buffer.iter().map(|(s, _)| s.as_u8()).collect::<Vec<_>>();

        for i in 0..buffer.len() {
            if let Some(rule) = self.grammar.get(&new_buffer[i..]).cloned() {
                println!("{rule:?} :: {i} :: {:?}", &buffer[i..]);
                return match buffer.len() - i {
                    0 => unreachable!(),
                    1 => Some((rule, buffer.pop().unwrap().1)),
                    2 => {
                        let end = buffer.pop().unwrap().1.end;
                        let start = buffer.pop().unwrap().1.start;
                        Some((rule, start..end))
                    }
                    _ => {
                        let end = buffer.pop().unwrap().1.end;
                        buffer.truncate(i + 1);
                        let start = buffer.pop().unwrap().1.start;
                        Some((rule, start..end))
                    }
                };
            }
        }

        None
    }
}

#[macro_export]
macro_rules! grammar {
    ($($($rule: expr),* => $result: expr,)*) => {{
        use $crate::parser::{grammar::{Item::*, Rule::*}, token::Token::*};
        $crate::parser::grammar::Grammar::new()$(.push([$($rule,)*], $result))*
    }};
}
