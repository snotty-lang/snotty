pub mod analyzed_builder;
pub mod type_checker;
pub mod value;

use std::collections::HashMap;

use crate::{
    error::Error,
    tree::{Tree, TreeBuilder},
};

use value::{LeafData, NodeData, Value};

pub type AnalyzedTree = Tree<NodeData, LeafData>;
pub type AnalyzedTreeBuilder = TreeBuilder<NodeData, LeafData>;

pub struct AnalysisResult<'a> {
    pub errors: Vec<Error<'a>>,
    pub analyzed: Analyzed<'a>,
}

pub struct Analyzed<'a> {
    pub tree: AnalyzedTree,
    pub memory: Vec<Value>,
    pub lookup: Vec<HashMap<&'a str, usize>>,
}
