use super::{
    parser,
    utils::{Error, Node},
};

type AnalyzeResult = Result<Node, Error>;

pub struct Analyzer;

impl Analyzer {
    pub fn analyze(ast: Node, parser: parser::Parser) -> AnalyzeResult {
        unimplemented!()
    }
}
