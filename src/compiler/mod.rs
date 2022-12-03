use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module, ModuleError};
use std::collections::HashMap;
use std::slice;

use crate::{
    analyzer::{
        value::{LeafData, NodeData, ValueData},
        Analyzed, AnalyzedTree,
    },
    parser::syntax::SyntaxKind as SK,
    tree::{Leaf, LeafId, Node, NodeId, TreeElement},
};

pub struct JIT<'a> {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    data_ctx: DataContext,
    module: JITModule,
    source: &'a str,
}

impl<'a> JIT<'a> {
    pub fn new(source: &'a str) -> Self {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names());

        let module = JITModule::new(builder.unwrap());
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
            source,
        }
    }

    pub fn compile(&mut self, analyzed: Analyzed) -> Result<*const u8, ModuleError> {
        self.translate(analyzed)?;

        let id =
            self.module
                .declare_function("__main__", Linkage::Export, &self.ctx.func.signature)?;

        self.module.define_function(id, &mut self.ctx)?;

        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions()?;

        let code = self.module.get_finalized_function(id);

        Ok(code)
    }

    /// Create a zero-initialized data section.
    pub fn create_data(&mut self, name: &str, contents: Vec<u8>) -> Result<&[u8], String> {
        self.data_ctx.define(contents.into_boxed_slice());
        let id = self
            .module
            .declare_data(name, Linkage::Export, true, false)
            .map_err(|e| e.to_string())?;

        self.module
            .define_data(id, &self.data_ctx)
            .map_err(|e| e.to_string())?;
        self.data_ctx.clear();
        self.module
            .finalize_definitions()
            .map_err(|e| e.to_string())?;
        let buffer = self.module.get_finalized_data(id);
        Ok(unsafe { slice::from_raw_parts(buffer.0, buffer.1) })
    }

    fn translate(&mut self, analyzed: Analyzed) -> Result<(), ModuleError> {
        let int = self.module.target_config().pointer_type();
        self.ctx.func.signature.returns.push(AbiParam::new(int));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let Analyzed {
            tree,
            memory,
            lookup,
        } = analyzed;

        let variables = memory
            .into_iter()
            .enumerate()
            .map(|(i, _)| Variable::new(i))
            .map(|v| {
                builder.declare_var(v, int);
                v
            })
            .collect::<Vec<_>>();

        let mut return_var = builder.ins().iconst(int, 0);

        let mut trans = FunctionTranslator {
            source: self.source,
            int,
            builder,
            lookup,
            variables,
            current_scope: 0,
            module: &mut self.module,
        };

        let root = tree.node(AnalyzedTree::ROOT);
        for &child in root.children() {
            return_var = trans.translate_node(&tree, tree.node(child));
        }

        trans.builder.ins().return_(&[return_var]);
        trans.builder.finalize();
        Ok(())
    }
}

/// A collection of state used for translating from toy-language AST nodes
/// into Cranelift IR.
struct FunctionTranslator<'a> {
    int: types::Type,
    source: &'a str,
    builder: FunctionBuilder<'a>,
    lookup: Vec<HashMap<&'a str, usize>>,
    variables: Vec<Variable>,
    current_scope: usize,
    module: &'a mut JITModule,
}

impl<'a> FunctionTranslator<'a> {
    #[inline]
    fn get(&self, ident: &'a str) -> Option<Variable> {
        self.lookup[..=self.current_scope]
            .iter()
            .rev()
            .find_map(|map| map.get(ident))
            .map(|&i| self.variables[i])
    }

    fn translate_element(
        &mut self,
        tree: &AnalyzedTree,
        element: TreeElement<NodeId, LeafId>,
    ) -> Value {
        match element {
            TreeElement::Node(id) => self.translate_node(tree, tree.node(id)),
            TreeElement::Leaf(id) => self.translate_leaf(tree, tree.leaf(id)),
        }
    }

    fn translate_node(&mut self, tree: &AnalyzedTree, node: &Node<NodeData>) -> Value {
        match node.kind() {
            SK::Statement => {
                let mut ret = self.builder.ins().iconst(self.int, 0);
                for &child in node.children() {
                    ret = self.translate_node(tree, tree.node(child));
                }
                ret
            }
            SK::Scope => {
                self.current_scope += 1;
                let mut ret = self.builder.ins().iconst(self.int, 0);
                for &child in node.children() {
                    ret = self.translate_node(tree, tree.node(child));
                }
                self.current_scope -= 1;
                ret
            }
            SK::BinaryOp => {
                let mut iter = node.children_with_leaves(tree);
                let e_a = iter.next().unwrap();
                let op = iter.next().unwrap().into_leaf().unwrap().get(tree).kind();
                let e_b = iter.next().unwrap();
                let a = self.translate_element(tree, e_a);
                let b = self.translate_element(tree, e_b);
                match op {
                    SK::Add => self.builder.ins().iadd(a, b),
                    SK::Sub => self.builder.ins().isub(a, b),
                    SK::Mul => self.builder.ins().imul(a, b),
                    SK::Div => self.builder.ins().sdiv(a, b),
                    SK::Mod => self.builder.ins().srem(a, b),
                    SK::And => self.builder.ins().band(a, b),
                    SK::Or => self.builder.ins().bor(a, b),
                    SK::Xor => self.builder.ins().bxor(a, b),
                    SK::Shl => self.builder.ins().ishl(a, b),
                    SK::Shr => self.builder.ins().sshr(a, b),
                    SK::Equal => self.builder.ins().icmp(IntCC::Equal, a, b),
                    SK::NotEqual => self.builder.ins().icmp(IntCC::NotEqual, a, b),
                    SK::GreaterThan => self.builder.ins().icmp(IntCC::SignedGreaterThan, a, b),
                    SK::GreaterEqual => {
                        self.builder
                            .ins()
                            .icmp(IntCC::SignedGreaterThanOrEqual, a, b)
                    }
                    SK::LessThan => self.builder.ins().icmp(IntCC::SignedLessThan, a, b),
                    SK::LessEqual => self.builder.ins().icmp(IntCC::SignedLessThanOrEqual, a, b),
                    _ => unreachable!(),
                }
            }
            SK::UnaryOp => {
                let mut iter = node.children_with_leaves(tree);
                let e_a = iter.next().unwrap();
                let op = iter.next().unwrap().into_leaf().unwrap().get(tree).kind();
                let a = self.translate_element(tree, e_a);
                match op {
                    SK::Not => self.builder.ins().bnot(a),
                    SK::Sub => self.builder.ins().ineg(a),
                    SK::Inc => self.builder.ins().iadd_imm(a, 1),
                    SK::Dec => self.builder.ins().iadd_imm(a, -1),
                    _ => unreachable!(),
                }
            }
            SK::Value => {
                self.translate_element(tree, node.children_with_leaves(tree).next().unwrap())
            }
            SK::Ternary => {
                let mut iter = node.children_with_leaves(tree);
                let a = iter.next().unwrap();
                let b = iter.next().unwrap();
                let c = iter.next().unwrap();
                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let merge_block = self.builder.create_block();

                self.builder.append_block_param(merge_block, self.int);
                let cond = self.translate_element(tree, a);
                self.builder.ins().brz(cond, else_block, &[]);
                self.builder.ins().jump(then_block, &[]);
                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let then_return = self.translate_element(tree, b);
                self.builder.ins().jump(merge_block, &[then_return]);
                self.builder.switch_to_block(else_block);
                self.builder.seal_block(else_block);
                let else_return = self.translate_element(tree, c);
                self.builder.ins().jump(merge_block, &[else_return]);
                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);
                self.builder.block_params(merge_block)[0]
            }
            SK::If => {
                let mut iter = node.children_with_leaves(tree);
                let a = iter.next().unwrap();
                let b = iter.next().unwrap();
                let c = iter.next();
                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let merge_block = self.builder.create_block();

                self.builder.append_block_param(merge_block, self.int);
                let cond = self.translate_element(tree, a);
                self.builder.ins().brz(cond, else_block, &[]);
                self.builder.ins().jump(then_block, &[]);
                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let then_return = self.translate_element(tree, b);
                self.builder.ins().jump(merge_block, &[then_return]);
                self.builder.switch_to_block(else_block);
                self.builder.seal_block(else_block);
                let else_return = c
                    .map(|c| self.translate_element(tree, c))
                    .unwrap_or_else(|| self.builder.ins().iconst(self.int, 0));
                self.builder.ins().jump(merge_block, &[else_return]);
                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);
                self.builder.block_params(merge_block)[0]
            }
            SK::Let => {
                let mut iter = node.children_with_leaves(tree);
                let variable = self
                    .get(&self.source[iter.next().unwrap().get(tree).span()])
                    .unwrap();
                let new_value = self.translate_element(tree, iter.next().unwrap());
                self.builder.def_var(variable, new_value);
                self.builder.ins().iconst(self.int, 0)
            }
            SK::Loop => {
                let mut iter = node.children_with_leaves(tree);
                let a = iter.next().unwrap();
                let b = iter.next().unwrap();
                let c = iter.next().unwrap();
                let d = iter.next().unwrap();

                let header_block = self.builder.create_block();
                let body_block = self.builder.create_block();
                let exit_block = self.builder.create_block();

                self.builder.ins().jump(header_block, &[]);
                self.builder.switch_to_block(header_block);
                self.translate_element(tree, a);
                let condition_value = self.translate_element(tree, b);
                self.builder.ins().brz(condition_value, exit_block, &[]);
                self.builder.ins().jump(body_block, &[]);
                self.builder.switch_to_block(body_block);
                self.builder.seal_block(body_block);
                self.translate_element(tree, d);
                self.translate_element(tree, c);
                self.builder.ins().jump(header_block, &[]);
                self.builder.switch_to_block(exit_block);
                self.builder.seal_block(header_block);
                self.builder.seal_block(exit_block);
                self.builder.ins().iconst(self.int, 0)
            }
            SK::OutKw => {
                let a =
                    self.translate_element(tree, node.children_with_leaves(tree).next().unwrap());
                let mut sig = self.module.make_signature();
                sig.params.push(AbiParam::new(self.int));
                sig.returns.push(AbiParam::new(self.int));

                let callee = self
                    .module
                    .declare_function("puts", cranelift_module::Linkage::Import, &sig)
                    .unwrap(); // For now

                let local_callee = self
                    .module
                    .declare_func_in_func(callee, &mut self.builder.func);

                let call = self.builder.ins().call(local_callee, &[a]);
                self.builder.inst_results(call)[0]
            }
            SK::Stuffing => self.builder.ins().iconst(self.int, 1),
            SK::Cast | SK::Kind => self.builder.ins().iconst(self.int, 0),
            s => unreachable!("{s}"),
        }
    }

    fn translate_leaf(&mut self, _tree: &AnalyzedTree, leaf: &Leaf<LeafData>) -> Value {
        match leaf.kind() {
            SK::Identifier => {
                let var = self.get(&self.source[leaf.span()]).unwrap();
                self.builder.use_var(var)
            }
            SK::Number | SK::Char => self.builder.ins().iconst(
                self.int,
                match leaf
                    .data()
                    .as_ref()
                    .unwrap()
                    .into_value()
                    .value
                    .as_ref()
                    .unwrap()
                {
                    ValueData::Number(n) => *n as i64,
                    ValueData::Char(c) => *c as i64,
                    _ => unreachable!(),
                },
            ),
            SK::SemiColon => self.builder.ins().iconst(self.int, 0),
            s => unreachable!("{s}"),
        }
    }
}
