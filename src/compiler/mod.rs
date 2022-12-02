use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};
use std::collections::HashMap;
use std::slice;

use crate::{
    analyzer::{
        value::{LeafData, NodeData},
        Analyzed, AnalyzedTree,
    },
    parser::syntax::SyntaxKind as SK,
    tree::{Leaf, LeafId, Node, NodeId, TreeElement},
};

pub struct JIT {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    data_ctx: DataContext,
    module: JITModule,
}

impl Default for JIT {
    fn default() -> Self {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names());
        let module = JITModule::new(builder.unwrap());
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
        }
    }
}

impl JIT {
    pub fn compile(&mut self, analyzed: Analyzed, source: &str) -> Result<*const u8, String> {
        self.translate(analyzed, source)?;

        let id = self
            .module
            .declare_function("_M__main__", Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;

        self.module
            .define_function(id, &mut self.ctx)
            .map_err(|e| e.to_string())?;

        self.module.clear_context(&mut self.ctx);
        self.module
            .finalize_definitions()
            .map_err(|e| e.to_string())?;

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

    fn translate(&mut self, analyzed: Analyzed, source: &str) -> Result<(), String> {
        let int = self.module.target_config().pointer_type();

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
            .collect::<Vec<_>>();

        let mut trans = FunctionTranslator {
            source,
            int,
            builder,
            lookup,
            variables,
            current_scope: 0,
            module: &mut self.module,
        };

        let root = tree.node(AnalyzedTree::ROOT);
        for &child in root.children() {
            trans.translate_node(&tree, tree.node(child));
        }

        trans.builder.ins().return_(&[]);
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
    ) -> Option<Value> {
        match element {
            TreeElement::Node(id) => self.translate_node(tree, tree.node(id)),
            TreeElement::Leaf(id) => self.translate_leaf(tree, tree.leaf(id)),
        }
    }

    fn translate_node(&mut self, tree: &AnalyzedTree, node: &Node<NodeData>) -> Option<Value> {
        match node.kind() {
            SK::Statement => {
                for &child in node.children() {
                    self.translate_node(tree, tree.node(child));
                }
                None
            }
            SK::Scope => {
                self.current_scope += 1;
                for &child in node.children() {
                    self.translate_node(tree, tree.node(child));
                }
                self.current_scope -= 1;
                None
            }
            SK::BinaryOp => {
                let mut iter = node.children_with_leaves(tree);
                let e_a = iter.next().unwrap();
                let op = iter.next().unwrap().into_leaf().unwrap().get(tree).kind();
                let e_b = iter.next().unwrap();
                let a = self.translate_element(tree, e_a).unwrap();
                let b = self.translate_element(tree, e_b).unwrap();
                Some(match op {
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
                })
            }
            SK::UnaryOp => {
                let mut iter = node.children_with_leaves(tree);
                let e_a = iter.next().unwrap();
                let op = iter.next().unwrap().into_leaf().unwrap().get(tree).kind();
                let a = self.translate_element(tree, e_a).unwrap();
                Some(match op {
                    SK::Not => self.builder.ins().bnot(a),
                    SK::Sub => self.builder.ins().ineg(a),
                    SK::Inc => self.builder.ins().iadd_imm(a, 1),
                    SK::Dec => self.builder.ins().iadd_imm(a, -1),
                    _ => unreachable!(),
                })
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
                let cond = self.translate_element(tree, a).unwrap();
                self.builder.ins().brz(cond, else_block, &[]);
                self.builder.ins().jump(then_block, &[]);
                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let then_return = self.translate_element(tree, b).unwrap();
                self.builder.ins().jump(merge_block, &[then_return]);
                self.builder.switch_to_block(else_block);
                self.builder.seal_block(else_block);
                let else_return = self.translate_element(tree, c).unwrap();
                self.builder.ins().jump(merge_block, &[else_return]);
                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);
                let phi = self.builder.block_params(merge_block)[0];
                Some(phi)
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
                let cond = self.translate_element(tree, a).unwrap();
                self.builder.ins().brz(cond, else_block, &[]);
                self.builder.ins().jump(then_block, &[]);
                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let then_return = self.translate_element(tree, b).unwrap();
                self.builder.ins().jump(merge_block, &[then_return]);
                self.builder.switch_to_block(else_block);
                self.builder.seal_block(else_block);
                let else_return = c
                    .and_then(|c| self.translate_element(tree, c))
                    .unwrap_or_else(|| self.builder.ins().iconst(self.int, 0));
                self.builder.ins().jump(merge_block, &[else_return]);
                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);
                let phi = self.builder.block_params(merge_block)[0];
                Some(phi)
            }
            SK::Let => {
                let mut iter = node.children_with_leaves(tree);
                let variable = self
                    .get(&self.source[iter.next().unwrap().get(tree).span()])
                    .unwrap();
                let new_value = self.translate_element(tree, iter.next().unwrap()).unwrap();
                self.builder.def_var(variable, new_value);
                None
            }
            _ => unreachable!(),
        }
    }

    fn translate_leaf(&mut self, tree: &AnalyzedTree, leaf: &Leaf<LeafData>) -> Option<Value> {
        match leaf.kind() {
            _ => unreachable!(),
        }
    }

    // fn translate_assign(&mut self, name: String, expr: Expr) -> Value {
    //     // `def_var` is used to write the value of a variable. Note that
    //     // variables can have multiple definitions. Cranelift will
    //     // convert them into SSA form for itself automatically.
    //     let new_value = self.translate_element(expr);
    //     let variable = self.variables.get(&name).unwrap();
    //     self.builder.def_var(*variable, new_value);
    //     new_value
    // }

    // fn translate_icmp(&mut self, cmp: IntCC, lhs: Expr, rhs: Expr) -> Value {
    //     let lhs = self.translate_element(lhs);
    //     let rhs = self.translate_element(rhs);
    //     let c = self.builder.ins().icmp(cmp, lhs, rhs);
    //     self.builder.ins().bint(self.int, c)
    // }

    // fn translate_while_loop(&mut self, condition: Expr, loop_body: Vec<Expr>) -> Value {
    //     let header_block = self.builder.create_block();
    //     let body_block = self.builder.create_block();
    //     let exit_block = self.builder.create_block();
    //     self.builder.ins().jump(header_block, &[]);
    //     self.builder.switch_to_block(header_block);
    //     let condition_value = self.translate_element(condition);
    //     self.builder.ins().brz(condition_value, exit_block, &[]);
    //     self.builder.ins().jump(body_block, &[]);
    //     self.builder.switch_to_block(body_block);
    //     self.builder.seal_block(body_block);
    //     for expr in loop_body {
    //         self.translate_element(expr);
    //     }
    //     self.builder.ins().jump(header_block, &[]);
    //     self.builder.switch_to_block(exit_block);
    //     // We've reached the bottom of the loop, so there will be no
    //     // more backedges to the header to exits to the bottom.
    //     self.builder.seal_block(header_block);
    //     self.builder.seal_block(exit_block);
    //     // Just return 0 for now.
    //     self.builder.ins().iconst(self.int, 0)
    // }

    // fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
    //     let mut sig = self.module.make_signature();
    //     // Add a parameter for each argument.
    //     for _arg in &args {
    //         sig.params.push(AbiParam::new(self.int));
    //     }
    //     // For simplicity for now, just make all calls return a single I64.
    //     sig.returns.push(AbiParam::new(self.int));
    //     // TODO: Streamline the API here?
    //     let callee = self
    //         .module
    //         .declare_function(&name, Linkage::Import, &sig)
    //         .expect("problem declaring function");
    //     let local_callee = self
    //         .module
    //         .declare_func_in_func(callee, &mut self.builder.func);
    //     let mut arg_values = Vec::new();
    //     for arg in args {
    //         arg_values.push(self.translate_element(arg))
    //     }
    //     let call = self.builder.ins().call(local_callee, &arg_values);
    //     self.builder.inst_results(call)[0]
    // }

    fn translate_global_data_addr(&mut self, name: String) -> Value {
        let sym = self
            .module
            .declare_data(&name, Linkage::Export, true, false)
            .expect("problem declaring data object");
        let local_id = self
            .module
            .declare_data_in_func(sym, &mut self.builder.func);

        let pointer = self.module.target_config().pointer_type();
        self.builder.ins().symbol_value(pointer, local_id)
    }
}
