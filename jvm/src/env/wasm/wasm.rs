use crate::bytecode::Bytecode;
use crate::classfile::resolved::attribute::Code;
use crate::classfile::resolved::{
    attribute, AccessFlags, Attribute, Class, Constant, Method, Ref, ReturnType,
};
use crate::env::wasm::cfg::{
    create_scopes, find_loops, identify_scopes, label_nodes, Block, LabeledNode, ScopeMetrics,
};
use crate::linker::ClassLoaderObject;
use crate::JVM;
use bitflags::Flags;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::task::Context;
use wasm_encoder::{
    BlockType, CodeSection, Function, FunctionSection, Instruction, Module, TableSection,
    TypeSection, ValType,
};
use crate::classfile::resolved;

pub const POINTER_SIZE: i32 = 4;
pub const POINTER_TYPE: ValType = ValType::I32;

pub struct CompiledClass {
    pub module: Module,
    pub link: HashMap<Arc<Ref>, u32>,
}

fn get_method_params(method: &Method) -> (Vec<ValType>, Vec<ValType>) {
    let mut params: Vec<ValType> = (0..method.descriptor.args.len())
        .map(|_| ValType::I32)
        .collect();

    if !method.access_flags.contains(AccessFlags::STATIC) {
        params.push(ValType::I32);
    }

    let result = if matches!(method.descriptor.return_type, ReturnType::Void) {
        vec![]
    } else {
        vec![ValType::I32]
    };

    (params, result)
}

pub fn compile_method(
    method: &Method,
    class: &Class,
    function_section: &mut FunctionSection,
    code_section: &mut CodeSection,
    type_section: &mut TypeSection,
    method_func_idx_map: &HashMap<&str, usize>,
    jvm: &JVM,
    external_references: &HashMap<Arc<Ref>, u32>,
) {
    let (params, results) = get_method_params(method);

    let (code, locals): (&Code, Vec<_>) =
        if let Some(Attribute::Code(code)) = method.attributes.get("Code") {
            (
                code,
                (0..code.max_locals - (params.len() as u16))
                    .map(|i| (i as u32, ValType::I32))
                    .collect::<Vec<_>>(),
            )
        } else {
            unreachable!()
        };

    let byte_index_to_instruction_index: HashMap<u32, u32> = code
        .instructions
        .iter()
        .map(|instruction| (instruction.bytes_index, instruction.bytecode_index))
        .collect();

    let (loops, nodes, scc_s) = find_loops(&code.instructions);

    let labeled = label_nodes(&loops, &nodes, &scc_s);

    let zipped: Vec<_> = nodes.iter().zip(labeled.iter()).collect();

    let metrics = identify_scopes(&zipped, &scc_s);

    let mut block_ranges = &metrics.blocks[..];
    let mut loop_ranges = &metrics.loops[..];

    let loop_out = loop_ranges.get(0) == Some(&(0..labeled.len()));

    let block = create_scopes(
        &mut block_ranges,
        &mut loop_ranges,
        &zipped,
        0..labeled.len(),
        loop_out,
    );

    let mut block_index = 0;

    let mut wasm_instructions = vec![];
    
    let mut ctx = BytecodeContext {
        locals_start: locals.len() as u32,
        locals,
        byte_to_idx_map: &byte_index_to_instruction_index,
        wasm_instrs: &mut wasm_instructions,
        scope_metrics: &metrics,
    };

    translate_block(
        &mut ctx,

        &block,
        &code.instructions,
        &labeled,
        &scc_s,
        type_section,
        true,
        method_func_idx_map,
        class,
        jvm,
        external_references,
        &mut block_index,
    );

    todo!()

    // wasm_instructions.push(Instruction::End);
    //
    // function_section.function(type_section.len());
    //
    // type_section.function(params, results);
    // code_section.function(&function);
}

fn get_jump_offset(
    scope_metrics: &ScopeMetrics,
    start: usize,
    target: usize,
    _block_index: u32,
) -> u32 {
    let begin = start.min(target);
    let end = start.max(target);

    let jump_range = begin..=end;

    if start < target {
        let starts: i32 = scope_metrics
            .blocks
            .iter()
            .chain(scope_metrics.loops.iter())
            .filter(|range| range.start > begin && jump_range.contains(&range.end))
            .map(|_| 1)
            .sum();
        let ends: i32 = scope_metrics
            .blocks
            .iter()
            .chain(scope_metrics.loops.iter())
            .filter(|range| jump_range.contains(&range.end))
            .map(|_| 1)
            .sum();

        let scope = (ends - starts) - 1;
        assert!(scope >= 0, "{:?}", scope_metrics);
        scope as u32
    } else {
        let the_loop = scope_metrics
            .loops
            .iter()
            .find(|range| range.contains(&begin) && range.end > end)
            .unwrap();

        let starts: u32 = scope_metrics
            .blocks
            .iter()
            .filter(|range| {
                range.start >= begin
                    && range.start <= end
                    && !(range.start == the_loop.start && range.end == the_loop.end)
            })
            .map(|_| 1)
            .sum();
        let ends: u32 = scope_metrics
            .blocks
            .iter()
            .filter(|range| range.end >= begin && range.end <= end)
            .map(|_| 1)
            .sum();

        starts - ends
    }
}

struct BuiltinRoutines {
    invoke_special_routine: u32,
}

struct PrimitiveTypes {
    object_array: u32,
}

struct Globals {
    jvm_ptr: u32,
}

struct CompilerState {
    routines: BuiltinRoutines,
    primitives: PrimitiveTypes,
    globals: Globals,

    descriptor_to_wasm_type_id: HashMap<String, u32>,

    hot_function_table: u32,
}

struct BytecodeContext<'a> {
    locals: Vec<(u32, ValType)>,
    locals_start: u32,
    byte_to_idx_map: &'a HashMap<u32, u32>,
    wasm_instrs: &'a mut Vec<Instruction<'a>>,
    scope_metrics: &'a ScopeMetrics,
}

impl<'a> BytecodeContext<'a> {

    fn get_helpers<const N: usize>(&mut self, types: [ValType; N]) -> [u32; N] {
        let mut offset = 0;

        types.map(|type_| {
            match self.locals.iter().filter(|(_, valtype)| *valtype == type_).skip(offset).next() {
                None => {
                    let index = self.locals_start + self.locals.len() as u32;

                    self.locals.push(
                        (index, type_)
                    );

                    index
                }
                Some((index, _)) => {
                    offset += 1;

                    *index
                }
            }
        })
    }

}

fn translate_instruction(
    ctx: &mut BytecodeContext,
    instruction: &attribute::Instruction,
    class: &Class,
    scope_offset: u32,
    block_idx: u32,
) {

    match &instruction.bytecode {
        Bytecode::Invokespecial(index) => {
            let method_constant = class.constant_pool.constants.get(index).unwrap();
            let method_ref = method_constant.as_ref().unwrap();



            todo!()
        }
        Bytecode::Pop => {
            ctx.wasm_instrs.push(Instruction::Drop);
        }
        Bytecode::Iload(index)
        | Bytecode::Iload_n(index)
        | Bytecode::Aload(index)
        | Bytecode::Aload_n(index) => {
            ctx.wasm_instrs.push(Instruction::LocalGet(*index as u32));
        }
        Bytecode::Iconst_n_m1(val) => {
            ctx.wasm_instrs.push(Instruction::I32Const(*val as i32));
        }
        Bytecode::Ifnull(offset) => {
            let target = (instruction.bytes_index as isize + *offset as isize) as usize;
            let target_idx = *ctx.byte_to_idx_map.get(&(target as u32)).unwrap();

            if target_idx == instruction.bytecode_index + 1 {
                return;
            }

            let jump = get_jump_offset(&ctx.scope_metrics, instruction.bytes_index as usize, target_idx as usize, block_idx);

            ctx.wasm_instrs.push(Instruction::BrOnNull(jump + scope_offset));
        }
        Bytecode::Goto(offset) => {
            let target = (instruction.bytes_index as isize + *offset as isize) as usize;
            let target_idx = *ctx.byte_to_idx_map.get(&(target as u32)).unwrap();

            if target_idx == instruction.bytecode_index + 1 {
                return;
            }

            let jump = get_jump_offset(&ctx.scope_metrics, instruction.bytes_index as usize, target_idx as usize, block_idx);

            ctx.wasm_instrs.push(Instruction::Br(jump + scope_offset));
        }
        Bytecode::If_icmpne(offset) => {
            let target = (instruction.bytes_index as isize + *offset as isize) as usize;
            let target_idx = *ctx.byte_to_idx_map.get(&(target as u32)).unwrap();

            if target_idx == instruction.bytecode_index + 1 {
                return;
            }

            let jump = get_jump_offset(&ctx.scope_metrics, instruction.bytes_index as usize, target_idx as usize, block_idx);

            ctx.wasm_instrs.push(Instruction::I32Ne);
            ctx.wasm_instrs.push(Instruction::BrIf(jump + scope_offset));
        }
        Bytecode::If_icmple(offset) => {
            let target = (instruction.bytes_index as isize + *offset as isize) as usize;
            let target_idx = *ctx.byte_to_idx_map.get(&(target as u32)).unwrap();

            if target_idx == instruction.bytecode_index + 1 {
                return;
            }

            let jump = get_jump_offset(&ctx.scope_metrics, instruction.bytes_index as usize, target_idx as usize, block_idx);

            ctx.wasm_instrs.push(Instruction::I32LeS);
            ctx.wasm_instrs.push(Instruction::BrIf(jump + scope_offset));
        }
        Bytecode::If_icmpgt(offset) => {
            let target = (instruction.bytes_index as isize + *offset as isize) as usize;
            let target_idx = *ctx.byte_to_idx_map.get(&(target as u32)).unwrap();

            if target_idx == instruction.bytecode_index + 1 {
                return;
            }

            let jump = get_jump_offset(&ctx.scope_metrics, instruction.bytes_index as usize, target_idx as usize, block_idx);

            ctx.wasm_instrs.push(Instruction::I32GtS);
            ctx.wasm_instrs.push(Instruction::BrIf(jump + scope_offset));
        }
        Bytecode::If_icmpge(offset) => {
            let target = (instruction.bytes_index as isize + *offset as isize) as usize;
            let target_idx = *ctx.byte_to_idx_map.get(&(target as u32)).unwrap();

            if target_idx == instruction.bytecode_index + 1 {
                return;
            }

            let jump = get_jump_offset(&ctx.scope_metrics, instruction.bytes_index as usize, target_idx as usize, block_idx);

            ctx.wasm_instrs.push(Instruction::I32GeS);
            ctx.wasm_instrs.push(Instruction::BrIf(jump + scope_offset));
        }
        Bytecode::Ifeq(offset) => {
            let target = (instruction.bytes_index as isize + *offset as isize) as usize;
            let target_idx = *ctx.byte_to_idx_map.get(&(target as u32)).unwrap();

            if target_idx == instruction.bytecode_index + 1 {
                return;
            }

            let jump = get_jump_offset(&ctx.scope_metrics, instruction.bytes_index as usize, target_idx as usize, block_idx);

            ctx.wasm_instrs.push(Instruction::I32Eqz);
            ctx.wasm_instrs.push(Instruction::BrIf(jump + scope_offset));
        }
        Bytecode::Ifne(offset) => {
            let target = (instruction.bytes_index as isize + *offset as isize) as usize;
            let target_idx = *ctx.byte_to_idx_map.get(&(target as u32)).unwrap();

            if target_idx == instruction.bytecode_index + 1 {
                return;
            }

            let jump = get_jump_offset(&ctx.scope_metrics, instruction.bytes_index as usize, target_idx as usize, block_idx);

            ctx.wasm_instrs.push(Instruction::I32Const(0));
            ctx.wasm_instrs.push(Instruction::I32Ne);
            ctx.wasm_instrs.push(Instruction::BrIf(jump + scope_offset));
        }
        Bytecode::Irem => {
            ctx.wasm_instrs.push(Instruction::I32RemS);
        }
        Bytecode::Ifge(offset) => {
            let target = (instruction.bytes_index as isize + *offset as isize) as usize;
            let target_idx = *ctx.byte_to_idx_map.get(&(target as u32)).unwrap();

            if target_idx == instruction.bytecode_index + 1 {
                return;
            }

            let jump = get_jump_offset(&ctx.scope_metrics, instruction.bytes_index as usize, target_idx as usize, block_idx);

            ctx.wasm_instrs.push(Instruction::I32Const(0));
            ctx.wasm_instrs.push(Instruction::I32GeS);
            ctx.wasm_instrs.push(Instruction::BrIf(jump + scope_offset));
        }
        Bytecode::Ineg => {
            ctx.wasm_instrs.push(Instruction::I32Const(i32::MAX));
            ctx.wasm_instrs.push(Instruction::I32Xor);
            ctx.wasm_instrs.push(Instruction::I32Const(1));
            ctx.wasm_instrs.push(Instruction::I32Add);
        }
        Bytecode::Iadd => {
            ctx.wasm_instrs.push(Instruction::I32Add);
        }
        Bytecode::Isub => {
            ctx.wasm_instrs.push(Instruction::I32Sub);
        }
        Bytecode::Putstatic(_constant_pool) => {
            todo!()
        }
        Bytecode::Sipush(value) => {
            ctx.wasm_instrs.push(Instruction::I32Const(*value as i32));
        }
        Bytecode::Bipush(value) => {
            ctx.wasm_instrs.push(Instruction::I32Const(*value as i32));
        }
        Bytecode::Istore(index) | Bytecode::Istore_n(index) => {
            ctx.wasm_instrs.push(Instruction::LocalSet(*index as u32));
        }
        Bytecode::Iinc(index, inc) => {
            ctx.wasm_instrs
                .extend([
                    Instruction::LocalGet(*index as u32),
                    Instruction::I32Const(*inc as i32),
                    Instruction::I32Add,
                    Instruction::LocalSet(*index as u32)
                ]);
        }
        Bytecode::Imul => {
            ctx.wasm_instrs.push(Instruction::I32Mul);
        }
        Bytecode::Invokevirtual(index) => {
            let constant = class.constant_pool.constants.get(index).unwrap();
            let method_ref = constant.as_ref().unwrap();

            todo!()
        }
        Bytecode::Invokestatic(constant_pool) => {
            let constant = class.constant_pool.constants.get(constant_pool).unwrap();
            match constant {
                Constant::MethodRef(method) => {
                    todo!()
                }
                Constant::InterfaceMethodRef(_) => todo!(),
                _ => unreachable!(),
            }
        }
        Bytecode::Invokespecial(constant_pool) => {
            todo!()
        }
        Bytecode::Return
        | Bytecode::Ireturn
        | Bytecode::Areturn
        | Bytecode::Dreturn
        | Bytecode::Freturn
        | Bytecode::Lreturn => {
            ctx.wasm_instrs.push(Instruction::Return);
        }
        Bytecode::New(index) => {
            // function.instruction(Instruction::I31);
        }
        bytecode => unimplemented!("Bytecode not implemented {:?} @ {}", bytecode, instruction.bytecode_index),
    }
}

fn get_block_type_signature(
    block: &Block,
    instructions: &[attribute::Instruction],
) -> Vec<ValType> {
    match block {
        Block::Loop(_) => vec![],
        Block::Normal(blocks) => get_block_type_signature(&blocks[0], instructions),
        Block::Nodes(nodes) => {
            let component = match nodes.get(0) {
                None => return vec![],
                Some((component, _)) => component,
            };

            match &instructions[component.idx].bytecode {
                Bytecode::If_acmpeq(_) | Bytecode::If_acmpne(_) => vec![POINTER_TYPE, POINTER_TYPE],
                Bytecode::If_icmpeq(_)
                | Bytecode::If_icmpne(_)
                | Bytecode::If_icmplt(_)
                | Bytecode::If_icmpge(_)
                | Bytecode::If_icmpgt(_)
                | Bytecode::If_icmple(_) => vec![ValType::I32, ValType::I32],
                Bytecode::Ifeq(_)
                | Bytecode::Ifne(_)
                | Bytecode::Iflt(_)
                | Bytecode::Ifge(_)
                | Bytecode::Ifgt(_)
                | Bytecode::Ifle(_) => vec![ValType::I32],
                Bytecode::Ifnonnull(_) | Bytecode::Ifnull(_) => vec![POINTER_TYPE],
                _ => vec![],
            }
        }
    }
}

fn translate_block(
    ctx: &mut BytecodeContext,

    block: &Block,

    instructions: &[attribute::Instruction],
    labeled: &[LabeledNode],
    scc_s: &HashMap<usize, Vec<usize>>,
    type_section: &mut TypeSection,
    skip: bool,
    method_func_idx_map: &HashMap<&str, usize>,
    class: &Class,
    jvm: &JVM,
    external_references: &HashMap<Arc<Ref>, u32>,
    block_index: &mut u32,
) {
    match block {
        Block::Loop(blocks) => {
            ctx.wasm_instrs.push(Instruction::Loop(BlockType::Empty));

            for block in blocks {
                translate_block(
                    ctx,
                    block,
                    instructions,
                    labeled,
                    scc_s,
                    type_section,
                    false,
                    method_func_idx_map,
                    class,
                    jvm,
                    external_references,
                    block_index,
                );
            }

            ctx.wasm_instrs.push(Instruction::End);

            *block_index += 1;
        }
        Block::Normal(blocks) => {
            let types = get_block_type_signature(block, instructions);

            let type_index = type_section.len();

            if !skip {
                if types.len() > 0 {
                    ctx.wasm_instrs.push(Instruction::Block(BlockType::FunctionType(type_index)));
                } else {
                    ctx.wasm_instrs.push(Instruction::Block(BlockType::Empty));
                }

                *block_index += 1;
            }

            type_section.function(types, []);

            for block in blocks {
                translate_block(
                    ctx,
                    block,
                    instructions,
                    labeled,
                    scc_s,
                    type_section,
                    false,
                    method_func_idx_map,
                    class,
                    jvm,
                    external_references,
                    block_index,
                );
            }

            if !skip {
                ctx.wasm_instrs.push(Instruction::End);
            }
        }
        Block::Nodes(nodes) => {
            for (component_node, labeled_node) in nodes {
                let idx = component_node.idx;
                let bytecode = &instructions[idx].bytecode;
                let byte_index = instructions[idx].bytes_index;

                match labeled_node {
                    LabeledNode::LoopContinue { node: component } => {
                        let bytecode = &instructions[component.idx].bytecode;

                        let header_idx = scc_s[&component.scc][0];

                        let branch_scope =
                            get_jump_offset(ctx.scope_metrics, component.idx, header_idx, *block_index);

                        match bytecode {
                            Bytecode::Goto(_) => {
                                ctx.wasm_instrs.push(Instruction::Br(branch_scope));
                            }
                            Bytecode::If_icmpne(_) => {
                                ctx.wasm_instrs.push(Instruction::I32Ne);
                                ctx.wasm_instrs.push(Instruction::BrIf(branch_scope));
                            }
                            Bytecode::If_icmpeq(_) => {
                                ctx.wasm_instrs.push(Instruction::I32Eq);
                                ctx.wasm_instrs.push(Instruction::BrIf(branch_scope));
                            }
                            Bytecode::If_icmple(_) => {
                                ctx.wasm_instrs.push(Instruction::I32LeS);
                                ctx.wasm_instrs.push(Instruction::BrIf(branch_scope));
                            }
                            x => unimplemented!("Special control scheme {:?} unimplemented", x),
                        }

                        continue;
                    }
                    LabeledNode::LabeledControlFlow {
                        node,
                        label: _,
                        target,
                    } => {
                        let _bytecode = &instructions[node.idx].bytecode;

                        let header_idx = scc_s[&node.scc][0];
                        let header = match &labeled[header_idx] {
                            LabeledNode::LoopController { targets, .. } => targets,
                            _ => unreachable!(),
                        };

                        let branch_scope =
                            get_jump_offset(ctx.scope_metrics, node.idx, header_idx, *block_index);

                        ctx.wasm_instrs.push(Instruction::I32Const(
                            header.iter().position(|i| i == target).unwrap() as i32 + 1,
                        ));
                        todo!();
                        // ctx.wasm_instrs.push(Instruction::LocalSet(ctx.get));
                        // ctx.wasm_instrs.push(Instruction::Br(branch_scope));

                        continue;
                    }
                    LabeledNode::LoopController { node, targets } => {
                        let [helper_1, helper_2] = ctx.get_helpers([ValType::I32; 2]);

                        ctx.wasm_instrs.push(Instruction::Block(BlockType::Empty));
                        ctx.wasm_instrs.push(Instruction::LocalGet(helper_1));
                        ctx.wasm_instrs.push(Instruction::LocalTee(helper_2));
                        ctx.wasm_instrs.push(Instruction::I32Const(0));
                        ctx.wasm_instrs.push(Instruction::LocalSet(helper_1));

                        let mut targets: Vec<u32> = targets
                            .iter()
                            .map(|target| {
                                get_jump_offset(ctx.scope_metrics, node.idx, *target, *block_index)
                                    + 1 as u32
                            })
                            .collect();
                        targets.insert(0, 0);

                        ctx.wasm_instrs.push(Instruction::BrTable(targets.into(), 0));
                        ctx.wasm_instrs.push(Instruction::End);
                    }

                    _ => {}
                };

                // translate_instruction(
                //     ctx,
                //     ,
                //     idx,
                //     byte_index,
                //     class,
                //     0,
                //     *block_index
                // );

                todo!()
            }
        }
    }
}
