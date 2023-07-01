use std::collections::HashMap;
use wasm_encoder::{BlockType, CodeSection, Function, Instruction, MemArg, Module, TypeSection, ValType};
use wasm_encoder::Instruction::Br;
use crate::bytecode::Bytecode;
use crate::classfile::resolved::{Attribute, attribute, Method};
use crate::classfile::resolved::attribute::Code;
use crate::jit::{Block, create_scopes, find_loops, identify_scopes, label_nodes, LabeledNode, ScopeMetrics};

pub const POINTER_SIZE: i32 = 4;

pub fn new_method(method: &Method, module: &mut Module) {
    let mut code_section = CodeSection::new();

    let (code, mut locals): (&Code, Vec<_>) = if let Some(Attribute::Code(code)) = method.attributes.get("Code") {
        (code, (0..code.max_locals).map(|i| (i as u32, ValType::I32)).collect::<Vec<_>>())
    } else {
        unreachable!()
    };

    let helper_start = locals.len() as u32;
    locals.push(((locals.len() as u32), (ValType::I32)));

    let byte_index_to_instruction_index: HashMap<u32, u32> = code.instructions
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

    let block = create_scopes(&mut block_ranges, &mut loop_ranges, &zipped, 0..labeled.len(), loop_out);

    let mut function = Function::new(locals);

    translate_block(&block, &mut function, &code.instructions, helper_start, &metrics, &byte_index_to_instruction_index);

    code_section.function(&function);
    module.section(&code_section);
}

fn get_jump_offset(scope_metrics: &ScopeMetrics, start: usize, target: usize) -> u32 {
    if target < start {
        scope_metrics.blocks.iter().filter(|range| range.start <= start && range.start > target).map(|_| 1).sum()
    } else {
        scope_metrics.blocks.iter().filter(|range| range.end > start && range.end < target).map(|_| 1).sum()
    }
}

fn translate_block(block: &Block, function: &mut Function, instructions: &[attribute::Instruction], helper_start: u32, scope_metrics: &ScopeMetrics, byte_to_idx_map: &HashMap<u32, u32>) {

    match block {
        Block::Loop(blocks) => {
            function.instruction(&Instruction::Loop(BlockType::Empty));

            for block in blocks {
                translate_block(block, function, instructions, helper_start, scope_metrics, byte_to_idx_map);
            }

            function.instruction(&Instruction::End);
        }
        Block::Normal(blocks) => {
            function.instruction(&Instruction::Block(BlockType::Empty));

            for block in blocks {
                translate_block(block, function, instructions, helper_start, scope_metrics, byte_to_idx_map);
            }

            function.instruction(&Instruction::End);
        }
        Block::Nodes(nodes) => {
            for (component_node, labeled_node) in nodes {

                match labeled_node {
                    LabeledNode::LoopContinue { node: component} => {
                        let bytecode = &instructions[component.idx].bytecode;

                        match bytecode {
                            Bytecode::Goto(_) => {
                                function.instruction(&Instruction::Br(0));
                            },
                            Bytecode::If_icmpne(_) => {
                                function.instruction(&Instruction::I32Ne);
                                function.instruction(&Instruction::BrIf(0));

                            }
                            x => unimplemented!("Special control scheme {:?} unimplemented", x)
                        }

                        continue;
                    },

                    _ => {}
                };

                let idx = component_node.idx;
                let bytecode = &instructions[idx].bytecode;
                let byte_index = instructions[idx].bytes_index;

                match bytecode {
                    Bytecode::Aaload => {
                        function.instruction(&Instruction::Block(BlockType::Empty))
                            .instruction(&Instruction::LocalSet(helper_start))
                            .instruction(&Instruction::BrOnNonNull(0))
                            .instruction(&Instruction::Unreachable)
                            .instruction(&Instruction::End)
                            .instruction(&Instruction::LocalGet(helper_start));

                        function.instruction(&Instruction::I32Const(POINTER_SIZE))
                            .instruction(&Instruction::I32Mul)
                            .instruction(&Instruction::I32Add)
                            .instruction(&Instruction::I32Load(MemArg {
                            offset: 0,
                            align: 4,
                            memory_index: 0,
                        }));
                    }
                    Bytecode::Iload(index)
                    | Bytecode::Iload_n(index)
                    | Bytecode::Aload(index)
                    | Bytecode::Aload_n(index) => { function.instruction(&Instruction::LocalGet(*index as u32)); },
                    Bytecode::Iconst_n_m1(val) => { function.instruction(&Instruction::I32Const(*val as i32)); },
                    Bytecode::Ifnull(offset) => {
                        let target = (byte_index as isize + *offset as isize) as usize;
                        let target_idx = *byte_to_idx_map.get(&(target as u32)).unwrap();

                        if target_idx as usize == idx + 1 {
                            continue;
                        }

                        let jump = get_jump_offset(scope_metrics, idx, target_idx as usize);

                        function.instruction(&Instruction::BrOnNull(jump));
                    },
                    Bytecode::Goto(offset) => {
                        let target = (byte_index as isize + *offset as isize) as usize;
                        let target_idx = *byte_to_idx_map.get(&(target as u32)).unwrap();

                        if target_idx as usize == idx + 1 {
                            continue;
                        }

                        let jump = get_jump_offset(scope_metrics, idx, target_idx as usize);

                        function.instruction(&Instruction::Br(jump));
                    }
                    Bytecode::Putstatic(constant_pool) => {
                        //todo
                    },
                    Bytecode::Istore_n(index) => { function.instruction(&Instruction::LocalSet(*index as u32)); }
                    Bytecode::Iinc(index, inc) => { function.instruction(&Instruction::LocalGet(*index as u32))
                        .instruction(&Instruction::I32Const(*inc as i32))
                        .instruction(&Instruction::I32Add)
                        .instruction(&Instruction::LocalSet(*index as u32));
                    },
                    Bytecode::Return
                    | Bytecode::Ireturn => { function.instruction(&Instruction::Return); },
                    bytecode => unimplemented!("Bytecode not implemented {:?}", bytecode)
                }

            }
        }
    }
}