use crate::bytecode::Bytecode;
use crate::classfile::resolved::attribute::Code;
use crate::classfile::resolved::{
    attribute, AccessFlags, Attribute, Class, Constant, Method, Ref, ReturnType,
};
use crate::env::aot::analyze_class;
use crate::env::wasm::cfg::{
    create_scopes, find_loops, identify_scopes, label_nodes, Block, LabeledNode, ScopeMetrics,
};
use crate::linker::ClassLoader;
use crate::JVM;
use bitflags::Flags;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use wasm_encoder::{
    BlockType, CodeSection, Function, FunctionSection, Instruction, Module, TableSection,
    TypeSection, ValType,
};

pub const POINTER_SIZE: i32 = 4;
pub const POINTER_TYPE: ValType = ValType::I32;

pub struct CompiledClass {
    pub module: Module,
    pub link: HashMap<Arc<Ref>, u32>,
}

pub fn create_module(
    entry_class: Arc<Class>,
    class_loader: Arc<dyn ClassLoader>,
    jvm: &JVM,
) -> CompiledClass {
    let mut module = Module::new();

    let mut function_section = FunctionSection::new();
    let mut type_section = TypeSection::new();
    let mut code_section = CodeSection::new();
    let mut table = TableSection::new();

    let mut classes = HashSet::new();

    analyze_class(entry_class.clone(), &class_loader, jvm, &mut classes);

    todo!()
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

    let (code, mut locals): (&Code, Vec<_>) =
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

    let helper_start = locals.len() as u32;
    locals.push(((locals.len() as u32), (ValType::I32)));
    locals.push(((locals.len() as u32), (ValType::I32)));

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

    let mut function = Function::new(locals);

    let mut block_index = 0;

    translate_block(
        &block,
        &mut function,
        &code.instructions,
        &labeled,
        helper_start,
        &metrics,
        &byte_index_to_instruction_index,
        &scc_s,
        type_section,
        true,
        method_func_idx_map,
        class,
        jvm,
        external_references,
        &mut block_index,
    );

    function.instruction(&Instruction::End);

    function_section.function(type_section.len());

    type_section.function(params, results);

    code_section.function(&function);
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

struct ContextState {
    temp_local1: u32,
    temp_local2: u32,
}

fn translate_bytecode(
    compiler_state: &CompilerState,
    context_state: &ContextState,

    bytecode: &Bytecode,
    idx: usize,
    byte_index: u32,
    function: &mut Function,
    scope_metrics: &ScopeMetrics,
    byte_to_idx_map: &HashMap<u32, u32>,
    type_section: &mut TypeSection,
    method_func_idx_map: &HashMap<&str, usize>,
    class: &Class,
    jvm: &JVM,
    external_references: &HashMap<Arc<Ref>, u32>,
    scope_offset: u32,
    block_idx: u32,
) {
    match bytecode {
        Bytecode::Invokespecial(index) => {
            let method_constant = class.constant_pool.constants.get(index).unwrap();
            let method_ref = method_constant.as_ref().unwrap();

            // let ty = *compiler_state.descriptor_to_wasm_type_id.get(&method_ref.name_and_type.descriptor).unwrap();

            // function.instruction(&Instruction::Call(compiler_state.routines.invoke_special_routine));
            //
            // function.instruction(&Instruction::LocalTee(context_state.temp_local1));
            // function.instruction(&Instruction::LocalGet(context_state.temp_local1));
            //
            // function.instruction(&Instruction::GlobalGet(compiler_state.globals.jvm_ptr));
            // // function.instruction(&Instruction::StructGet {});
            // function.instruction(&Instruction::Call(compiler_state.routines.invoke_special_routine));
            // function.instruction(&Instruction::CallIndirect {
            //     ty,
            //     table: compiler_state.hot_function_table,
            // });

            todo!()
        }
        Bytecode::Pop => {
            function.instruction(&Instruction::Drop);
        }
        Bytecode::Iload(index)
        | Bytecode::Iload_n(index)
        | Bytecode::Aload(index)
        | Bytecode::Aload_n(index) => {
            function.instruction(&Instruction::LocalGet(*index as u32));
        }
        Bytecode::Iconst_n_m1(val) => {
            function.instruction(&Instruction::I32Const(*val as i32));
        }
        Bytecode::Ifnull(offset) => {
            let target = (byte_index as isize + *offset as isize) as usize;
            let target_idx = *byte_to_idx_map.get(&(target as u32)).unwrap();

            if target_idx as usize == idx + 1 {
                return;
            }

            let jump = get_jump_offset(scope_metrics, idx, target_idx as usize, block_idx);

            function.instruction(&Instruction::BrOnNull(jump + scope_offset));
        }
        Bytecode::Goto(offset) => {
            let target = (byte_index as isize + *offset as isize) as usize;
            let target_idx = *byte_to_idx_map.get(&(target as u32)).unwrap();

            if target_idx as usize == idx + 1 {
                return;
            }

            let jump = get_jump_offset(scope_metrics, idx, target_idx as usize, block_idx);

            function.instruction(&Instruction::Br(jump + scope_offset));
        }
        Bytecode::If_icmpne(offset) => {
            let target = (byte_index as isize + *offset as isize) as usize;
            let target_idx = *byte_to_idx_map.get(&(target as u32)).unwrap();

            if target_idx as usize == idx + 1 {
                return;
            }

            let jump = get_jump_offset(scope_metrics, idx, target_idx as usize, block_idx);

            function.instruction(&Instruction::I32Ne);
            function.instruction(&Instruction::BrIf(jump + scope_offset));
        }
        Bytecode::If_icmple(offset) => {
            let target = (byte_index as isize + *offset as isize) as usize;
            let target_idx = *byte_to_idx_map.get(&(target as u32)).unwrap();

            if target_idx as usize == idx + 1 {
                return;
            }

            let jump = get_jump_offset(scope_metrics, idx, target_idx as usize, block_idx);

            function.instruction(&Instruction::I32LeS);
            function.instruction(&Instruction::BrIf(jump + scope_offset));
        }
        Bytecode::If_icmpgt(offset) => {
            let target = (byte_index as isize + *offset as isize) as usize;
            let target_idx = *byte_to_idx_map.get(&(target as u32)).unwrap();

            if target_idx as usize == idx + 1 {
                return;
            }

            let jump = get_jump_offset(scope_metrics, idx, target_idx as usize, block_idx);

            function.instruction(&Instruction::I32GtS);
            function.instruction(&Instruction::BrIf(jump + scope_offset));
        }
        Bytecode::If_icmpge(offset) => {
            let target = (byte_index as isize + *offset as isize) as usize;
            let target_idx = *byte_to_idx_map.get(&(target as u32)).unwrap();

            if target_idx as usize == idx + 1 {
                return;
            }

            let jump = get_jump_offset(scope_metrics, idx, target_idx as usize, block_idx);

            function.instruction(&Instruction::I32GeS);
            function.instruction(&Instruction::BrIf(jump + scope_offset));
        }
        Bytecode::Ifeq(offset) => {
            let target = (byte_index as isize + *offset as isize) as usize;
            let target_idx = *byte_to_idx_map.get(&(target as u32)).unwrap();

            if target_idx as usize == idx + 1 {
                return;
            }

            let jump = get_jump_offset(scope_metrics, idx, target_idx as usize, block_idx);

            function.instruction(&Instruction::I32Eqz);
            function.instruction(&Instruction::BrIf(jump + scope_offset));
        }
        Bytecode::Ifne(offset) => {
            let target = (byte_index as isize + *offset as isize) as usize;
            let target_idx = *byte_to_idx_map.get(&(target as u32)).unwrap();

            if target_idx as usize == idx + 1 {
                return;
            }

            let jump = get_jump_offset(scope_metrics, idx, target_idx as usize, block_idx);

            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::I32Ne);
            function.instruction(&Instruction::BrIf(jump + scope_offset));
        }
        Bytecode::Irem => {
            function.instruction(&Instruction::I32RemS);
        }
        Bytecode::Ifge(offset) => {
            let target = (byte_index as isize + *offset as isize) as usize;
            let target_idx = *byte_to_idx_map.get(&(target as u32)).unwrap();

            if target_idx as usize == idx + 1 {
                return;
            }

            let jump = get_jump_offset(scope_metrics, idx, target_idx as usize, block_idx);

            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::I32GeS);
            function.instruction(&Instruction::BrIf(jump + scope_offset));
        }
        Bytecode::Ineg => {
            function.instruction(&Instruction::I32Const(i32::MAX));
            function.instruction(&Instruction::I32Xor);
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::I32Add);
        }
        Bytecode::Iadd => {
            function.instruction(&Instruction::I32Add);
        }
        Bytecode::Isub => {
            function.instruction(&Instruction::I32Sub);
        }
        Bytecode::Putstatic(_constant_pool) => {
            //todo
        }
        Bytecode::Sipush(value) => {
            function.instruction(&Instruction::I32Const(*value as i32));
        }
        Bytecode::Bipush(value) => {
            function.instruction(&Instruction::I32Const(*value as i32));
        }
        Bytecode::Istore(index) | Bytecode::Istore_n(index) => {
            function.instruction(&Instruction::LocalSet(*index as u32));
        }
        Bytecode::Iinc(index, inc) => {
            function
                .instruction(&Instruction::LocalGet(*index as u32))
                .instruction(&Instruction::I32Const(*inc as i32))
                .instruction(&Instruction::I32Add)
                .instruction(&Instruction::LocalSet(*index as u32));
        }
        Bytecode::Imul => {
            function.instruction(&Instruction::I32Mul);
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
                    if method.class != class.this_class {
                        let idx = external_references[method];
                        function.instruction(&Instruction::I32Const(idx as i32));
                        function.instruction(&Instruction::CallIndirect {
                            ty: type_section.len(),
                            table: 0,
                        });

                        let other_class = jvm
                            .find_class(&method.class, class.class_loader.clone())
                            .unwrap();
                        let other_method = other_class.get_method(&method.name_and_type).unwrap();

                        let types = get_method_params(other_method);
                        type_section.function(types.0, types.1);
                    } else {
                        let idx = *method_func_idx_map
                            .get(&method.name_and_type.name[..])
                            .unwrap() as u32;

                        function.instruction(&Instruction::Call(idx));
                    }
                }
                Constant::InterfaceMethodRef(_) => todo!(),
                _ => unreachable!(),
            }
        }
        Bytecode::Invokespecial(constant_pool) => {
            let constant = class.constant_pool.constants.get(constant_pool).unwrap();
            let method_ref = match constant {
                Constant::MethodRef(method) | Constant::InterfaceMethodRef(method) => method,
                _ => unreachable!(),
            };

            let idx = external_references[method_ref];
            function.instruction(&Instruction::I32Const(idx as i32));
            function.instruction(&Instruction::CallIndirect {
                ty: type_section.len(),
                table: 0,
            });

            let other_class = jvm
                .find_class(&method_ref.class, class.class_loader.clone())
                .unwrap();
            let other_method = other_class.get_method(&method_ref.name_and_type).unwrap();

            let types = get_method_params(other_method);
            type_section.function(types.0, types.1);

            // let named_class = class.class_loader.find_class(&*method_ref.class, jvm).unwrap();
            // let method = class.get_method(&method_ref.name_and_type).unwrap();
            //
            // let c = if !method.is_instance_initialization(&*class.this_class, &*class.class_loader, &jvm) && !named_class.access_flags.contains(AccessFlags::INTERFACE) && JVM::is_subclass(class, &named_class) && named_class.access_flags.contains(AccessFlags::SUPER) {
            //     class.super_class.as_ref().unwrap()
            // } else {
            //     &named_class
            // };
        }
        Bytecode::Return
        | Bytecode::Ireturn
        | Bytecode::Areturn
        | Bytecode::Dreturn
        | Bytecode::Freturn
        | Bytecode::Lreturn => {
            function.instruction(&Instruction::Return);
        }
        Bytecode::New(index) => {
            // function.instruction(Instruction::I31);
        }
        bytecode => unimplemented!("Bytecode not implemented {:?} @ {}", bytecode, idx),
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
    block: &Block,
    function: &mut Function,
    instructions: &[attribute::Instruction],
    labeled: &[LabeledNode],
    helper_start: u32,
    scope_metrics: &ScopeMetrics,
    byte_to_idx_map: &HashMap<u32, u32>,
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
            function.instruction(&Instruction::Loop(BlockType::Empty));

            for block in blocks {
                translate_block(
                    block,
                    function,
                    instructions,
                    labeled,
                    helper_start,
                    scope_metrics,
                    byte_to_idx_map,
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

            function.instruction(&Instruction::End);

            *block_index += 1;
        }
        Block::Normal(blocks) => {
            let types = get_block_type_signature(block, instructions);

            let type_index = type_section.len();

            if !skip {
                if types.len() > 0 {
                    function.instruction(&Instruction::Block(BlockType::FunctionType(type_index)));
                } else {
                    function.instruction(&Instruction::Block(BlockType::Empty));
                }

                *block_index += 1;
            }

            type_section.function(types, []);

            for block in blocks {
                translate_block(
                    block,
                    function,
                    instructions,
                    labeled,
                    helper_start,
                    scope_metrics,
                    byte_to_idx_map,
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
                function.instruction(&Instruction::End);
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
                            get_jump_offset(scope_metrics, component.idx, header_idx, *block_index);

                        match bytecode {
                            Bytecode::Goto(_) => {
                                function.instruction(&Instruction::Br(branch_scope));
                            }
                            Bytecode::If_icmpne(_) => {
                                function.instruction(&Instruction::I32Ne);
                                function.instruction(&Instruction::BrIf(branch_scope));
                            }
                            Bytecode::If_icmpeq(_) => {
                                function.instruction(&Instruction::I32Eq);
                                function.instruction(&Instruction::BrIf(branch_scope));
                            }
                            Bytecode::If_icmple(_) => {
                                function.instruction(&Instruction::I32LeS);
                                function.instruction(&Instruction::BrIf(branch_scope));
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
                            get_jump_offset(scope_metrics, node.idx, header_idx, *block_index);

                        function.instruction(&Instruction::I32Const(
                            header.iter().position(|i| i == target).unwrap() as i32 + 1,
                        ));
                        function.instruction(&Instruction::LocalSet(helper_start));
                        function.instruction(&Instruction::Br(branch_scope));

                        continue;
                    }
                    LabeledNode::LoopController { node, targets } => {
                        function.instruction(&Instruction::Block(BlockType::Empty));
                        function.instruction(&Instruction::LocalGet(helper_start));
                        function.instruction(&Instruction::LocalTee(helper_start + 1));
                        function.instruction(&Instruction::I32Const(0));
                        function.instruction(&Instruction::LocalSet(helper_start));

                        let mut targets: Vec<u32> = targets
                            .iter()
                            .map(|target| {
                                get_jump_offset(scope_metrics, node.idx, *target, *block_index)
                                    + 1 as u32
                            })
                            .collect();
                        targets.insert(0, 0);

                        function.instruction(&Instruction::BrTable(targets.into(), 0));
                        function.instruction(&Instruction::End);
                    }

                    _ => {}
                };

                todo!()

                // translate_bytecode(
                //     bytecode,
                //     idx,
                //     function,
                //     scope_metrics,
                //     byte_to_idx_map,
                //     type_section,
                //     method_func_idx_map,
                //     class,
                //     jvm,
                //     external_references,
                //     0,
                //     *block_index,
                // );
            }
        }
    }
}
