use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use petgraph::algo::{condensation, kosaraju_scc, tarjan_scc};
use petgraph::Graph;
use petgraph::graph::NodeIndex;
use petgraph::prelude::EdgeRef;
use wasm_encoder::{BlockType, CodeSection, EntityType, ExportKind, ExportSection, Function, FunctionSection, HeapType, ImportSection, Module, RefType, TableSection, TableType, TypeSection, ValType};
use wasm_encoder::BlockType::FunctionType;
use crate::bytecode::Bytecode;
use crate::classfile::resolved::attribute::Instruction;
use crate::classfile::resolved::{AccessFlags, Attribute, Class, Constant, FieldType, Method, MethodDescriptor};
use crate::env::Environment;
use crate::env::wasm::stub::stub::MethodSignature;
use crate::env::wasm::WasmEnvironment;
use crate::JVM;
use crate::linker::ClassLoader;
use crate::thread::{Operand, Thread};

type Instr<'a> = wasm_encoder::Instruction<'a>;

pub struct JitResult<'m> {
    pub module: Module,
    pub functions: Vec<(&'m Method, &'m Class, String)>
}

pub fn jit_module<'m>(jvm: &JVM, env: &WasmEnvironment, methods: &'m [(&Method, &Class)], thread: &mut Thread) -> JitResult<'m> {
    let mut module = Module::new();

    let mut types = TypeSection::new();

    let init_static_type = types.len();
    types.function([ValType::I32; 2], []);

    let i32_in = types.len();
    types.function([ValType::I32], []);

    let i64_in = types.len();
    types.function([ValType::I64], []);

    let ref_in = types.len();
    types.function([ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Any,
    })], []);

    let mut generated_function_signatures: HashMap<MethodDescriptor, u32> = HashMap::new();

    let mut imports = ImportSection::new();

    let function_table_id = imports.len();

    {
        imports.import("jvm", "functions", TableType {
            element_type: RefType::FUNCREF,
            minimum: 0,
            maximum: None,
        });

        imports.import("jvm", "objects", TableType {
            element_type: RefType {
                nullable: true,
                heap_type: HeapType::Any,
            },
            minimum: 0,
            maximum: None,
        });

        imports.import("jvm", "init_static", EntityType::Function(init_static_type));
    }

    let func_imports_len = 1;

    let mut code = CodeSection::new();
    let mut funcs = FunctionSection::new();
    let mut exports = ExportSection::new();

    let mut translation = ModuleTranslation {
        jvm_functions: JvmFuncs { init_static: 0 },
        type_section: &mut types,
        function_signatures: &mut generated_function_signatures,
        jvm,
        env,
        i32_in_func_type: i32_in,
        thread,
    };

    let mut funcs_out = vec![];

    for (method, class) in methods {
        let func = translate_method_java_to_wasm(method, &class, &mut translation).unwrap();

        let key = format!("{}-{}", method.class, method.identifier);
        exports.export(&key, ExportKind::Func, funcs.len() + func_imports_len);

        funcs.function(translation.get_method_descriptor_id(&method.descriptor, method.access_flags.contains(AccessFlags::STATIC)));
        code.function(&func);


        funcs_out.push((*method, *class, key));
    }

    drop(translation);

    module.section(&types);
    module.section(&imports);
    module.section(&funcs);
    module.section(&exports);
    module.section(&code);

    JitResult {
        module,
        functions: funcs_out,
    }
}

pub fn get_instruction_edges(instruction: &Instruction, mapping: &HashMap<u32, u32>, last_index: u32) -> Vec<u32> {
    match instruction.bytecode {
        Bytecode::Goto_w(offset) => vec![
            *mapping
                .get(&((offset + instruction.bytes_index as i32) as u32))
                .unwrap(),
        ],
        Bytecode::Goto(offset) => vec![
            *mapping
                .get(&(((offset as i32) + instruction.bytes_index as i32) as u32))
                .unwrap(),
        ],
        Bytecode::If_acmpeq(offset)
        | Bytecode::If_acmpne(offset)
        | Bytecode::If_icmpeq(offset)
        | Bytecode::If_icmpne(offset)
        | Bytecode::If_icmplt(offset)
        | Bytecode::If_icmpge(offset)
        | Bytecode::If_icmpgt(offset)
        | Bytecode::If_icmple(offset)
        | Bytecode::Ifeq(offset)
        | Bytecode::Ifne(offset)
        | Bytecode::Iflt(offset)
        | Bytecode::Ifge(offset)
        | Bytecode::Ifgt(offset)
        | Bytecode::Ifle(offset)
        | Bytecode::Ifnonnull(offset)
        | Bytecode::Ifnull(offset) => {
            let jump = (*mapping
                .get(&((offset as i32 + instruction.bytes_index as i32) as u32))
                .unwrap());

            if instruction.bytecode_index < last_index {
                vec![jump, instruction.bytecode_index + 1]
            } else {
                vec![jump]
            }
        }
        Bytecode::Lookupswitch(_, _) => unimplemented!(),
        Bytecode::Tableswitch { .. } => unimplemented!(),
        Bytecode::Wide(_) => unimplemented!(),
        // Bytecode::Athrow => vec![],
        _ => {
            if instruction.bytecode_index < last_index {
                vec![instruction.bytecode_index + 1]
            } else {
                vec![]
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum CompilationError {
    NoCode
}

pub fn translate_method_java_to_wasm<'a, 'b>(method: &Method, class: &Class, module: &'a mut ModuleTranslation<'b>) -> Result<Function, CompilationError> {
    let code = if let Attribute::Code(code) = method.attributes.get("Code").unwrap() {
        code
    } else {
        return Err(CompilationError::NoCode);
    };

    let mut graph = Graph::<&Instruction, u32>::new();

    let byte_to_index_map: HashMap<u32, u32> = code.instructions.iter().map(|instr| (instr.bytes_index, instr.bytecode_index)).collect();

    let last_index = code.instructions.len() as u32 - 1;

    let instruction_edge_ids: Vec<NodeIndex<u32>> = code.instructions.iter().map(|instruction| {
        graph.add_node(instruction)
    }).collect();

    for instruction in &code.instructions {
        let edges = get_instruction_edges(instruction, &byte_to_index_map, last_index);

        let a = instruction_edge_ids[instruction.bytecode_index as usize];

        edges.into_iter().for_each(|edge| {
            let b = instruction_edge_ids[edge as usize];

            graph.add_edge(a, b, 0);
        });
    }

    console_log!("{graph:?}");

    let scc_result = kosaraju_scc(&graph);

    let condensation = condensation(graph.clone(), false);
    let (_, scc_edges) = condensation.into_nodes_edges();

    let mut loops_needing_fixed = HashSet::new();

    let mut forward_jumps_needing_fixed = Vec::new();
    let mut backward_jumps_needing_fixed = HashSet::new();

    for (edge_index, scc_edge) in scc_edges.into_iter().enumerate() {
        let target_loop = &scc_result[scc_edge.target().index()];

        if target_loop.len() == 1 { continue; }
        
        let header = target_loop.iter().min().unwrap();

        let instruction_edge = &graph.raw_edges()[edge_index];

        if scc_edge.source() == scc_edge.target() {
            if instruction_edge.target() < instruction_edge.source() && &instruction_edge.target() != header {
                loops_needing_fixed.insert((scc_edge.target().index(), header));
                backward_jumps_needing_fixed.insert((instruction_edge.target().index(), scc_edge.target(), header.index()));
            }
        } else {
            //The condensation is by definition a directed acyclic graph so this is guaranteed to be a forward jump
            if instruction_edge.target().index() != header.index() {
                loops_needing_fixed.insert((scc_edge.target().index(), header));
                //Forward jump
                forward_jumps_needing_fixed.push((instruction_edge.target().index(), scc_edge.target(), header.index()))
            }
        }

    }

    let mut index = 0;
    //Key: (target instr, loop/scc id)
    //value: flag for lookup table
    let mut loop_flags = HashMap::new();

    for scc_index in 0..scc_result.len() {
        //Guarantee: none of these edges' targets are a loop header
        for (target, scc_target, _) in forward_jumps_needing_fixed.iter().chain(backward_jumps_needing_fixed.iter()) {
            if scc_target.index() != scc_index { continue; }

            let key = (*target, scc_target.index());

            if !loop_flags.contains_key(&key) {
                loop_flags.insert(
                    key,
                    index
                );
            }
        }

        index = 0;
    }

    let mut boundaries: Vec<_> = vec![];

    for edge in graph.raw_edges() {
        if edge.target().index() as i32 - edge.source().index() as i32 > 1 {
            if let Some((_, _, header)) = forward_jumps_needing_fixed.iter().find(|(target, _, header)| *target == edge.target().index()) {
                boundaries.push((
                    edge.source().index() as u32,
                    *header as u32
                ));
            } else {
                boundaries.push((
                    edge.source().index() as u32,
                    edge.target().index() as u32
                ));
            }
        }
    }

    let mut block_ranges = vec![];

    for (target, _, header) in forward_jumps_needing_fixed.iter().chain(backward_jumps_needing_fixed.iter()) {
        boundaries.push(
            (*header as u32, *target as u32 - 1)
        );
        block_ranges.push(
            (*header as u32, *target as u32 - 1, BlockRangeType::Block)
        );
    }

    for scc in &scc_result {
        if scc.len() > 1 {
            boundaries.push((
                scc.iter().min().unwrap().index() as u32,
                scc.iter().max().unwrap().index() as u32,
            ));
        }
    }

    console_log!("sccs: {scc_result:?}");
    console_log!("boundaries: {boundaries:?}");
    console_log!("forward jumps needing fixed: {forward_jumps_needing_fixed:?}");
    console_log!("backward jumps needing fixed: {backward_jumps_needing_fixed:?}");

    'scc: for scc in &scc_result {

        for node in scc {
            if graph.edges(*node).any(|edge| edge.target().index() < node.index()) {
                block_ranges.push((scc.iter().min().unwrap().index() as u32, scc.iter().max().unwrap().index() as u32, BlockRangeType::Loop));
                continue 'scc;
            }
        }
    }

    for instruction in &code.instructions {
        let edges = get_instruction_edges(instruction, &byte_to_index_map, last_index);

        for target in edges {
            if target > instruction.bytecode_index && instruction.bytecode_index + 1 == target {
                continue;
            }

            if target > instruction.bytecode_index {
                //Loop through the forward jumps, find any jumps whose target is contained within the region
                //between this instruction's index and this instruction's edge's target, then get the
                //earliest edge source. If there are no intersections, the block begins at the instruction
                let mut block_start= instruction.bytecode_index;

                loop {
                    let new_start = boundaries
                        .iter()
                        .filter(|forward_edge|
                            forward_edge.1 > block_start
                                && forward_edge.0 < block_start
                                && forward_edge.1 < target
                        )
                        .map(|forward_edge| forward_edge.0)
                        .min()
                        .unwrap_or(block_start);

                    if new_start == block_start {
                        break;
                    } else {
                        block_start = new_start;
                    }
                }

                let jump_target;

                if let Some((_, _, header)) = forward_jumps_needing_fixed.iter().find(|(target_, _, _)| *target_ as u32 == target) {
                    jump_target = (*header - 1) as u32;
                } else {
                    jump_target = target - 1;
                }

                let range = (block_start, jump_target, BlockRangeType::Block);
                console_log!("{}:{} -> {range:?}", instruction.bytecode_index, target);

                if !block_ranges.iter().any(|existing| existing.0 == range.0 && existing.1 == range.1 && existing.2 != BlockRangeType::Loop) {
                    block_ranges.push(range);
                }
            }
        }
    }

    block_ranges.sort_by(|(a_start, a_end, a_type), (b_start, b_end, b_type)| {
        a_start.cmp(b_start).then(a_end.cmp(b_end).reverse().then(a_type.cmp(b_type)))
    });

    let mut range_binding = &block_ranges[..];

    console_log!("{block_ranges:?}");

    let method_signature = MethodSignature::new(method);

    let mut params = method_signature.params.clone();
    params.extend([ValType::I32; 2]);

    let mut loop_controllers = HashMap::new();

    for (scc_index, loop_header) in &loops_needing_fixed {
        let mut flag_offsets_unsorted: Vec<(u32, u32)> = loop_flags
            .iter()
            .filter(|((_, scc_id), _)| *scc_id == *scc_index)
            .map(|((target_id, _), entry_index)| (*target_id as u32, *entry_index))
            .collect();

        flag_offsets_unsorted.sort_by(|(_, a), (_, b)| a.cmp(b));

        loop_controllers.insert(loop_header.index() as u32, LoopController {
            flag_offsets: flag_offsets_unsorted.iter().map(|(target_id, _)| *target_id).collect()
        });
    }

    let mut flagged_jumps = HashMap::new();

    for (target, scc_id, header) in forward_jumps_needing_fixed.into_iter().chain(backward_jumps_needing_fixed) {
        let flag = *loop_flags.get(
            &(target, scc_id.index())
        ).unwrap();

        flagged_jumps.insert(
            target as u32,
            FlaggedJump {
                header: header as u32,
                flag,
            }
        );
    }

    let locals = analyze_locals(&method_signature, &code.instructions);

    let mut function = Function::new_with_locals_types(
        locals.wasm_locals.clone().into_iter().chain([ValType::I32; 2])
    );

    console_log!("{locals:?}");

    let mut translating = MethodTranslation {
        byte_to_index_map: &byte_to_index_map,
        block_ranges: &block_ranges[..],
        locals: MetaLocals {
            accumulator: locals.java_to_wasm_locals.len() as u32,
            cfg_flag: locals.java_to_wasm_locals.len() as u32 + 1,
        },
        method_signature,
        flagged_jumps,
        loop_controllers,
        class,
        method,
        module,
        local_map: locals,
    };

    fold(&code.instructions[..], &mut range_binding, &mut function, 0, &mut translating);

    function.instruction(&Instr::End);

    Ok(function)

}

#[derive(Debug)]
struct LoopController {
    flag_offsets: Vec<u32>
}

#[derive(Debug)]
struct MetaLocals {
    //For temporary values
    accumulator: u32,
    cfg_flag: u32
}

#[derive(Debug)]
struct FlaggedJump {
    header: u32,
    flag: u32
}

struct JvmFuncs {
    init_static: u32
}

struct ModuleTranslation<'a> {
    jvm_functions: JvmFuncs,
    type_section: &'a mut TypeSection,
    i32_in_func_type: u32,
    function_signatures: &'a mut HashMap<MethodDescriptor, u32>,
    jvm: &'a JVM,
    env: &'a WasmEnvironment,
    thread: &'a mut Thread
}

impl<'a> ModuleTranslation<'a> {

    pub fn get_method_descriptor_id(&mut self, method_descriptor: &MethodDescriptor, is_static: bool) -> u32 {
        match self.function_signatures.get(method_descriptor) {
            None => {}
            Some(id) => return *id
        }

        let sig = MethodSignature::new_from_descriptor(method_descriptor, is_static);

        let func_id = self.type_section.len();
        self.type_section.function(sig.params, sig.return_type);

        self.function_signatures.insert(method_descriptor.clone(), func_id);
        func_id
    }

}

// #[derive(Debug)]
struct MethodTranslation<'method, 'module: 'method> {
    byte_to_index_map: &'method HashMap<u32, u32>,
    block_ranges: &'method [(u32, u32, BlockRangeType)],
    locals: MetaLocals,
    method_signature: MethodSignature,
    flagged_jumps: HashMap<u32, FlaggedJump>,
    loop_controllers: HashMap<u32, LoopController>,
    class: &'method Class,
    method: &'method Method,
    module: &'method mut ModuleTranslation<'module>,
    local_map: LocalMap
}

impl<'method, 'module: 'method> MethodTranslation<'method, 'module> {

    pub fn branch(&self, from: u32, offset: i32, function: &mut Function, callback: fn(u32) -> Instr<'method>) {
        let to = *self.byte_to_index_map.get(&((from as i32 + offset) as u32)).unwrap();
        let from = *self.byte_to_index_map.get(&from).unwrap();

        let target = if let Some(flagged) = self.flagged_jumps.get(&to) {
            function.instruction(&Instr::I32Const(flagged.flag as i32));
            function.instruction(&Instr::LocalSet(self.locals.cfg_flag));

            flagged.header
        } else {
            to
        };

        let br = self.br_offset_absolute(from, target);
        function.instruction(
            &callback(br)
        );
    }

    pub fn br_offset_absolute(&self, from: u32, to: u32) -> u32 {
        if from < to {
            console_log!("from {from} to {to}");
            (self.block_ranges.iter().filter(|range| {
                from <= range.1 && to > range.1 && range.0 <= from
            }).count() as u32).checked_sub(1).unwrap()
        } else {
            self.block_ranges.iter().filter(|range| {
                range.0 <= from && range.0 > to && range.1 >= from
            }).count() as u32
        }
    }

}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BlockRangeType {
    Block,
    Loop
}

#[derive(Debug, Clone)]
enum Capsule<'a> {
    Java(Bytecode),
    FlaggedControlFlow {
        bytecode: Bytecode,
        flag: u32
    },
    WASM(Instr<'a>),
    SetFlag(u32)
}

fn translate_instructions<'method, 'module>(instruction: &Instruction, function: &mut Function, t: &mut MethodTranslation<'method, 'module>) {

    // match instruction.bytecode {
    //     _ => unimplemented!("{:?} unimplemented in JIT compiler", instruction.bytecode)
    // }

    macro_rules! i {
        ($i:expr) => { function.instruction(&$i); }
    }

    if let Some(controller) = t.loop_controllers.get(&instruction.bytecode_index) {

        i!(Instr::Block(BlockType::Empty));
        i!(Instr::LocalGet(t.locals.cfg_flag));
        i!(Instr::I32Const(-1));
        i!(Instr::LocalSet(t.locals.cfg_flag));

        let table: Vec<u32> = controller.flag_offsets.iter().map(|target| {
            t.br_offset_absolute(instruction.bytecode_index, *target) + 1
        }).collect();

        i!(Instr::BrTable(Cow::Owned(table), 0));
        i!(Instr::End);

    }

    match instruction.bytecode {
        Bytecode::Goto(offset) => {
            t.branch(instruction.bytes_index, offset as i32, function, |br| Instr::Br(br));
        }
        Bytecode::Ifne(offset) => {
            t.branch(instruction.bytes_index, offset as i32, function, |br| Instr::BrIf(br));
        }
        Bytecode::Ifeq(offset) => {
            i!(Instr::I32Eqz);
            t.branch(instruction.bytes_index, offset as i32, function, |br| Instr::BrIf(br));
        }
        Bytecode::If_icmpge(offset) => {
            i!(Instr::I32GeS);
            t.branch(instruction.bytes_index, offset as i32, function, |br| Instr::BrIf(br));
        }
        Bytecode::Iinc(index, amount) => {
            i!(Instr::LocalGet(t.local_map.get(index as u32)));
            i!(Instr::I32Const(amount as i32));
            i!(Instr::I32Add);
            i!(Instr::LocalSet(t.local_map.get(index as u32)));
        }
        Bytecode::Invokestatic(constant_index) => {
            let ref_ = t.class
                .constant_pool
                .constants
                .get(&constant_index)
                .unwrap()
                .as_ref()
                .unwrap();

            let method_class = t.class.class_loader.find_class(t.module.thread, &ref_.class).unwrap();

            if !t.class.is_initialized() {
                i!(Instr::I32Const(t.class as *const Class as i32));
                i!(Instr::LocalGet(t.method_signature.thread_local));
                i!(Instr::Call(t.module.jvm_functions.init_static));
            }

            let method = method_class.get_method(&ref_.name_and_type).unwrap();
            let ty = t.module.get_method_descriptor_id(&method.descriptor, true);

            if method.access_flags.contains(AccessFlags::NATIVE) {
                todo!()
            }

            let func_id = t.module.env.get_linked_function_ptr(&method_class, method.get_identifier());

            i!(Instr::LocalGet(t.method_signature.thread_local));
            i!(Instr::I32Const(func_id as i32));
            i!(Instr::CallIndirect {
                ty,
                table: 0,
            });
        }
        Bytecode::Ldc(constant_index) => {
            let constant = t.class
                .constant_pool
                .constants
                .get(&(constant_index as u16))
                .unwrap();

            match constant {
                Constant::Integer(int) => {
                    i!(Instr::I32Const(*int));
                }
                Constant::Class {
                    path, depth
                } => {
                    if *depth == 0 {
                        let class = t.class.class_loader.find_class(t.module.thread, path).unwrap();
                        let ptr = class.class_instance.get().unwrap().ptr;

                        let table_index = ptr as u32;

                        i!(Instr::I32Const(table_index as i32));
                        i!(Instr::TableGet(1));

                    } else {
                        let class = t.class.class_loader.find_class(t.module.thread, path).unwrap();
                        let class = t.module.jvm.find_array_class(t.module.thread, class, *depth);
                        let object = class.class_instance.get().unwrap().ptr as u32;

                        i!(Instr::I32Const(object as i32));
                        i!(Instr::TableGet(1));
                    }
                }
                Constant::String(string) => {
                    let string = t.module.jvm.new_string(&string, t.module.thread);
                    let operand = string.consume();

                    i!(Instr::I32Const(operand.ref_() as i32));
                    i!(Instr::TableGet(1));
                }
                // _ => { unimplemented!("Ldc {constant:?} unimplemented") },
                _ => {}
            }
        }
        Bytecode::Iconst_n_m1(int) => {
            i!(Instr::I32Const(int as i32));
        }
        Bytecode::Sipush(short) => { i!(Instr::I32Const(short as i32)); },
        Bytecode::Bipush(byte) => { i!(Instr::I32Const(byte as i32)); },
        Bytecode::Istore(index)
        | Bytecode::Istore_n(index) => {
            let local = t.local_map.get(index as u32);
            i!(Instr::LocalSet(local));
        }
        Bytecode::Iload(index)
        | Bytecode::Iload_n(index) => {
            let local = t.local_map.get(index as u32);
            i!(Instr::LocalGet(local));
        }
        Bytecode::Ireturn => {
            i!(Instr::Return);
        }
        Bytecode::Return => {
            i!(Instr::Return);
        }
        _ => { i!(Instr::Nop); }
        // _ => unimplemented!("{:?}", instruction.bytecode)
    }

}

#[derive(Debug)]
struct LocalMap {
    java_to_wasm_locals: HashMap<u32, u32>,
    wasm_locals: Vec<ValType>
}

impl LocalMap {

    fn get(&self, java_local: u32) -> u32 {
        self.java_to_wasm_locals.get(&java_local).copied().unwrap()
    }

}

fn analyze_locals(method_signature: &MethodSignature, instructions: &[Instruction]) -> LocalMap {
    let mut locals = HashMap::new();

    for instruction in instructions {
        match instruction.bytecode {
            Bytecode::Iload(index) | Bytecode::Iload_n(index)
            | Bytecode::Istore(index) | Bytecode::Istore_n(index) => { locals.insert(index as u32, ValType::I32); },
            Bytecode::Lload(index) | Bytecode::Lload_n(index) => { locals.insert(index as u32, ValType::I64); },
            Bytecode::Aload(index) | Bytecode::Aload_n(index) => { locals.insert(index as u32, ValType::Ref(RefType { nullable: true, heap_type: HeapType::Any })); },
            Bytecode::Fstore(index) | Bytecode::Fstore_n(index) => { locals.insert(index as u32, ValType::F32); },
            Bytecode::Dstore(index) | Bytecode::Dstore_n(index) => { locals.insert(index as u32, ValType::F64); },
            _ => {}
        }
    }

    let mut locals: Vec<(u32, ValType)> = locals.into_iter().collect();
    locals.sort_by_key(|local| local.0);

    let mut map = HashMap::new();

    for (wasm_index, (java_local, _)) in locals.iter().enumerate() {
        if wasm_index > method_signature.params.len() - 1 {
            //Dealing with a local that isn't part of the method signature or the thread local
            map.insert(*java_local, wasm_index as u32 + 1);
        } else {
            //A local from the method signature
            map.insert(*java_local, wasm_index as u32 + 1);
        }
    }

    LocalMap {
        java_to_wasm_locals: map,
        wasm_locals: (method_signature.params.len()-1..locals.len()).map(|index| locals[index].1).collect(),
    }
}

fn fold<'method, 'module: 'method, 'a, 'b: 'a>(instructions: &'method [Instruction], ranges: &'a mut &'b [(u32, u32, BlockRangeType)], function: &mut Function, absolute_offset: usize, translating: &mut MethodTranslation<'method, 'module>) -> usize {

    if instructions.len() == 0 {
        return 0;
    }

    let mut consumed_instructions = 0;

    loop {
        console_log!("folding, len: {}, offset: {}-{}", instructions.len(), absolute_offset, instructions.len() + absolute_offset-1);

        let (next_block_start, next_block_end, block_type) = match ranges.get(0).copied() {
            None => {
                for instr in &instructions[consumed_instructions..] {
                    translate_instructions(instr, function, translating);
                }

                return instructions.len();
            }
            Some(range) => {
                //Next block is not nested in the current one
                if range.0 as usize > instructions.len() + absolute_offset - 1 {
                    for instr in &instructions[consumed_instructions..] {
                        translate_instructions(instr, function, translating);
                    }

                    return instructions.len();
                }

                range
            },
        };

        console_log!("next_block_start: {next_block_start} next_block_end: {next_block_end}");

        let instrs_to_write = next_block_start as usize - absolute_offset - consumed_instructions;

        for index in 0..instrs_to_write {
            translate_instructions(&instructions[index + consumed_instructions], function, translating);
        }

        // console_log!("written: {instrs_to_write}, next_block_start: {next_block_start}, absolute: {absolute_offset}, n-a: {}, consumed: {consumed_instructions}", next_block_start as usize - absolute_offset);

        consumed_instructions += instrs_to_write;

        match block_type {
            BlockRangeType::Block => {
                let block_type = match instructions.get(next_block_start as usize - absolute_offset).map(|instr| &instr.bytecode) {
                    Some(Bytecode::Ifeq(_) | Bytecode::Ifne(_)) => BlockType::FunctionType(translating.module.i32_in_func_type),
                    _ => BlockType::Empty
                };

                function.instruction(&Instr::Block(block_type));
            },
            BlockRangeType::Loop => { function.instruction(&Instr::Loop(BlockType::Empty)); }
        }

        *ranges = &ranges[1..];

        consumed_instructions += fold(
            &instructions[next_block_start as usize - absolute_offset..next_block_end as usize - absolute_offset + 1],
            ranges,
            function,
            next_block_start as usize,
            translating
        );

        function.instruction(&Instr::End);
    }

}