use crate::bytecode::{ArrayType, Bytecode};
use crate::classfile::resolved::attribute::Instruction;
use crate::classfile::resolved::{
    AccessFlags, Attribute, Class, Constant, FieldType, Method, MethodDescriptor, ReturnType,
};
use crate::env::wasm::jit::OperandType::{Class1, Class2, Ref};
use crate::env::wasm::stub::stub::{create_base_object_type, sub_type_from_class, MethodSignature};
use crate::env::wasm::WasmEnvironment;
use crate::env::Environment;
use crate::linker::{
    routine_resolve_field, routine_resolve_method, routine_select_method, ClassLoader,
};
use crate::thread::{Operand, OperandTag, Thread};
use crate::JVM;
use petgraph::algo::{condensation, kosaraju_scc, tarjan_scc};
use petgraph::graph::NodeIndex;
use petgraph::prelude::EdgeRef;
use petgraph::Graph;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::vec::Splice;
use wasm_encoder::BlockType::FunctionType;
use wasm_encoder::{
    BlockType, CodeSection, CompositeType, ConstExpr, EntityType, ExportKind, ExportSection,
    Function, FunctionSection, GlobalSection, GlobalType, HeapType, ImportSection, MemArg,
    MemorySection, MemoryType, Module, RefType, StorageType, StructType, SubType, TableSection,
    TableType, TypeSection, ValType,
};
use wasmparser::WasmFeatures;

type Instr<'a> = wasm_encoder::Instruction<'a>;

pub struct JitResult<'m> {
    pub module: Module,
    pub functions: Vec<(&'m Method, &'m Class, String)>,
}

pub fn jit_module<'m>(
    jvm: &JVM,
    env: &WasmEnvironment,
    methods: &'m [(&Method, &Class)],
    thread: &mut Thread,
) -> Result<JitResult<'m>, JITError> {
    let mut module = Module::new();

    let mut globals = GlobalSection::new();
    let mut memories = MemorySection::new();

    globals.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: true,
        },
        &ConstExpr::i32_const(-1),
    );

    memories.memory(MemoryType {
        minimum: 1,
        maximum: Some(1),
        memory64: false,
        shared: false,
    });

    let mut types = TypeSection::new();

    let init_static_type = types.len();
    types.function([ValType::I32; 2], []);

    let i32_in_func_type = types.len();
    types.function([ValType::I32], []);

    let _2_i32_in_func_type = types.len();
    types.function([ValType::I32; 2], []);

    let ref_in_func_type = types.len();
    types.function(
        [ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Any,
        })],
        [],
    );

    let ref_in_i32_out_func_type = types.len();
    types.function(
        [ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Any,
        })],
        [ValType::I32],
    );

    let i64_in = types.len();
    types.function([ValType::I64], []);

    let routine_select_method = types.len();
    types.function([ValType::I32; 4], [ValType::I32]);

    let routine_checkcast = types.len();
    types.function([ValType::I32; 2], [ValType::I32]);

    let object_type = create_base_object_type(&mut types);

    let array_type = types.len();
    types.rec([SubType {
        is_final: false,
        supertype_idx: Some(object_type),
        composite_type: CompositeType::Struct(StructType {
            fields: Box::new([
                wasm_encoder::FieldType {
                    element_type: StorageType::Val(ValType::I32),
                    mutable: true,
                },
                wasm_encoder::FieldType {
                    element_type: StorageType::Val(ValType::I64),
                    mutable: true,
                },
                wasm_encoder::FieldType {
                    element_type: StorageType::Val(ValType::Ref(RefType {
                        nullable: false,
                        heap_type: HeapType::Array,
                    })),
                    mutable: true,
                },
            ]),
        }),
    }]);

    let mut static_function_signatures: HashMap<MethodDescriptor, u32> = HashMap::new();
    let mut generated_function_signatures: HashMap<MethodDescriptor, u32> = HashMap::new();
    let mut array_storage_types: HashMap<StorageType, u32> = HashMap::new();
    let mut generated_object_recs: HashMap<String, u32> = HashMap::new();

    generated_object_recs.insert("java/lang/Object".into(), object_type);

    for storage_type in [
        StorageType::Val(ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Struct,
        })),
        StorageType::I8,
        StorageType::I16,
        StorageType::Val(ValType::I32),
        StorageType::Val(ValType::I64),
        StorageType::Val(ValType::F32),
        StorageType::Val(ValType::F64),
    ] {
        array_storage_types.insert(storage_type, types.len());
        types.array(&storage_type, true);
    }

    let mut imports = ImportSection::new();

    let function_table_id = imports.len();

    let jit_routine_select_method;
    let jit_routine_checkcast;

    {
        imports.import(
            "jvm",
            "functions",
            TableType {
                element_type: RefType::FUNCREF,
                minimum: 0,
                maximum: None,
            },
        );

        imports.import(
            "jvm",
            "objects",
            TableType {
                element_type: RefType {
                    nullable: true,
                    heap_type: HeapType::Any,
                },
                minimum: 0,
                maximum: None,
            },
        );

        imports.import(
            "jvm",
            "memory",
            EntityType::Memory(MemoryType {
                minimum: 1,
                maximum: Some(65536),
                memory64: false,
                shared: true,
            }),
        );

        imports.import("jvm", "init_static", EntityType::Function(init_static_type));
        jit_routine_select_method = 1;
        imports.import(
            "jvm",
            "_jit_routine_select_method",
            EntityType::Function(routine_select_method),
        );
        jit_routine_checkcast = 2;
        imports.import(
            "jvm",
            "_jit_routine_checkcast",
            EntityType::Function(routine_checkcast),
        );
    }

    let func_imports_len = 3;

    let mut code = CodeSection::new();
    let mut funcs = FunctionSection::new();
    let mut exports = ExportSection::new();

    let mut translation = ModuleTranslation {
        jvm_functions: JvmFuncs {
            init_static: 0,
            _jit_routine_select_method: jit_routine_select_method,
            _jit_routine_checkcast: jit_routine_checkcast,
        },
        type_section: &mut types,
        function_signatures: &mut generated_function_signatures,
        static_function_signatures: &mut static_function_signatures,
        array_storage_types: &array_storage_types,
        object_recs: &mut generated_object_recs,
        jvm,
        env,
        i32_in_func_type,
        _2_i32_in_func_type,
        ref_in_func_type,
        ref_in_i32_out_func_type,
        array_object_type: array_type,
        object_type,
        thread,
    };

    let mut funcs_out = vec![];

    for (method, class) in methods {
        let func = translate_method_java_to_wasm(method, &class, &mut translation)?;

        let key = format!("{}-{}", method.class, method.identifier);
        exports.export(&key, ExportKind::Func, funcs.len() + func_imports_len);

        funcs.function(translation.get_method_descriptor_id(
            &method.descriptor,
            method.access_flags.contains(AccessFlags::STATIC),
        ));
        code.function(&func);

        funcs_out.push((*method, *class, key));
    }

    drop(translation);

    module.section(&types);
    module.section(&imports);
    module.section(&funcs);
    module.section(&memories);
    module.section(&globals);
    module.section(&exports);
    module.section(&code);

    Ok(JitResult {
        module,
        functions: funcs_out,
    })
}

pub fn get_instruction_edges(
    instruction: &Instruction,
    mapping: &HashMap<u32, u32>,
    last_index: u32,
) -> Vec<u32> {
    match instruction.bytecode {
        Bytecode::Goto_w(offset) => vec![*mapping
            .get(&((offset + instruction.bytes_index as i32) as u32))
            .unwrap()],
        Bytecode::Goto(offset) => vec![*mapping
            .get(&(((offset as i32) + instruction.bytes_index as i32) as u32))
            .unwrap()],
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
            let jump = *mapping
                .get(&((offset as i32 + instruction.bytes_index as i32) as u32))
                .unwrap();

            if instruction.bytecode_index < last_index {
                vec![jump, instruction.bytecode_index + 1]
            } else {
                vec![jump]
            }
        }
        Bytecode::Lookupswitch(default, ref jumps) => {
            let mut targets = vec![
                *mapping
                    .get(&((default + instruction.bytes_index as i32) as u32))
                    .unwrap()
            ];

            for jump in jumps {
                targets.push(
                    *mapping
                        .get(&((jump.offset + instruction.bytes_index as i32) as u32))
                        .unwrap()
                );
            }

            targets
        },
        Bytecode::Tableswitch { default, ref offsets, .. } => {
            let mut targets = vec![
                *mapping
                    .get(&((default + instruction.bytes_index as i32) as u32))
                    .unwrap()
            ];

            for jump in offsets {
                targets.push(
                    *mapping
                        .get(&((jump + instruction.bytes_index as i32) as u32))
                        .unwrap()
                );
            }

            targets
        },
        Bytecode::Athrow => vec![],
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
pub enum JITError {
    NoCode,
    UnimplementedBytecode(Bytecode),
    AnalysisFailure,
    LinkingError(String)
}

pub fn translate_method_java_to_wasm<'a, 'b>(
    method: &Method,
    class: &Class,
    module: &'a mut ModuleTranslation<'b>,
) -> Result<Function, JITError> {
    let code = if let Attribute::Code(code) = method.attributes.get("Code").unwrap() {
        code
    } else {
        return Err(JITError::NoCode);
    };

    let mut graph = Graph::<&Instruction, u32>::new();

    let byte_to_index_map: HashMap<u32, u32> = code
        .instructions
        .iter()
        .map(|instr| (instr.bytes_index, instr.bytecode_index))
        .collect();

    let last_index = code.instructions.len() as u32 - 1;

    let instruction_edge_ids: Vec<NodeIndex<u32>> = code
        .instructions
        .iter()
        .map(|instruction| graph.add_node(instruction))
        .collect();

    for instruction in &code.instructions {
        let edges = get_instruction_edges(instruction, &byte_to_index_map, last_index);

        let a = instruction_edge_ids[instruction.bytecode_index as usize];

        edges.into_iter().for_each(|edge| {
            let b = instruction_edge_ids[edge as usize];

            graph.add_edge(a, b, 0);
        });
    }

    let scc_result = kosaraju_scc(&graph);

    let condensation = condensation(graph.clone(), false);
    let (_, scc_edges) = condensation.into_nodes_edges();

    let mut loops_needing_fixed = HashSet::new();

    let mut forward_jumps_needing_fixed = Vec::new();
    let mut backward_jumps_needing_fixed = HashSet::new();

    for (edge_index, scc_edge) in scc_edges.into_iter().enumerate() {
        let target_loop = &scc_result[scc_edge.target().index()];

        if target_loop.len() == 1 {
            continue;
        }

        let header = target_loop.iter().min().unwrap();

        let instruction_edge = &graph.raw_edges()[edge_index];

        if scc_edge.source() == scc_edge.target() {
            if instruction_edge.target() < instruction_edge.source()
                && &instruction_edge.target() != header
            {
                loops_needing_fixed.insert((scc_edge.target().index(), header));
                backward_jumps_needing_fixed.insert((
                    instruction_edge.target().index(),
                    scc_edge.target(),
                    header.index(),
                ));
            }
        } else {
            //The condensation is by definition a directed acyclic graph so this is guaranteed to be a forward jump
            if instruction_edge.target().index() != header.index() {
                loops_needing_fixed.insert((scc_edge.target().index(), header));
                //Forward jump
                forward_jumps_needing_fixed.push((
                    instruction_edge.target().index(),
                    scc_edge.target(),
                    header.index(),
                ))
            }
        }
    }

    let mut index = 0;
    //Key: (target instr, loop/scc id)
    //value: flag for lookup table
    let mut loop_flags = HashMap::new();

    for scc_index in 0..scc_result.len() {
        //Guarantee: none of these edges' targets are a loop header
        for (target, scc_target, _) in forward_jumps_needing_fixed
            .iter()
            .chain(backward_jumps_needing_fixed.iter())
        {
            if scc_target.index() != scc_index {
                continue;
            }

            let key = (*target, scc_target.index());

            if !loop_flags.contains_key(&key) {
                loop_flags.insert(key, index);
            }
        }

        index = 0;
    }

    let mut boundaries: Vec<_> = vec![];

    for edge in graph.raw_edges() {
        if edge.target().index() as i32 - edge.source().index() as i32 > 1 {
            if let Some((_, _, header)) = forward_jumps_needing_fixed
                .iter()
                .find(|(target, _, header)| *target == edge.target().index())
            {
                boundaries.push((edge.source().index() as u32, *header as u32));
            } else {
                boundaries.push((edge.source().index() as u32, edge.target().index() as u32));
            }
        }
    }

    let mut block_ranges = vec![];

    for (target, _, header) in forward_jumps_needing_fixed
        .iter()
        .chain(backward_jumps_needing_fixed.iter())
    {
        boundaries.push((*header as u32, *target as u32 - 1));
        block_ranges.push((*header as u32, *target as u32 - 1, BlockRangeType::Block));
    }

    for scc in &scc_result {
        if scc.len() > 1 {
            boundaries.push((
                scc.iter().min().unwrap().index() as u32,
                scc.iter().max().unwrap().index() as u32,
            ));
        }
    }

    'scc: for scc in &scc_result {
        for node in scc {
            if graph
                .edges(*node)
                .any(|edge| edge.target().index() < node.index())
            {
                block_ranges.push((
                    scc.iter().min().unwrap().index() as u32,
                    scc.iter().max().unwrap().index() as u32,
                    BlockRangeType::Loop,
                ));
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
                let mut block_start = instruction.bytecode_index;

                loop {
                    let new_start = boundaries
                        .iter()
                        .filter(|forward_edge| {
                            forward_edge.1 > block_start
                                && forward_edge.0 < block_start
                                && forward_edge.1 < target
                        })
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

                if let Some((_, _, header)) = forward_jumps_needing_fixed
                    .iter()
                    .find(|(target_, _, _)| *target_ as u32 == target)
                {
                    jump_target = (*header - 1) as u32;
                } else {
                    jump_target = target - 1;
                }

                let range = (block_start, jump_target, BlockRangeType::Block);
                // console_log!("{}:{} -> {range:?}", instruction.bytecode_index, target);

                if !block_ranges.iter().any(|existing| {
                    existing.0 == range.0
                        && existing.1 == range.1
                        && existing.2 != BlockRangeType::Loop
                }) {
                    block_ranges.push(range);
                }
            }
        }
    }

    block_ranges.sort_by(|(a_start, a_end, a_type), (b_start, b_end, b_type)| {
        a_start
            .cmp(b_start)
            .then(a_end.cmp(b_end).reverse().then(a_type.cmp(b_type)))
    });

    let mut range_binding = &block_ranges[..];

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

        loop_controllers.insert(
            loop_header.index() as u32,
            LoopController {
                flag_offsets: flag_offsets_unsorted
                    .iter()
                    .map(|(target_id, _)| *target_id)
                    .collect(),
            },
        );
    }

    let mut flagged_jumps = HashMap::new();

    for (target, scc_id, header) in forward_jumps_needing_fixed
        .into_iter()
        .chain(backward_jumps_needing_fixed)
    {
        let flag = *loop_flags.get(&(target, scc_id.index())).unwrap();

        flagged_jumps.insert(
            target as u32,
            FlaggedJump {
                header: header as u32,
                flag,
            },
        );
    }

    let locals = analyze_locals(&method_signature, &code.instructions);

    let mut function =
        Function::new_with_locals_types(locals.wasm_locals.clone().into_iter().chain([
            ValType::I32,
            ValType::I64,
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Any,
            }),
            ValType::I32,
            ValType::I32,
            ValType::I64,
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Any,
            }),
        ]));

    let analyzed_stack = analyze_stack(&code.instructions, class, module.thread)?;
    let mut splices = HashMap::new();

    for instruction in &code.instructions {
        match instruction.bytecode {
            Bytecode::Invokevirtual(constant_index) => {
                let method_ref = class
                    .constant_pool
                    .constants
                    .get(&constant_index)
                    .unwrap()
                    .as_ref()
                    .unwrap();

                let classpath = &**method_ref.class;

                let method_ref_class = class
                    .class_loader
                    .find_class(module.thread, classpath)
                    .unwrap();

                let resolved_method =
                    routine_resolve_method(&module.thread, &method_ref, &method_ref_class).unwrap();
                let resolved_method_class = class
                    .class_loader
                    .find_class(module.thread, &method_ref.class)
                    .unwrap();

                'walk: {
                    if !resolved_method.is_signature_polymorphic(classpath) {
                        //Invokevirtual needs an object, so it shouldn't ever be the first instruction, so this access is fine
                        let stack_length =
                            analyzed_stack[instruction.bytecode_index as usize - 1].len();
                        let target_length = stack_length - resolved_method.descriptor.args.len();

                        for walk_index in (0..instruction.bytecode_index).rev() {
                            let walk_stack_length = analyzed_stack[walk_index as usize].len();
                            if walk_stack_length == target_length {
                                assert!(splices
                                    .insert(
                                        walk_index + 1,
                                        SpliceType::Invokevirtual(
                                            resolved_method.identifier.clone(),
                                            resolved_method_class.clone()
                                        )
                                    )
                                    .is_none());
                                break 'walk;
                            }
                        }
                    } else {
                        todo!()
                    };

                    let stack_debug: Vec<_> = analyzed_stack.iter().enumerate().collect();
                    // panic!("Couldn't find splice target for Invokevirtual {stack_debug:#?}");
                    return Err(JITError::AnalysisFailure);
                }
            }
            Bytecode::Bastore | Bytecode::Iastore | Bytecode::Aastore => 'walk: {
                let stack_length = analyzed_stack[instruction.bytecode_index as usize - 1].len();
                let target_length = stack_length - 2;

                let storage_type = match instruction.bytecode {
                    Bytecode::Bastore => StorageType::I8,
                    Bytecode::Iastore => StorageType::Val(ValType::I32),
                    Bytecode::Aastore => StorageType::Val(ValType::Ref(RefType { nullable: true, heap_type: HeapType::Struct })),
                    _ => unreachable!()
                };

                for walk_index in (0..instruction.bytecode_index).rev() {
                    let walk_stack_length = analyzed_stack[walk_index as usize].len();
                    if walk_stack_length == target_length {
                        assert!(splices
                            .insert(walk_index + 1, SpliceType::Arraystore(storage_type))
                            .is_none());
                        break 'walk;
                    }
                }

                return Err(JITError::AnalysisFailure);
            }
            _ => {}
        }
    }

    let max_local = method_signature.params.len() as u32 + locals.wasm_locals.len() as u32;

    let mut translating = MethodTranslation {
        byte_to_index_map: &byte_to_index_map,
        block_ranges: &block_ranges[..],
        locals: MetaLocals {
            i32_accumulator: max_local,
            i64_accumulator: max_local + 1,
            ref_accumulator: max_local + 2,
            cfg_flag: max_local + 3,
            i32_accumulator_2: max_local + 4,
            i64_accumulator_2: max_local + 5,
            ref_accumulator_2: max_local + 6,
        },
        method_signature,
        flagged_jumps,
        loop_controllers,
        class,
        method,
        module,
        local_map: locals,
        analyzed_stack,
        splices,
    };

    if loops_needing_fixed.len() > 0 {
        function.instruction(&Instr::I32Const(-1));
        function.instruction(&Instr::LocalSet(translating.locals.cfg_flag));
    }

    fold(
        &code.instructions[..],
        &mut range_binding,
        &mut function,
        0,
        &mut translating,
    )?;

    function.instruction(&Instr::End);

    Ok(function)
}

#[derive(Debug)]
struct LoopController {
    flag_offsets: Vec<u32>,
}

#[derive(Debug)]
struct MetaLocals {
    //For temporary values
    i32_accumulator: u32,
    i64_accumulator: u32,
    ref_accumulator: u32,
    cfg_flag: u32,
    i32_accumulator_2: u32,
    i64_accumulator_2: u32,
    ref_accumulator_2: u32,
}

#[derive(Debug)]
struct FlaggedJump {
    header: u32,
    flag: u32,
}

struct JvmFuncs {
    init_static: u32,
    _jit_routine_select_method: u32,
    _jit_routine_checkcast: u32,
}

struct ModuleTranslation<'a> {
    jvm_functions: JvmFuncs,
    type_section: &'a mut TypeSection,
    i32_in_func_type: u32,
    _2_i32_in_func_type: u32,
    ref_in_func_type: u32,
    ref_in_i32_out_func_type: u32,
    array_object_type: u32,
    function_signatures: &'a mut HashMap<MethodDescriptor, u32>,
    static_function_signatures: &'a mut HashMap<MethodDescriptor, u32>,
    array_storage_types: &'a HashMap<StorageType, u32>,
    object_recs: &'a mut HashMap<String, u32>,
    jvm: &'a JVM,
    env: &'a WasmEnvironment,
    thread: &'a mut Thread,
    object_type: u32,
}

impl<'a> ModuleTranslation<'a> {
    pub fn get_method_descriptor_id(
        &mut self,
        method_descriptor: &MethodDescriptor,
        is_static: bool,
    ) -> u32 {
        if is_static {
            match self.static_function_signatures.get(method_descriptor) {
                None => {}
                Some(id) => return *id,
            }
        } else {
            match self.function_signatures.get(method_descriptor) {
                None => {}
                Some(id) => return *id,
            }
        }

        let sig = MethodSignature::new_from_descriptor(method_descriptor, is_static);

        let func_id = self.type_section.len();
        self.type_section.function(sig.params, sig.return_type);

        if is_static {
            self.static_function_signatures
                .insert(method_descriptor.clone(), func_id);
        } else {
            self.function_signatures
                .insert(method_descriptor.clone(), func_id);
        }

        func_id
    }

    pub fn get_object_type(&mut self, class: &Class) -> u32 {
        if let Some(id) = self.object_recs.get(&*class.this_class) {
            return *id;
        }

        let super_id = self.get_object_type(class.super_class.as_ref().unwrap());

        let parents = class.parents();
        let recursive_fields = parents.iter().rev()
            .map(|parent| &***parent)
            .chain([class])
            .map(|parent| parent.fields.iter().filter(|field| !field.access_flags.contains(AccessFlags::STATIC)))
            .flatten();

        let sub_type = sub_type_from_class(super_id, recursive_fields);
        let id = self.type_section.len();
        self.type_section.rec([sub_type]);

        self.object_recs.insert((*class.this_class).clone(), id);

        id
    }
}

#[derive(Debug)]
enum SpliceType {
    Invokevirtual(Arc<String>, Arc<Class>),
    Arraystore(StorageType)
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
    local_map: LocalMap,
    analyzed_stack: Vec<Vec<OperandType>>,
    splices: HashMap<u32, SpliceType>,
}

impl<'method, 'module: 'method> MethodTranslation<'method, 'module> {
    pub fn branch(
        &self,
        from: u32,
        offset: i32,
        function: &mut Function,
        callback: fn(u32) -> Instr<'method>,
    ) {
        let to = *self
            .byte_to_index_map
            .get(&((from as i32 + offset) as u32))
            .unwrap();
        let from = *self.byte_to_index_map.get(&from).unwrap();

        let target = if let Some(flagged) = self.flagged_jumps.get(&to) {
            function.instruction(&Instr::I32Const(flagged.flag as i32));
            function.instruction(&Instr::LocalSet(self.locals.cfg_flag));

            flagged.header
        } else {
            to
        };

        let br = self.br_offset_absolute(from, target);
        function.instruction(&callback(br));
    }

    pub fn br_offset_absolute(&self, from: u32, to: u32) -> u32 {
        if from < to {
            (self
                .block_ranges
                .iter()
                .filter(|range| from <= range.1 && to > range.1 && range.0 <= from)
                .count() as u32)
                .checked_sub(1)
                .unwrap()
        } else {
            let (loop_block, _) = self
                .block_ranges
                .iter()
                .enumerate()
                .filter(|(_, block)| block.0 == to && block.2 == BlockRangeType::Loop)
                .last()
                .unwrap();

            self.block_ranges
                .iter()
                .enumerate()
                .filter(|(index, range)| {
                    range.0 <= from && range.0 >= to && range.1 >= from && *index > loop_block
                })
                .count() as u32
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BlockRangeType {
    Block,
    Loop,
}

#[derive(Debug, Clone)]
enum Capsule<'a> {
    Java(Bytecode),
    FlaggedControlFlow { bytecode: Bytecode, flag: u32 },
    WASM(Instr<'a>),
    SetFlag(u32),
}

fn get_class_recursive_member_field_count(class: &Class) -> u32 {
    let field_id_offset = class
        .parents()
        .iter()
        .map(|parent| {
            parent
                .fields
                .iter()
                .filter(|field| !field.access_flags.contains(AccessFlags::STATIC))
        })
        .flatten()
        .count() as u32
        + 2;

    class
        .fields
        .iter()
        .filter(|field| !field.access_flags.contains(AccessFlags::STATIC))
        .count() as u32
        + field_id_offset
}

fn translate_instructions<'method, 'module>(
    instruction: &Instruction,
    function: &mut Function,
    t: &mut MethodTranslation<'method, 'module>,
) -> Result<(), JITError> {
    // match instruction.bytecode {
    //     _ => unimplemented!("{:?} unimplemented in JIT compiler", instruction.bytecode)
    // }

    macro_rules! i {
        ($i:expr) => {
            function.instruction(&$i);
        };
    }

    if let Some(controller) = t.loop_controllers.get(&instruction.bytecode_index) {
        i!(Instr::Block(BlockType::Empty));
        i!(Instr::LocalGet(t.locals.cfg_flag));
        i!(Instr::I32Const(-1));
        i!(Instr::LocalSet(t.locals.cfg_flag));

        let table: Vec<u32> = controller
            .flag_offsets
            .iter()
            .map(|target| t.br_offset_absolute(instruction.bytecode_index, *target) + 1)
            .collect();

        i!(Instr::BrTable(Cow::Owned(table), 0));
        i!(Instr::End);
    }

    if let Some(splice) = t.splices.get(&instruction.bytecode_index) {
        match splice {
            SpliceType::Invokevirtual(method, class) => {
                let jlo = t
                    .module
                    .jvm
                    .fetch_linked_class("java/lang/Object", 0)
                    .unwrap();
                let object_ty = t.module.get_object_type(&jlo);

                i!(Instr::LocalTee(t.locals.ref_accumulator));
                i!(Instr::RefCastNullable(HeapType::Concrete(object_ty)));
                i!(Instr::StructGet {
                    struct_type_index: object_ty,
                    field_index: 0
                });

                let method_id = class
                    .methods
                    .iter()
                    .position(|c_method| c_method.get_identifier() == &**method)
                    .ok_or(JITError::NoCode)?;

                i!(Instr::LocalGet(t.method_signature.thread_local));
                i!(Instr::I32Const(method_id as i32));
                i!(Instr::I32Const(&**class as *const Class as i32));
                i!(Instr::Call(
                    t.module.jvm_functions._jit_routine_select_method
                ));
                i!(Instr::LocalSet(t.locals.i32_accumulator));

                i!(Instr::I32Const(1));
                i!(Instr::GlobalGet(0));
                i!(Instr::I32Add);
                i!(Instr::GlobalSet(0));
                i!(Instr::GlobalGet(0));
                i!(Instr::I32Const(2));
                i!(Instr::I32Shl);
                i!(Instr::LocalGet(t.locals.i32_accumulator));
                i!(Instr::I32Store(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 1,
                }));

                i!(Instr::LocalGet(t.locals.ref_accumulator));
            }
            SpliceType::Arraystore(storage_type) => {
                let &array_type = t.module.array_storage_types.get(&storage_type).unwrap();

                i!(Instr::RefCastNullable(HeapType::Concrete(
                    t.module.array_object_type
                )));
                i!(Instr::StructGet {
                    struct_type_index: t.module.array_object_type,
                    field_index: 2
                });
                i!(Instr::RefCastNullable(HeapType::Concrete(array_type)));
            }
        }
    }

    match instruction.bytecode {
        Bytecode::Goto(offset) => {
            t.branch(instruction.bytes_index, offset as i32, function, |br| {
                Instr::Br(br)
            });
        }
        Bytecode::Lookupswitch(default, ref jumps) => {
            let default_target = *t.byte_to_index_map
                .get(&instruction.bytes_index.saturating_add_signed(default))
                .unwrap();

            let default_br = if default_target == instruction.bytecode_index + 1 {
                0
            } else {
                t.br_offset_absolute(instruction.bytecode_index, default_target) + 1
            };

            let branches: Vec<u32> = jumps.iter().map(|entry| {
                let target = *t.byte_to_index_map
                    .get(&instruction.bytes_index.saturating_add_signed(entry.offset))
                    .unwrap();

                if target == instruction.bytecode_index + 1 {
                    0
                } else {
                    t.br_offset_absolute(instruction.bytecode_index, target) + 1
                }
            }).collect();

            i!(Instr::LocalSet(t.locals.i32_accumulator));

            for (index, jump) in jumps.iter().enumerate() {
                i!(Instr::I32Const(index as i32));
                i!(Instr::I32Const(-1));

                i!(Instr::LocalGet(t.locals.i32_accumulator));
                i!(Instr::I32Const(jump.key));
                i!(Instr::I32Eq);
                i!(Instr::Select);
                i!(Instr::LocalSet(t.locals.i32_accumulator_2));
            }

            i!(Instr::LocalGet(t.locals.i32_accumulator_2));
            i!(Instr::Block(BlockType::FunctionType(t.module.i32_in_func_type)));
            i!(Instr::BrTable(Cow::Owned(branches), default_br));
            i!(Instr::End);
        }
        Bytecode::Tableswitch { default, low, high, ref offsets } => {
            let default_target = *t.byte_to_index_map
                .get(&instruction.bytes_index.saturating_add_signed(default))
                .unwrap();

            let default_br = if default_target == instruction.bytecode_index + 1 {
                0
            } else {
                t.br_offset_absolute(instruction.bytecode_index, default_target) + 1
            };

            let branches: Vec<u32> = offsets.iter().map(|offset| {
                let target = *t.byte_to_index_map
                    .get(&instruction.bytes_index.saturating_add_signed(*offset))
                    .unwrap();

                if target == instruction.bytecode_index + 1 {
                    0
                } else {
                    t.br_offset_absolute(instruction.bytecode_index, target) + 1
                }
            }).collect();

            i!(Instr::Block(BlockType::FunctionType(t.module.i32_in_func_type)));

            i!(Instr::I32Const(low));
            i!(Instr::I32Sub);
            i!(Instr::BrTable(Cow::Owned(branches), default_br));

            i!(Instr::End);
        }
        Bytecode::Athrow => {
            i!(Instr::Unreachable);
        }
        Bytecode::Ifne(offset) => {
            i!(Instr::I32Const(0));
            i!(Instr::I32Ne);
            t.branch(instruction.bytes_index, offset as i32, function, |br| {
                Instr::BrIf(br)
            });
        }
        Bytecode::Iflt(offset) => {
            i!(Instr::I32Const(0));
            i!(Instr::I32LtS);
            t.branch(instruction.bytes_index, offset as i32, function, |br| {
                Instr::BrIf(br)
            });
        }
        Bytecode::I2l => {
            i!(Instr::I64ExtendI32S);
        }
        Bytecode::I2b => {
            i!(Instr::I32Const(0xff));
            i!(Instr::I32And);
            i!(Instr::I32Extend8S);
        }
        Bytecode::Iand => {
            i!(Instr::I32And);
        }
        Bytecode::Ixor => {
            i!(Instr::I32Xor);
        }
        Bytecode::Ior => {
            i!(Instr::I32Or);
        }
        Bytecode::Lshl => {
            i!(Instr::I64ExtendI32U);
            i!(Instr::I64Const(0b111111));
            i!(Instr::I64And);
            i!(Instr::I64Shl);
        }
        Bytecode::Lshr => {
            i!(Instr::I64ExtendI32U);
            i!(Instr::I64Const(0b111111));
            i!(Instr::I64And);
            i!(Instr::I64ShrS);
        }
        Bytecode::Ladd => {
            i!(Instr::I64Add);
        }
        Bytecode::Lsub => {
            i!(Instr::I64Sub);
        }
        Bytecode::Lushr => {
            i!(Instr::I64ExtendI32U);
            i!(Instr::I64Const(0b111111));
            i!(Instr::I64And);
            i!(Instr::I64ShrU);
        }
        Bytecode::Ishl => {
            //Pretty sure this doesn't matter
            // i!(Instr::I32Const(0b11111));
            // i!(Instr::I32And);
            i!(Instr::I32Shl);
        }
        Bytecode::Ishr => {
            i!(Instr::I32ShrS);
        }
        Bytecode::Iushr => {
            i!(Instr::I32ShrU);
        }
        Bytecode::Ifeq(offset) => {
            i!(Instr::I32Eqz);
            t.branch(instruction.bytes_index, offset as i32, function, |br| {
                Instr::BrIf(br)
            });
        }
        Bytecode::Ifnull(offset) => {
            i!(Instr::RefIsNull);
            //Doesn't use BrOnNull because that leaves the reference on the stack
            t.branch(instruction.bytes_index, offset as i32, function, |br| {
                Instr::BrIf(br)
            });
        }
        Bytecode::Ifnonnull(offset) => {
            i!(Instr::RefIsNull);
            i!(Instr::I32Eqz);
            //Doesn't use BrOnNull because that leaves the reference on the stack
            t.branch(instruction.bytes_index, offset as i32, function, |br| {
                Instr::BrIf(br)
            });
        }
        Bytecode::Aconst_null => {
            i!(Instr::RefNull(HeapType::Any));
        }
        Bytecode::Checkcast(constant_index) => {
            let constant = t.class.constant_pool.constants.get(&constant_index).unwrap();

            let (classpath, depth) = if let Constant::Class { path, depth } = constant {
                (path, *depth)
            } else {
                panic!()
            };

            let component_class = t.class.class_loader.find_class(t.module.thread, classpath).unwrap();
            let type_class = t.module.jvm.find_array_class(t.module.thread, component_class, depth);

            i!(Instr::LocalTee(t.locals.ref_accumulator));

            i!(Instr::Block(BlockType::FunctionType(t.module.ref_in_func_type)));
            i!(Instr::BrOnNull(0));
            i!(Instr::RefCastNullable(HeapType::Concrete(t.module.object_type)));
            i!(Instr::StructGet { struct_type_index: t.module.object_type, field_index: 0 });
            i!(Instr::I32Const(&*type_class as *const Class as i32));
            i!(Instr::Call(t.module.jvm_functions._jit_routine_checkcast));
            i!(Instr::BrIf(0));

            //TODO exception
            i!(Instr::Unreachable);

            i!(Instr::End);
            i!(Instr::LocalGet(t.locals.ref_accumulator));
        }
        Bytecode::Dup_x1 => {
            let stack = &t.analyzed_stack[instruction.bytecode_index as usize - 1];
            let a = stack[stack.len() - 1];
            let b = stack[stack.len() - 2];

            let a_local;
            let b_local;

            if a == b {
                match a {
                    Ref => {
                        a_local = t.locals.ref_accumulator;
                        b_local = t.locals.ref_accumulator_2;
                    }
                    Class1 => {
                        a_local = t.locals.i32_accumulator;
                        b_local = t.locals.i32_accumulator_2;
                    }
                    Class2 => {
                        a_local = t.locals.i64_accumulator;
                        b_local = t.locals.i64_accumulator_2;
                    }
                }
            } else {
                a_local = match a {
                    Ref => t.locals.ref_accumulator,
                    Class1 => t.locals.i32_accumulator,
                    Class2 => t.locals.i64_accumulator
                };
                b_local = match b {
                    Ref => t.locals.ref_accumulator,
                    Class1 => t.locals.i32_accumulator,
                    Class2 => t.locals.i64_accumulator
                };
            }

            i!(Instr::LocalSet(a_local));
            i!(Instr::LocalSet(b_local));
            i!(Instr::LocalGet(a_local));
            i!(Instr::LocalGet(b_local));
            i!(Instr::LocalGet(a_local));
        }
        Bytecode::Instanceof(constant_index) => {
            let constant = t.class.constant_pool.constants.get(&constant_index).unwrap();

            let (classpath, depth) = if let Constant::Class { path, depth } = constant {
                (path, *depth)
            } else {
                panic!()
            };

            let component_class = t.class.class_loader.find_class(t.module.thread, classpath).unwrap();
            let type_class = t.module.jvm.find_array_class(t.module.thread, component_class, depth);

            i!(Instr::Block(BlockType::FunctionType(t.module.ref_in_i32_out_func_type)));
            i!(Instr::Block(BlockType::FunctionType(t.module.ref_in_func_type)));

            i!(Instr::BrOnNull(0));
            i!(Instr::RefCastNullable(HeapType::Concrete(t.module.object_type)));
            i!(Instr::StructGet { struct_type_index: t.module.object_type, field_index: 0 });
            i!(Instr::I32Const(&*type_class as *const Class as i32));
            i!(Instr::Call(t.module.jvm_functions._jit_routine_checkcast));
            i!(Instr::Br(1));

            i!(Instr::End);

            i!(Instr::I32Const(0));

            i!(Instr::End);
        }
        Bytecode::If_acmpne(offset) => {
            i!(Instr::LocalSet(t.locals.ref_accumulator));
            i!(Instr::RefCastNullable(HeapType::Eq));
            i!(Instr::LocalGet(t.locals.ref_accumulator));
            i!(Instr::RefCastNullable(HeapType::Eq));

            i!(Instr::RefEq);
            i!(Instr::I32Eqz);
            t.branch(instruction.bytes_index, offset as i32, function, |br| {
                Instr::BrIf(br)
            });
        }
        Bytecode::If_acmpeq(offset) => {
            i!(Instr::LocalSet(t.locals.ref_accumulator));
            i!(Instr::RefCastNullable(HeapType::Eq));
            i!(Instr::LocalGet(t.locals.ref_accumulator));
            i!(Instr::RefCastNullable(HeapType::Eq));

            i!(Instr::RefEq);
            t.branch(instruction.bytes_index, offset as i32, function, |br| {
                Instr::BrIf(br)
            });
        }
        Bytecode::If_icmpge(offset) => {
            i!(Instr::I32GeS);
            t.branch(instruction.bytes_index, offset as i32, function, |br| {
                Instr::BrIf(br)
            });
        }
        Bytecode::If_icmpgt(offset) => {
            i!(Instr::I32GtS);
            t.branch(instruction.bytes_index, offset as i32, function, |br| {
                Instr::BrIf(br)
            });
        }
        Bytecode::If_icmple(offset) => {
            i!(Instr::I32LeS);
            t.branch(instruction.bytes_index, offset as i32, function, |br| {
                Instr::BrIf(br)
            });
        }
        Bytecode::If_icmplt(offset) => {
            i!(Instr::I32LtS);
            t.branch(instruction.bytes_index, offset as i32, function, |br| {
                Instr::BrIf(br)
            });
        }
        Bytecode::If_icmpeq(offset) => {
            i!(Instr::I32Eq);
            t.branch(instruction.bytes_index, offset as i32, function, |br| {
                Instr::BrIf(br)
            });
        }
        Bytecode::If_icmpne(offset) => {
            i!(Instr::I32Ne);
            t.branch(instruction.bytes_index, offset as i32, function, |br| {
                Instr::BrIf(br)
            });
        }
        Bytecode::Irem => {
            i!(Instr::I32RemS);
        }
        Bytecode::Iadd => {
            i!(Instr::I32Add);
        }
        Bytecode::Isub => {
            i!(Instr::I32Sub);
        }
        Bytecode::Imul => {
            i!(Instr::I32Mul);
        }
        Bytecode::Iinc(index, amount) => {
            i!(Instr::LocalGet(
                t.local_map.get(index as u32, LocalType::I32)
            ));
            i!(Instr::I32Const(amount as i32));
            i!(Instr::I32Add);
            i!(Instr::LocalSet(
                t.local_map.get(index as u32, LocalType::I32)
            ));
        }
        Bytecode::Dup => {
            let operand_type = &t.analyzed_stack[instruction.bytecode_index as usize - 1];

            let accumulator = match operand_type.last().unwrap() {
                Ref => t.locals.ref_accumulator,
                Class1 => t.locals.i32_accumulator,
                Class2 => t.locals.i64_accumulator,
            };

            i!(Instr::LocalTee(accumulator));
            i!(Instr::LocalGet(accumulator));
        }
        Bytecode::Invokevirtual(constant_index) => {
            let method_ref = t
                .class
                .constant_pool
                .constants
                .get(&constant_index)
                .unwrap()
                .as_ref()
                .unwrap();

            let classpath = &**method_ref.class;

            let method_ref_class = t
                .class
                .class_loader
                .find_class(t.module.thread, classpath)
                .unwrap();

            let resolved_method =
                routine_resolve_method(&t.module.thread, &method_ref, &method_ref_class).unwrap();
            let method_ty = t
                .module
                .get_method_descriptor_id(&resolved_method.descriptor, false);

            i!(Instr::LocalGet(t.method_signature.thread_local));

            i!(Instr::GlobalGet(0));
            i!(Instr::LocalTee(t.locals.i32_accumulator));
            i!(Instr::I32Const(2));
            i!(Instr::I32Shl);
            i!(Instr::I32Load(MemArg {
                align: 2,
                offset: 0,
                memory_index: 1
            }));

            i!(Instr::LocalGet(t.locals.i32_accumulator));
            i!(Instr::I32Const(1));
            i!(Instr::I32Sub);
            i!(Instr::GlobalSet(0));

            i!(Instr::CallIndirect {
                ty: method_ty,
                table: 0
            });
        }
        Bytecode::Invokespecial(constant_index) => {
            let ref_ = t
                .class
                .constant_pool
                .constants
                .get(&constant_index)
                .unwrap()
                .as_ref()
                .unwrap();

            let classpath = &ref_.class;
            let target_class = t
                .class
                .class_loader
                .find_class(t.module.thread, classpath)
                .unwrap();

            let method = target_class.get_method(&ref_.name_and_type).unwrap();

            let c = if {
                !method.is_init(classpath, t.module.thread)
                    && !target_class.access_flags.contains(AccessFlags::INTERFACE)
                    && JVM::is_subclass(&t.class, &target_class)
                    && target_class.access_flags.contains(AccessFlags::SUPER)
            } {
                &**t.class.super_class.as_ref().unwrap()
            } else {
                &target_class
            };

            i!(Instr::I32Const(c as *const Class as i32));
            i!(Instr::LocalGet(t.method_signature.thread_local));
            i!(Instr::Call(t.module.jvm_functions.init_static));

            let method = match c.get_method(&ref_.name_and_type) {
                None => todo!(),
                Some(method) => method,
            };

            if method.access_flags.contains(AccessFlags::SYNCHRONIZED)
                | method.access_flags.contains(AccessFlags::NATIVE)
            {
                todo!()
            }

            let ty = t.module.get_method_descriptor_id(
                &method.descriptor,
                method.access_flags.contains(AccessFlags::STATIC),
            );

            let func_id = t
                .module
                .env
                .get_linked_function_ptr(&c, method.get_identifier());

            i!(Instr::LocalGet(t.method_signature.thread_local));
            i!(Instr::I32Const(func_id as i32));
            i!(Instr::CallIndirect { ty, table: 0 });
        }
        Bytecode::Arraylength => {
            i!(Instr::RefCastNullable(HeapType::Concrete(
                t.module.array_object_type
            )));
            i!(Instr::StructGet {
                struct_type_index: t.module.array_object_type,
                field_index: 2
            });
            i!(Instr::ArrayLen);
        }
        Bytecode::Getstatic(constant_index) => {
            let constant = t
                .class
                .constant_pool
                .constants
                .get(&constant_index)
                .unwrap();

            let field_ref = constant.as_ref().unwrap();
            let classpath = &field_ref.class;
            let target_class = t
                .class
                .class_loader
                .find_class(t.module.thread, classpath)
                .unwrap();

            let (_, resolved_class) =
                routine_resolve_field(&field_ref.name_and_type, &target_class).unwrap();

            if resolved_class.this_class != t.class.this_class {
                if !resolved_class.is_initialized() {}
            }

            let (index, field) = resolved_class
                .fields
                .iter()
                .filter(|field| field.access_flags.contains(AccessFlags::STATIC))
                .enumerate()
                .find(|(_, field)| field.name == field_ref.name_and_type.name)
                .unwrap();

            unsafe {
                i!(Instr::I32Const(
                    resolved_class.statics.as_mut_ptr().add(index) as i32
                ));
            }

            //TODO atomic statics
            match field.descriptor {
                FieldType::Byte => {
                    i!(Instr::I32Load8U(MemArg {
                        memory_index: 0,
                        offset: 0,
                        align: 0
                    }));
                }
                FieldType::Char => {
                    i!(Instr::I32Load16U(MemArg {
                        memory_index: 0,
                        offset: 0,
                        align: 1
                    }));
                }
                FieldType::Float => {
                    i!(Instr::F32Load(MemArg {
                        memory_index: 0,
                        offset: 0,
                        align: 2
                    }));
                }
                FieldType::Int => {
                    i!(Instr::I32Load(MemArg {
                        memory_index: 0,
                        offset: 0,
                        align: 2
                    }));
                }
                FieldType::Short => {
                    i!(Instr::I32Load16S(MemArg {
                        memory_index: 0,
                        offset: 0,
                        align: 1
                    }));
                }
                FieldType::Boolean => {
                    i!(Instr::I32Load8U(MemArg {
                        memory_index: 0,
                        offset: 0,
                        align: 0
                    }));
                }
                FieldType::Double
                | FieldType::Long => {
                    i!(Instr::I64Load(MemArg {
                        memory_index: 0,
                        offset: 0,
                        align: 3
                    }));
                }
                FieldType::Class(_) | FieldType::Array { .. } => {
                    i!(Instr::I32Load(MemArg {
                        memory_index: 0,
                        offset: 0,
                        align: 2
                    }));
                    i!(Instr::TableGet(1));
                }
            }
        }
        Bytecode::Pop => {
            i!(Instr::Drop);
        }
        Bytecode::Newarray(array_type) => {
            let (storage_type, classpath) = match array_type {
                ArrayType::Bool => (StorageType::I8, "[Z"),
                ArrayType::Char => (StorageType::I16, "[C"),
                ArrayType::Float => (StorageType::Val(ValType::F32), "[F"),
                ArrayType::Double => (StorageType::Val(ValType::F64), "[D"),
                ArrayType::Byte => (StorageType::I8, "[B"),
                ArrayType::Short => (StorageType::I16, "[S"),
                ArrayType::Int => (StorageType::Val(ValType::I32), "[I"),
                ArrayType::Long => (StorageType::Val(ValType::I64), "[J"),
            };

            let array_type_index = t.module.array_storage_types.get(&storage_type).unwrap();

            let array_class = t.module.jvm.fetch_linked_class(classpath, 0).unwrap();
            i!(Instr::ArrayNewDefault(*array_type_index));
            i!(Instr::LocalSet(t.locals.ref_accumulator));

            i!(Instr::I32Const(&*array_class as *const Class as i32));

            let env = (*t.module.jvm.environment)
                .to_any()
                .downcast_ref::<WasmEnvironment>()
                .unwrap();
            let object_id_ptr = env.object_id.as_ptr();

            i!(Instr::I32Const(object_id_ptr as i32));
            i!(Instr::I64Const(1));
            i!(Instr::I64AtomicRmwAdd(MemArg {
                offset: 0,
                align: 3,
                memory_index: 0
            }));

            i!(Instr::LocalGet(t.locals.ref_accumulator));
            i!(Instr::RefCastNonNull(HeapType::Array));

            i!(Instr::StructNew(t.module.array_object_type));
        }
        Bytecode::Baload | Bytecode::Iaload | Bytecode::Aaload => {
            let array_object_type = t.module.array_object_type;

            let storage_type = match instruction.bytecode {
                Bytecode::Baload => StorageType::I8,
                Bytecode::Iaload => StorageType::Val(ValType::I32),
                Bytecode::Aaload => StorageType::Val(ValType::Ref(RefType { nullable: true, heap_type: HeapType::Struct })),
                _ => unreachable!()
            };

            let array_type = *t.module.array_storage_types.get(&storage_type).unwrap();

            //TODO use a splice
            i!(Instr::LocalSet(t.locals.i32_accumulator));
            i!(Instr::RefCastNullable(HeapType::Concrete(array_object_type)));
            i!(Instr::StructGet {
                struct_type_index: array_object_type,
                field_index: 2
            });
            i!(Instr::RefCastNullable(HeapType::Concrete(array_type)));
            i!(Instr::LocalGet(t.locals.i32_accumulator));

            match instruction.bytecode {
                Bytecode::Baload => { i!(Instr::ArrayGetU(array_type)); },
                Bytecode::Aaload
                | Bytecode::Iaload => { i!(Instr::ArrayGet(array_type)); },
                _ => unreachable!()
            };
        }
        Bytecode::Aastore => {
            let &object_array = t.module.array_storage_types.get(&StorageType::Val(ValType::Ref(RefType { nullable: true, heap_type: HeapType::Struct }))).unwrap();

            i!(Instr::RefCastNullable(HeapType::Struct));
            i!(Instr::ArraySet(object_array));
        }
        Bytecode::Bastore => {
            let &byte_array = t.module.array_storage_types.get(&StorageType::I8).unwrap();

            i!(Instr::ArraySet(byte_array));
        }
        Bytecode::Iastore => {
            let &int_array = t.module.array_storage_types.get(&StorageType::Val(ValType::I32)).unwrap();

            i!(Instr::ArraySet(int_array));
        }
        Bytecode::New(constant_index) => {
            let constant = t
                .class
                .constant_pool
                .constants
                .get(&constant_index)
                .unwrap();

            if let Constant::Class { path, .. } = constant {
                let target_class = t
                    .module
                    .jvm
                    .retrieve_class(path, &*t.class.class_loader, t.module.thread)
                    .unwrap();

                if !target_class.is_initialized() {
                    i!(Instr::I32Const(t.class as *const Class as i32));
                    i!(Instr::LocalGet(t.method_signature.thread_local));
                    i!(Instr::Call(t.module.jvm_functions.init_static));
                }

                let class_type_id = t.module.get_object_type(&target_class);

                i!(Instr::StructNewDefault(class_type_id));
                i!(Instr::LocalTee(t.locals.ref_accumulator));
                i!(Instr::RefCastNullable(HeapType::Concrete(class_type_id)));
                i!(Instr::I32Const(&*target_class as *const Class as i32));
                i!(Instr::StructSet {
                    struct_type_index: class_type_id,
                    field_index: 0,
                });

                i!(Instr::LocalGet(t.locals.ref_accumulator));
            } else {
                panic!()
            }
        }
        Bytecode::Putfield(constant_index) => {
            let constant = t
                .class
                .constant_pool
                .constants
                .get(&constant_index)
                .unwrap();

            let field_ref = constant.as_ref().unwrap();

            let classpath = &field_ref.class;
            let target_class = t
                .module
                .jvm
                .retrieve_class(classpath, &*t.class.class_loader, t.module.thread)
                .unwrap();

            let (field, field_class) =
                routine_resolve_field(&field_ref.name_and_type, &target_class).unwrap();

            let field_id_offset = field_class
                .parents()
                .iter()
                .map(|parent| {
                    parent
                        .fields
                        .iter()
                        .filter(|field| !field.access_flags.contains(AccessFlags::STATIC))
                })
                .flatten()
                .count() as u32
                + 2;

            let (field_id, _) = field_class
                .fields
                .iter()
                .filter(|field| !field.access_flags.contains(AccessFlags::STATIC))
                .enumerate()
                .find(|(_, test_field)| test_field.name == field.name)
                .unwrap();

            let struct_type = t.module.get_object_type(field_class);

            let accumulator = match &field.descriptor {
                FieldType::Array { .. } | FieldType::Class(_) => t.locals.ref_accumulator,
                FieldType::Double => todo!(),
                FieldType::Long => t.locals.i64_accumulator,
                _ => t.locals.i32_accumulator,
            };

            i!(Instr::LocalSet(accumulator));
            i!(Instr::RefCastNullable(HeapType::Concrete(struct_type)));
            i!(Instr::LocalGet(accumulator));

            i!(Instr::StructSet {
                struct_type_index: struct_type,
                field_index: field_id as u32 + field_id_offset,
            });
        }
        Bytecode::Getfield(constant_index) => {
            let constant = t
                .class
                .constant_pool
                .constants
                .get(&constant_index)
                .unwrap();

            let field_ref = constant.as_ref().unwrap();

            let classpath = &field_ref.class;
            let target_class = t
                .module
                .jvm
                .retrieve_class(classpath, &*t.class.class_loader, t.module.thread)
                .unwrap();

            let (field, field_class) =
                routine_resolve_field(&field_ref.name_and_type, &target_class).unwrap();

            let field_id_offset = field_class
                .parents()
                .iter()
                .map(|parent| {
                    parent
                        .fields
                        .iter()
                        .filter(|field| !field.access_flags.contains(AccessFlags::STATIC))
                })
                .flatten()
                .count() as u32
                + 2;

            let (field_id, _) = field_class
                .fields
                .iter()
                .filter(|field| !field.access_flags.contains(AccessFlags::STATIC))
                .enumerate()
                .find(|(_, test_field)| test_field.name == field.name)
                .unwrap();

            let struct_type = t.module.get_object_type(field_class);

            i!(Instr::RefCastNullable(HeapType::Concrete(struct_type)));

            i!(Instr::StructGet {
                struct_type_index: struct_type,
                field_index: field_id as u32 + field_id_offset,
            });
        }
        Bytecode::Invokestatic(constant_index) => {
            let ref_ = t
                .class
                .constant_pool
                .constants
                .get(&constant_index)
                .unwrap()
                .as_ref()
                .unwrap();

            let method_class = t
                .class
                .class_loader
                .find_class(t.module.thread, &ref_.class)
                .unwrap();

            if !t.class.is_initialized() {
                i!(Instr::I32Const(t.class as *const Class as i32));
                i!(Instr::LocalGet(t.method_signature.thread_local));
                i!(Instr::Call(t.module.jvm_functions.init_static));
            }

            let method = method_class.get_method(&ref_.name_and_type).unwrap();
            let ty = t.module.get_method_descriptor_id(&method.descriptor, true);

            let func_id = t
                .module
                .env
                .get_linked_function_ptr(&method_class, method.get_identifier());

            i!(Instr::LocalGet(t.method_signature.thread_local));
            i!(Instr::I32Const(func_id as i32));
            i!(Instr::CallIndirect { ty, table: 0 });
        }
        Bytecode::Monitorenter | Bytecode::Monitorexit => {
            i!(Instr::Drop);
        }
        Bytecode::Ldc(constant_index) => {
            let constant = t
                .class
                .constant_pool
                .constants
                .get(&(constant_index as u16))
                .unwrap();

            match constant {
                Constant::Integer(int) => {
                    i!(Instr::I32Const(*int));
                }
                Constant::Class { path, depth } => {
                    if *depth == 0 {
                        let class = t
                            .class
                            .class_loader
                            .find_class(t.module.thread, path)
                            .unwrap();
                        let ptr = class.class_instance.get().unwrap().ptr;

                        let table_index = ptr as u32;

                        i!(Instr::I32Const(table_index as i32));
                        i!(Instr::TableGet(1));
                    } else {
                        let class = t
                            .class
                            .class_loader
                            .find_class(t.module.thread, path)
                            .unwrap();
                        let class = t
                            .module
                            .jvm
                            .find_array_class(t.module.thread, class, *depth);
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
        Bytecode::Lconst_n(long) => {
            i!(Instr::I64Const(long as i64));
        }
        Bytecode::Sipush(short) => {
            i!(Instr::I32Const(short as i32));
        }
        Bytecode::Bipush(byte) => {
            i!(Instr::I32Const(byte as i32));
        }
        Bytecode::Istore(index) => {
            let local = t.local_map.get(index as u32, LocalType::I32);
            i!(Instr::LocalSet(local));
        }
        Bytecode::Lstore(index) => {
            let local = t.local_map.get(index as u32, LocalType::I64);
            i!(Instr::LocalSet(local));
        }
        Bytecode::Astore(index) => {
            let local = t.local_map.get(index as u32, LocalType::Ref);
            i!(Instr::LocalSet(local));
        }
        Bytecode::Iload(index) => {
            let local = t.local_map.get(index as u32, LocalType::I32);
            i!(Instr::LocalGet(local));
        }
        Bytecode::Aload(index) => {
            let local = t.local_map.get(index as u32, LocalType::Ref);
            i!(Instr::LocalGet(local));
        }
        Bytecode::Lload(index) => {
            let local = t.local_map.get(index as u32, LocalType::I64);
            i!(Instr::LocalGet(local));
        }
        Bytecode::Ireturn => {
            i!(Instr::Return);
        }
        Bytecode::Return => {
            i!(Instr::Return);
        }
        Bytecode::Areturn => {
            i!(Instr::Return);
        }
        // _ => { i!(Instr::Nop); }
        // _ => unimplemented!("{:?}", instruction.bytecode),
        _ => return Err(JITError::UnimplementedBytecode(instruction.bytecode.clone()))
    }
    
    Ok(())
}

#[derive(Debug)]
struct LocalMap {
    java_to_wasm_locals: HashMap<(u32, LocalType), u32>,
    wasm_locals: Vec<ValType>,
}

impl LocalMap {
    fn get(&self, java_local: u32, local_type: LocalType) -> u32 {
        self.java_to_wasm_locals
            .get(&(java_local, local_type))
            .copied()
            .unwrap()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum LocalType {
    I32,
    I64,
    F32,
    F64,
    Ref,
}

fn analyze_locals(method_signature: &MethodSignature, instructions: &[Instruction]) -> LocalMap {
    let mut locals = HashMap::new();
    let mut rewrites = HashSet::new();

    for index in 0..method_signature.params.len() - 1 {
        let arg = method_signature.params[index];
        let local_type = match arg {
            ValType::I32 => LocalType::I32,
            ValType::I64 => LocalType::I64,
            ValType::F32 => LocalType::F32,
            ValType::F64 => LocalType::F64,
            ValType::Ref(_) => LocalType::Ref,
            _ => unreachable!(),
        };
        locals.insert((index as u32, local_type), arg);
    }

    for instruction in instructions {
        match instruction.bytecode {
            Bytecode::Iload(index) | Bytecode::Istore(index) => {
                if locals.iter().any(|((e_index, local), _)| {
                    *e_index == index as u32 && *local != LocalType::I32
                }) {
                    rewrites.insert((index as u32, LocalType::I32));
                } else {
                    locals.insert((index as u32, LocalType::I32), ValType::I32);
                }
            }
            Bytecode::Lload(index) => {
                if locals.iter().any(|((e_index, local), _)| {
                    *e_index == index as u32 && *local != LocalType::I64
                }) {
                    rewrites.insert((index as u32, LocalType::I64));
                } else {
                    locals.insert((index as u32, LocalType::I64), ValType::I64);
                }
            }
            Bytecode::Aload(index) | Bytecode::Astore(index) => {
                if locals.iter().any(|((e_index, local), _)| {
                    *e_index == index as u32 && *local != LocalType::Ref
                }) {
                    rewrites.insert((index as u32, LocalType::Ref));
                } else {
                    locals.insert(
                        (index as u32, LocalType::Ref),
                        ValType::Ref(RefType {
                            nullable: true,
                            heap_type: HeapType::Any,
                        }),
                    );
                }
            }
            Bytecode::Fstore(index) => {
                if locals.iter().any(|((e_index, local), _)| {
                    *e_index == index as u32 && *local != LocalType::F32
                }) {
                    rewrites.insert((index as u32, LocalType::F32));
                } else {
                    locals.insert((index as u32, LocalType::F32), ValType::F32);
                }
            }
            Bytecode::Dstore(index) => {
                if locals.iter().any(|((e_index, local), _)| {
                    *e_index == index as u32 && *local != LocalType::F64
                }) {
                    rewrites.insert((index as u32, LocalType::F64));
                } else {
                    locals.insert((index as u32, LocalType::F64), ValType::F64);
                }
            }
            _ => {}
        }
    }

    let mut java_locals: Vec<((u32, LocalType), ValType)> = locals.into_iter().collect();
    java_locals.sort_by_key(|local| local.0 .0);

    let mut map = HashMap::new();

    let thread_local_index = method_signature.params.len() - 1;

    let mut non_argument_local_index = 0;

    for (java_local, _) in java_locals.iter() {
        if java_local.0 >= thread_local_index as u32 {
            //Dealing with a local that isn't part of the original method descriptor
            map.insert(
                *java_local,
                method_signature.params.len() as u32 + non_argument_local_index,
            );
            non_argument_local_index += 1;
        } else {
            //A local from the method signature
            map.insert(*java_local, java_local.0);
        }
    }

    let highest_mapped_local = map.iter().map(|((_, _), index)| *index).max().unwrap_or(0);
    let mut rewrite_index = (method_signature.params.len() as u32).max(highest_mapped_local + 1);

    let mut rewritten = vec![];

    for (java_local_index, local_type) in rewrites {
        map.insert((java_local_index, local_type), rewrite_index);
        rewritten.push(match local_type {
            LocalType::I32 => ValType::I32,
            LocalType::I64 => ValType::I64,
            LocalType::F32 => ValType::F32,
            LocalType::F64 => ValType::F64,
            LocalType::Ref => ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Any,
            }),
        });
        rewrite_index += 1;
    }

    let wasm_locals = java_locals
        .iter()
        .filter(|((index, _), _)| *index >= thread_local_index as u32)
        .map(|(_, val_type)| *val_type)
        .chain(rewritten)
        .collect();

    LocalMap {
        java_to_wasm_locals: map,
        wasm_locals,
    }
}

fn fold<'method, 'module: 'method, 'a, 'b: 'a>(
    instructions: &'method [Instruction],
    ranges: &'a mut &'b [(u32, u32, BlockRangeType)],
    function: &mut Function,
    absolute_offset: usize,
    translating: &mut MethodTranslation<'method, 'module>,
) -> Result<usize, JITError> {
    if instructions.len() == 0 {
        return Ok(0);
    }

    let mut consumed_instructions = 0;

    loop {
        let (next_block_start, next_block_end, block_type) = match ranges.get(0).copied() {
            None => {
                for instr in &instructions[consumed_instructions..] {
                    translate_instructions(instr, function, translating)?;
                }

                return Ok(instructions.len());
            }
            Some(range) => {
                //Next block is not nested in the current one
                if range.0 as usize > instructions.len() + absolute_offset - 1 {
                    for instr in &instructions[consumed_instructions..] {
                        translate_instructions(instr, function, translating)?;
                    }

                    return Ok(instructions.len());
                }

                range
            }
        };

        let instrs_to_write = next_block_start as usize - absolute_offset - consumed_instructions;

        for index in 0..instrs_to_write {
            translate_instructions(
                &instructions[index + consumed_instructions],
                function,
                translating,
            )?;
        }

        // console_log!("written: {instrs_to_write}, next_block_start: {next_block_start}, absolute: {absolute_offset}, n-a: {}, consumed: {consumed_instructions}", next_block_start as usize - absolute_offset);

        consumed_instructions += instrs_to_write;

        match block_type {
            BlockRangeType::Block => {
                let block_type = if let Some(instr) = instructions
                    .get(next_block_start as usize - absolute_offset)
                    .map(|instr| &instr.bytecode)
                {
                    let params_in: &[ValType] = match instr {
                        Bytecode::Ifeq(_)
                        | Bytecode::Ifne(_)
                        | Bytecode::Ifge(_)
                        | Bytecode::Ifgt(_)
                        | Bytecode::Ifle(_)
                        | Bytecode::Iflt(_) => {
                            &[ValType::I32]
                        }
                        Bytecode::If_icmpge(_)
                        | Bytecode::If_icmpeq(_)
                        | Bytecode::If_icmpgt(_)
                        | Bytecode::If_icmple(_)
                        | Bytecode::If_icmpne(_)
                        | Bytecode::If_icmplt(_) => {
                            &[ValType::I32; 2]
                        }
                        Bytecode::Ifnull(_) | Bytecode::Ifnonnull(_) => {
                            &[ValType::Ref(RefType { heap_type: HeapType::Any, nullable: true })]
                        }
                        Bytecode::If_acmpne(_) | Bytecode::If_acmpeq(_) => {
                            &[ValType::Ref(RefType { heap_type: HeapType::Any, nullable: true }); 2]
                        }
                        Bytecode::Lookupswitch(_, _) | Bytecode::Tableswitch { .. } => {
                            &[ValType::I32]
                        }
                        _ => &[],
                    };

                    let params_out: Vec<ValType> = translating.analyzed_stack[next_block_end as usize].iter().map(|operand| match operand {
                        Ref => ValType::Ref(RefType { heap_type: HeapType::Any, nullable: true }),
                        Class1 => ValType::I32,
                        Class2 => ValType::I64
                    }).collect();

                    let type_id = translating.module.type_section.len();
                    translating.module.type_section.function(params_in.to_owned(), params_out);

                    BlockType::FunctionType(type_id)
                } else {
                    BlockType::Empty
                };

                function.instruction(&Instr::Block(block_type));
            }
            BlockRangeType::Loop => {
                function.instruction(&Instr::Loop(BlockType::Empty));
            }
        }

        *ranges = &ranges[1..];

        consumed_instructions += fold(
            &instructions[next_block_start as usize - absolute_offset
                ..next_block_end as usize - absolute_offset + 1],
            ranges,
            function,
            next_block_start as usize,
            translating,
        )?;

        function.instruction(&Instr::End);
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum OperandType {
    Ref,
    Class1, //anything 32-bits or smaller
    Class2, //longs and doubles
}

fn analyze_stack(
    bytecode: &[Instruction],
    class: &Class,
    thread: &mut Thread,
) -> Result<Vec<Vec<OperandType>>, JITError> {
    let mut stack = vec![];
    let mut out = vec![];

    for instr in bytecode {
        let (change, res): (isize, &[OperandType]) = match &instr.bytecode {
            Bytecode::Aaload => (-2, &[Ref]),
            Bytecode::Aastore => (-3, &[]),
            Bytecode::Aconst_null => (0, &[Ref]),
            Bytecode::Aload(_) => (0, &[Ref]),
            Bytecode::Anewarray(_) => (-1, &[Ref]),
            Bytecode::Areturn => (-1, &[]),
            Bytecode::Arraylength => (-1, &[Class1]),
            Bytecode::Astore(_) => (-1, &[]),
            Bytecode::Athrow => (-1, &[]),
            Bytecode::Baload => (-2, &[Class1]),
            Bytecode::Bastore => (-3, &[]),
            Bytecode::Bipush(_) => (1, &[Class1]),
            Bytecode::Caload => (-2, &[Class1]),
            Bytecode::Castore => (-3, &[]),
            Bytecode::Checkcast(_) => (-1, &[]),
            Bytecode::D2f => (-1, &[Class1]),
            Bytecode::D2i => (-1, &[Class1]),
            Bytecode::D2l => (-1, &[Class1]),
            Bytecode::Dadd => (-1, &[Class2]),
            Bytecode::Daload => (-2, &[Class1]),
            Bytecode::Dastore => (-3, &[]),
            Bytecode::Dcmpg => (-2, &[Class1]),
            Bytecode::Dcmpl => (-2, &[Class1]),
            Bytecode::Dconst_n(_) => (0, &[Class2]),
            Bytecode::Ddiv => (-2, &[Class2]),
            Bytecode::Dload(_) => (0, &[Class2]),
            Bytecode::Dmul => (-2, &[Class2]),
            Bytecode::Dneg => (-1, &[Class2]),
            Bytecode::Drem => (-2, &[Class2]),
            Bytecode::Dreturn => (-1, &[]),
            Bytecode::Dstore(_) => (-1, &[]),
            Bytecode::Dsub => (-2, &[Class2]),
            Bytecode::Dup => (-1, &[*stack.last().unwrap(), *stack.last().unwrap()]),
            Bytecode::Dup_x2 => return Err(JITError::UnimplementedBytecode(instr.bytecode.clone())),
            Bytecode::Dup2 => return Err(JITError::UnimplementedBytecode(instr.bytecode.clone())),
            Bytecode::Dup2_x1 => return Err(JITError::UnimplementedBytecode(instr.bytecode.clone())),
            Bytecode::Dup2_x2 => return Err(JITError::UnimplementedBytecode(instr.bytecode.clone())),
            Bytecode::F2d => (-1, &[Class2]),
            Bytecode::F2i => (-1, &[Class1]),
            Bytecode::F2l => (-1, &[Class2]),
            Bytecode::Fadd => (-2, &[Class1]),
            Bytecode::Faload => (-2, &[Class1]),
            Bytecode::Fastore => (-3, &[]),
            Bytecode::Fcmpg => (-2, &[Class1]),
            Bytecode::Fcmpl => (-2, &[Class1]),
            Bytecode::Fconst_n(_) => (0, &[Class1]),
            Bytecode::Fdiv => (-2, &[Class1]),
            Bytecode::Fload(_) => (0, &[Class1]),
            Bytecode::Fmul => (-2, &[Class1]),
            Bytecode::Fneg => (-1, &[Class1]),
            Bytecode::Frem => (-2, &[Class1]),
            Bytecode::Freturn => (-1, &[]),
            Bytecode::Fstore(_) => (-1, &[]),
            Bytecode::Fsub => (-2, &[Class1]),
            Bytecode::Getfield(constant_index) => {
                let constant = class.constant_pool.constants.get(&constant_index).unwrap();

                let field_ref = constant.as_ref().unwrap();

                let classpath = &field_ref.class;
                let target_class = class.class_loader.find_class(thread, classpath).unwrap();
                let (field, _) =
                    routine_resolve_field(&field_ref.name_and_type, &target_class).unwrap();
                let res = match field.descriptor {
                    FieldType::Array { .. } | FieldType::Class(_) => &[Ref],
                    FieldType::Double | FieldType::Long => &[Class2],
                    _ => &[Class1],
                };

                (-1, &res[..])
            }
            Bytecode::Getstatic(constant_index) => {
                let constant = class.constant_pool.constants.get(constant_index).unwrap();

                let field_ref = constant.as_ref().unwrap();
                let classpath = &field_ref.class;
                let target_class = class.class_loader.find_class(thread, classpath).unwrap();

                let (field, _) =
                    routine_resolve_field(&field_ref.name_and_type, &target_class).unwrap();
                let res = match field.descriptor {
                    FieldType::Array { .. } | FieldType::Class(_) => &[Ref],
                    FieldType::Double | FieldType::Long => &[Class2],
                    _ => &[Class1],
                };

                (0, &res[..])
            }
            Bytecode::I2b => (-1, &[Class1]),
            Bytecode::I2c => (-1, &[Class1]),
            Bytecode::I2d => (-1, &[Class1]),
            Bytecode::I2f => (-1, &[Class1]),
            Bytecode::I2l => (-1, &[Class1]),
            Bytecode::I2s => (-1, &[Class1]),
            Bytecode::Iadd => (-2, &[Class1]),
            Bytecode::Iaload => (-2, &[Class1]),
            Bytecode::Iand => (-2, &[Class1]),
            Bytecode::Iastore => (-3, &[]),
            Bytecode::Iconst_n_m1(_) => (0, &[Class1]),
            Bytecode::Idiv => (-2, &[Class1]),
            Bytecode::If_acmpeq(_)
            | Bytecode::If_acmpne(_)
            | Bytecode::If_icmpeq(_)
            | Bytecode::If_icmpne(_)
            | Bytecode::If_icmplt(_)
            | Bytecode::If_icmpge(_)
            | Bytecode::If_icmpgt(_)
            | Bytecode::If_icmple(_) => (-2, &[]),
            Bytecode::Ifeq(_)
            | Bytecode::Ifne(_)
            | Bytecode::Iflt(_)
            | Bytecode::Ifge(_)
            | Bytecode::Ifgt(_)
            | Bytecode::Ifle(_)
            | Bytecode::Ifnonnull(_)
            | Bytecode::Ifnull(_) => (-1, &[]),
            Bytecode::Goto(_) | Bytecode::Goto_w(_) => {
                stack.clear();
                (0, &[])
            }

            Bytecode::Iinc(_, _) => (0, &[]),
            Bytecode::Iload(_) => (0, &[Class1]),
            Bytecode::Imul => (-2, &[Class1]),
            Bytecode::Ineg => (-1, &[Class1]),
            Bytecode::Instanceof(_) => (-1, &[Class1]),
            Bytecode::Invokedynamic(constant_index)
            | Bytecode::Invokeinterface(constant_index, _)
            | Bytecode::Invokespecial(constant_index)
            | Bytecode::Invokevirtual(constant_index)
            | Bytecode::Invokestatic(constant_index) => {
                let method = class
                    .constant_pool
                    .constants
                    .get(constant_index)
                    .unwrap()
                    .as_ref()
                    .unwrap();

                let descriptor =
                    MethodDescriptor::try_from(&**method.name_and_type.descriptor).unwrap();

                let res = match descriptor.return_type {
                    ReturnType::FieldType(FieldType::Array { .. })
                    | ReturnType::FieldType(FieldType::Class(_)) => &[Ref],
                    ReturnType::FieldType(FieldType::Double)
                    | ReturnType::FieldType(FieldType::Long) => &[Class2],
                    ReturnType::FieldType(_) => &[Class1],
                    ReturnType::Void => &[][..],
                };

                let input = if matches!(instr.bytecode, Bytecode::Invokestatic(_)) {
                    descriptor.args.len() as isize
                } else {
                    descriptor.args.len() as isize + 1
                };

                (-input, res)
            }
            Bytecode::Ior => (-2, &[Class1]),
            Bytecode::Irem => (-2, &[Class1]),
            Bytecode::Ireturn => (-1, &[]),
            Bytecode::Ishr => (-2, &[Class1]),
            Bytecode::Ishl => (-2, &[Class1]),
            Bytecode::Istore(_) => (-1, &[]),
            Bytecode::Isub => (-2, &[Class1]),
            Bytecode::Iushr => (-2, &[Class1]),
            Bytecode::Ixor => (-2, &[Class1]),
            Bytecode::Jsr(_) => todo!(),
            Bytecode::Jsr_w(_) => todo!(),
            Bytecode::L2d => (-1, &[Class2]),
            Bytecode::L2f => (-1, &[Class1]),
            Bytecode::L2i => (-1, &[Class1]),
            Bytecode::Ladd => (-2, &[Class2]),
            Bytecode::Laload => (-2, &[Class2]),
            Bytecode::Land => (-2, &[Class2]),
            Bytecode::Lastore => (-3, &[]),
            Bytecode::Lcmp => (-2, &[Class1]),
            Bytecode::Lconst_n(_) => (0, &[Class2]),
            Bytecode::Ldc(constant_index) => {
                let constant = class
                    .constant_pool
                    .constants
                    .get(&(*constant_index as u16))
                    .unwrap();

                let res = match constant {
                    Constant::Class { .. } | Constant::String(_) => &[Ref],
                    Constant::Integer(_) | Constant::Float(_) => &[Class1],
                    Constant::Long(_) | Constant::Double(_) => &[Class2],
                    _ => unimplemented!(),
                };

                (1, res)
            }
            Bytecode::Ldc_w(constant_index) => return Err(JITError::UnimplementedBytecode(instr.bytecode.clone())),
            Bytecode::Ldc2_w(_) => return Err(JITError::UnimplementedBytecode(instr.bytecode.clone())),
            Bytecode::Ldiv => (-2, &[Class2]),
            Bytecode::Lload(_) => (0, &[Class2]),
            Bytecode::Lmul => (-2, &[Class2]),
            Bytecode::Lneg => (-1, &[Class2]),
            Bytecode::Lookupswitch(_, _) => {
                stack.clear();

                (0, &[])
            }
            Bytecode::Lor => (-1, &[Class2]),
            Bytecode::Lrem => (-2, &[Class2]),
            Bytecode::Lreturn => (-1, &[]),
            Bytecode::Lshl => (-2, &[Class2]),
            Bytecode::Lshr => (-2, &[Class2]),
            Bytecode::Lstore(_) => (-1, &[]),
            Bytecode::Lsub => (-2, &[Class2]),
            Bytecode::Lushr => (-2, &[Class2]),
            Bytecode::Lxor => (-2, &[Class2]),
            Bytecode::Monitorenter => (-1, &[]),
            Bytecode::Monitorexit => (-1, &[]),
            Bytecode::Multianewarray(_, dimensions) => (1 - (*dimensions as isize), &[Ref]),
            Bytecode::New(_) => (0, &[Ref]),
            Bytecode::Newarray(_) => (-1, &[Ref]),
            Bytecode::Nop => (0, &[]),
            Bytecode::Pop => (-1, &[]),
            Bytecode::Pop2 => todo!(),
            Bytecode::Putfield(_) => (-2, &[]),
            Bytecode::Putstatic(_) => (-1, &[]),
            Bytecode::Ret(_) => (0, &[]),
            Bytecode::Return => (0, &[]),
            Bytecode::Saload => (-2, &[Class1]),
            Bytecode::Sastore => (-3, &[Class1]),
            Bytecode::Sipush(_) => (0, &[Class1]),
            Bytecode::Swap => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();

                (0, &[a, b])
            }
            Bytecode::Tableswitch { .. } => {
                stack.clear();
                (0, &[])
            }
            Bytecode::Dup_x1 => {
                let a = stack.pop().ok_or(JITError::AnalysisFailure)?;
                let b = stack.pop().ok_or(JITError::AnalysisFailure)?;

                (0, &[b, a, b])
            },
        };

        if change < 0 {
            for _ in 0..change.abs() as usize {
                stack.pop();
            }
        }

        stack.extend(res);

        out.push(stack.clone());
    }

    Ok(out)
}
