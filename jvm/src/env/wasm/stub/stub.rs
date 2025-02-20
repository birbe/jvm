use std::collections::HashMap;
use wasm_encoder::{BlockType, CodeSection, CompositeType, ConstExpr, Elements, ElementSection, EntityType, ExportKind, ExportSection, Function, FunctionSection, HeapType, ImportSection, Instruction, MemArg, MemoryType, Module, RefType, StorageType, StructType, SubType, TableSection, TableType, TypeSection, ValType};
use std::sync::Arc;
use wasmparser::WasmFeatures;
use crate::classfile::resolved::{AccessFlags, Attribute, Class, Field, FieldType, Method, MethodDescriptor, ReturnType};
use crate::thread::{Operand, RawFrame};

#[derive(Debug)]
pub struct MethodSignature {
    pub params: Vec<ValType>,
    pub return_type: Option<ValType>,
    pub thread_local: u32
}

impl MethodSignature {

    fn field_type_to_val_type(field_type: &FieldType) -> ValType {
        match field_type {
            FieldType::Array { .. } => ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Any,
            }),
            FieldType::Byte
            | FieldType::Char
            | FieldType::Int
            | FieldType::Short
            | FieldType::Boolean => ValType::I32,
            FieldType::Double => ValType::F64,
            FieldType::Float => ValType::F32,
            FieldType::Long => ValType::I64,
            FieldType::Class(_) => ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Any,
            }),
        }
    }

    pub fn new_from_descriptor(descriptor: &MethodDescriptor, is_static: bool) -> Self {
        let mut params: Vec<ValType> = descriptor
            .args
            .iter()
            .map(Self::field_type_to_val_type)
            .collect();

        //*mut Thread
        params.push(ValType::I32);

        if !is_static {
            params.insert(
                0,
                ValType::Ref(RefType {
                    nullable: false,
                    heap_type: HeapType::Any,
                }),
            );
        }

        let return_type = match &descriptor.return_type {
            ReturnType::FieldType(field_type) => Some(Self::field_type_to_val_type(field_type)),
            ReturnType::Void => None,
        };

        Self {
            thread_local: (params.len() - 1) as u32,
            params,
            return_type,
        }
    }

    pub fn new(method: &Method) -> Self {
        let mut params: Vec<ValType> = method
            .descriptor
            .args
            .iter()
            .map(Self::field_type_to_val_type)
            .collect();

        //*mut Thread
        params.push(ValType::I32);

        if !method.access_flags.contains(AccessFlags::STATIC) {
            params.insert(
                0,
                ValType::Ref(RefType {
                    nullable: false,
                    heap_type: HeapType::Any,
                }),
            );
        }

        let return_type = match &method.descriptor.return_type {
            ReturnType::FieldType(field_type) => Some(Self::field_type_to_val_type(field_type)),
            ReturnType::Void => None,
        };

        Self {
            thread_local: (params.len() - 1) as u32,
            params,
            return_type
        }
    }
}

pub struct StubModule {
    pub(crate) wasm: Module,
    pub(crate) accessors: Vec<String>,
}

fn sub_type_from_class<'a>(index: u32, fields: impl IntoIterator<Item = &'a Field>) -> SubType {
    let mut field_types = fields
        .into_iter()
        .filter(|field| !field.access_flags.contains(AccessFlags::STATIC))
        .map(|field| {
            match field.descriptor {
                FieldType::Array { .. } => wasm_encoder::FieldType {
                    element_type: StorageType::Val(ValType::Ref(RefType {
                        nullable: true,
                        heap_type: HeapType::Any,
                    })),
                    mutable: true,
                },
                FieldType::Boolean | FieldType::Byte => {
                    wasm_encoder::FieldType {
                        element_type: StorageType::Val(ValType::I32),
                        // element_type: StorageType::I8,
                        mutable: true,
                    }
                }
                FieldType::Char | FieldType::Short => wasm_encoder::FieldType {
                    element_type: StorageType::Val(ValType::I32),
                    // element_type: StorageType::I16,
                    mutable: true,
                },
                FieldType::Double => wasm_encoder::FieldType {
                    element_type: StorageType::Val(ValType::F64),
                    mutable: true,
                },
                FieldType::Float => wasm_encoder::FieldType {
                    element_type: StorageType::Val(ValType::F32),
                    mutable: true,
                },
                FieldType::Int => wasm_encoder::FieldType {
                    element_type: StorageType::Val(ValType::I32),
                    mutable: true,
                },
                FieldType::Long => wasm_encoder::FieldType {
                    element_type: StorageType::Val(ValType::I64),
                    mutable: true,
                },
                FieldType::Class(_) => wasm_encoder::FieldType {
                    element_type: StorageType::Val(ValType::Ref(RefType {
                        nullable: true,
                        heap_type: HeapType::Any,
                    })),
                    mutable: true,
                },
            }
        })
        .collect::<Vec<_>>();

    field_types.insert(0, wasm_encoder::FieldType {
        element_type: StorageType::Val(ValType::I32),
        mutable: true,
    });

    field_types.insert(1, wasm_encoder::FieldType {
        element_type: StorageType::Val(ValType::I64),
        mutable: true,
    });

    SubType {
        is_final: false,
        supertype_idx: Some(index),
        composite_type: CompositeType::Struct(StructType {
            fields: field_types.into_boxed_slice(),
        }),
    }
}

pub fn create_base_object_type(types: &mut TypeSection) {
    types.rec([
        SubType {
            is_final: false,
            supertype_idx: None,
            composite_type: CompositeType::Struct(StructType {
                fields: Box::new([
                    wasm_encoder::FieldType {
                        element_type: StorageType::Val(ValType::I32),
                        mutable: true,
                    },
                    wasm_encoder::FieldType {
                        element_type: StorageType::Val(ValType::I64),
                        mutable: true,
                    }
                ]),
            }),
        }
    ]);

}

struct GeneratedRec<'a> {
    this_class_field_offset: u32,
    recursive_fields: Vec<(&'a Class, &'a Field)>,
    type_index: u32
}

fn create_class_rec<'a: 'b, 'b>(class: &'a Class, types: &'b mut TypeSection) -> GeneratedRec<'a> {
    let mut recursive_fields = vec![];
    let parents = class.parents();

    for (index, parent) in parents.iter().rev().enumerate() {
        recursive_fields.extend(
            parent
                .fields
                .iter()
                .filter(|field| !field.access_flags.contains(AccessFlags::STATIC))
                .map(|field| (&***parent, field))
                .collect::<Vec<(&Class, &Field)>>()
        );

        types.rec(
            [sub_type_from_class(index as u32, recursive_fields.iter().map(|(_, field)| *field))]
        );
    }

    let this_class_field_offset = recursive_fields.len() as u32;

    recursive_fields.extend(
        class
            .fields
            .iter()
            .filter(|field| !field.access_flags.contains(AccessFlags::STATIC))
            .map(|field| (class, field))
            .collect::<Vec<(&Class, &Field)>>()
    );

    let type_index = types.len();

    types.rec(
        [sub_type_from_class(class.parents().len() as u32, recursive_fields.iter().map(|(_, field)| *field))]
    );

    GeneratedRec {
        this_class_field_offset,
        recursive_fields,
        type_index
    }
}

pub fn create_stub_module(class: &Class, func_table_offset: u32) -> StubModule {
    let mut module = Module::new();

    let mut types = TypeSection::new();
    let mut exports = ExportSection::new();

    create_base_object_type(&mut types);

    let generated_rec = create_class_rec(class, &mut types);

    let mut imports = ImportSection::new();

    imports.import(
        "jvm",
        "objects",
        EntityType::Table(TableType {
            element_type: RefType {
                nullable: true,
                heap_type: HeapType::Any,
            },
            minimum: 0,
            maximum: None,
        }),
    );

    imports.import(
        "jvm",
        "functions",
        EntityType::Table(TableType {
            element_type: RefType {
                nullable: true,
                heap_type: HeapType::Func,
            },
            minimum: 0,
            maximum: None,
        }),
    );

    imports.import(
        "jvm",
        "memory",
        EntityType::Memory(MemoryType {
            minimum: 1,
            maximum: Some(16384),
            memory64: false,
            shared: true,
        }),
    );

    let i32_out = types.len();
    types.function([], [ValType::I32]);
    let i32_in = types.len();
    types.function([ValType::I32], []);
    let malloc_type = types.len();
    types.function([ValType::I32], [ValType::I32]);
    let free_type = types.len();
    //Size, ptr
    types.function([ValType::I32, ValType::I32], []);
    let interpret_func_type = types.len();
    types.function([ValType::I32], [ValType::I32]);
    let push_frame_type = types.len();
    types.function([ValType::I32; 4], [ValType::I32]);
    let debug_type = types.len();
    types.function([ValType::I32; 2], []);

    let alloc_ref = 0;
    imports.import("jvm", "alloc_ref", EntityType::Function(i32_out));
    let dealloc_ref = 1;
    imports.import("jvm", "dealloc_ref", EntityType::Function(i32_in));
    let malloc = 2;
    imports.import("jvm", "malloc", EntityType::Function(malloc_type));
    let free = 3;
    imports.import("jvm", "free", EntityType::Function(free_type));
    let interpret = 4;
    imports.import(
        "jvm",
        "interpret",
        EntityType::Function(interpret_func_type),
    );

    let push_frame = 5;
    imports.import(
        "jvm",
        "push_frame",
        EntityType::Function(push_frame_type),
    );

    let debug = 6;
    imports.import(
        "jvm",
        "debug",
        EntityType::Function(debug_type)
    );
    let func_imports_len = 7;

    // let func_imports_len = 6;

    let mut funcs = FunctionSection::new();
    let mut code = CodeSection::new();

    let mut accessors = vec![];

    let member_fields = class.fields.iter().filter(|field| !field.access_flags.contains(AccessFlags::STATIC));

    for (index, field) in member_fields.enumerate().map(|(index, field)| (index as u32 + generated_rec.this_class_field_offset + 2, field))
    {
        for is_setter in [true, false] {
            let func_type_index = types.len();

            let field_type = match field.descriptor {
                FieldType::Double => ValType::F64,
                FieldType::Float => ValType::F32,
                FieldType::Char
                | FieldType::Boolean
                | FieldType::Short
                | FieldType::Int
                | FieldType::Byte => ValType::I32,
                FieldType::Long => ValType::I64,
                FieldType::Class(_) | FieldType::Array { .. } => ValType::I32,
            };

            let is_object = matches!(
                field.descriptor,
                FieldType::Class(_) | FieldType::Array { .. }
            );

            let mut function = Function::new(if is_object {
                vec![(1, ValType::I32), (1, ValType::Ref(RefType { nullable: true, heap_type: HeapType::Any }))]
            } else {
                vec![]
            });

            if is_setter {
                types.function([ValType::I32, field_type], []);

                //Push the reference index onto the stack
                function.instruction(&Instruction::LocalGet(0));
                function.instruction(&Instruction::TableGet(0));
                function.instruction(&Instruction::RefCastNullable(HeapType::Concrete(
                    generated_rec.type_index,
                )));

                //Push the value we want to set onto the stack
                function.instruction(&Instruction::LocalGet(1));

                if is_object {
                    function.instruction(&Instruction::TableGet(0));

                    function.instruction(&Instruction::LocalGet(1));
                    function.instruction(&Instruction::Call(dealloc_ref));
                }

                //Set the field
                function.instruction(&Instruction::StructSet {
                    struct_type_index: generated_rec.type_index,
                    field_index: index,
                });
            } else {
                types.function([ValType::I32], [field_type]);

                if is_object {
                    function.instruction(&Instruction::Block(BlockType::Empty));
                }

                function.instruction(&Instruction::LocalGet(0));
                function.instruction(&Instruction::TableGet(0));
                function.instruction(&Instruction::RefCastNullable(HeapType::Concrete(
                    generated_rec.type_index,
                )));
                function.instruction(&Instruction::StructGet {
                    struct_type_index: generated_rec.type_index,
                    field_index: index,
                });

                if is_object {
                    function.instruction(&Instruction::RefCastNullable(HeapType::Any));
                    function.instruction(&Instruction::LocalTee(2));
                    function.instruction(&Instruction::BrOnNull(0));

                    function.instruction(&Instruction::Call(alloc_ref));
                    function.instruction(&Instruction::LocalTee(1));

                    function.instruction(&Instruction::LocalGet(2));

                    function.instruction(&Instruction::TableSet(0));
                    function.instruction(&Instruction::LocalGet(1));
                    function.instruction(&Instruction::Return);

                    function.instruction(&Instruction::End);

                    function.instruction(&Instruction::I32Const(0));
                }
            }

            function.instruction(&Instruction::End);

            let accessor_name =
                format!("{}_{}", if is_setter { "set" } else { "get" }, &field.name);

            exports.export(
                &accessor_name,
                ExportKind::Func,
                funcs.len() + func_imports_len,
            );

            accessors.push(accessor_name);

            funcs.function(func_type_index);
            code.function(&function);
        }
    }

    let mut tables = TableSection::new();
    let mut method_func_indices = vec![];

    let mut method_types = vec![];

    let stubbable_methods: Vec<&Arc<Method>> = class.methods.iter().filter(|method| !method.access_flags.contains(AccessFlags::NATIVE)).collect();

    //Trampoline for WASM to interpreter
    for method in &stubbable_methods {
        let sig = MethodSignature::new(&method);
        let mut func = Function::new([(4, ValType::I32)]);

        let mut params = method.descriptor.args.clone();

        if !method.access_flags.contains(AccessFlags::STATIC) {
            //Class name is irrelevant
            params.insert(0, FieldType::Class("".into()))
        }

        // let frame_address_local = sig.params.len() as u32;
        // let jlocal_box_addr_local = sig.params.len() as u32 + 1;
        // let i32_helper = sig.params.len() as u32 + 2;
        // let frame_stack_local = sig.params.len() as u32 + 3;
        let thread_local = sig.thread_local;
        let jlocal_box_addr_local = sig.params.len() as u32;
        let i32_helper = sig.params.len() as u32 + 1;
        let frame_stack_local = sig.params.len() as u32 + 2;
        let frame_address_local = sig.params.len() as u32 + 3;

        let max_locals = match method.attributes.get("Code") {
            Some(Attribute::Code(code)) => code.max_locals,
            _ => 256 //TODO: bad
        };

        func.instruction(&Instruction::LocalGet(thread_local));
        func.instruction(&Instruction::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        func.instruction(&Instruction::LocalTee(frame_stack_local));
        func.instruction(&Instruction::I32Const(max_locals as i32));
        func.instruction(&Instruction::I32Const(class as *const Class as i32));
        func.instruction(&Instruction::I32Const(&***method as *const Method as i32));
        func.instruction(&Instruction::Call(push_frame));
        func.instruction(&Instruction::LocalTee(frame_address_local));
        func.instruction(&Instruction::I32Load(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }));
        func.instruction(&Instruction::LocalSet(jlocal_box_addr_local));

        let mut local_store_index = 0;

        for (index, arg) in params.iter().enumerate() {
            match arg {
                //Heap type
                FieldType::Array { .. } | FieldType::Class(_) => {
                    //Alloc the ref
                    func.instruction(&Instruction::Block(BlockType::Empty));
                    func.instruction(&Instruction::Block(BlockType::Empty));

                    func.instruction(&Instruction::LocalGet(index as u32));
                    func.instruction(&Instruction::BrOnNull(0));

                    func.instruction(&Instruction::Call(alloc_ref));
                    func.instruction(&Instruction::LocalTee(i32_helper));
                    func.instruction(&Instruction::LocalGet(index as u32));

                    if matches!(arg, FieldType::Array { .. }) {
                        func.instruction(&Instruction::RefCastNullable(HeapType::Any));
                    }

                    //Store the ref in the table
                    func.instruction(&Instruction::TableSet(0));

                    //Store the index of the ref in the args
                    func.instruction(&Instruction::LocalGet(jlocal_box_addr_local));
                    func.instruction(&Instruction::LocalGet(i32_helper));
                    func.instruction(&Instruction::I32Store(MemArg {
                        offset: (local_store_index * size_of::<Operand>()) as u64,
                        align: 2,
                        memory_index: 0,
                    }));

                    func.instruction(&Instruction::Br(1));

                    func.instruction(&Instruction::End);

                    func.instruction(&Instruction::LocalGet(jlocal_box_addr_local));
                    func.instruction(&Instruction::I32Const(0));
                    func.instruction(&Instruction::I32Store(MemArg {
                        offset: (local_store_index * size_of::<Operand>()) as u64,
                        align: 2,
                        memory_index: 0,
                    }));

                    func.instruction(&Instruction::End);

                    local_store_index += 1;
                }
                //Scalar type
                FieldType::Int
                | FieldType::Byte
                | FieldType::Short
                | FieldType::Char
                | FieldType::Boolean => {
                    func.instruction(&Instruction::LocalGet(jlocal_box_addr_local));
                    func.instruction(&Instruction::LocalGet(index as u32));
                    func.instruction(&Instruction::I32Store(MemArg {
                        offset: (local_store_index * size_of::<Operand>()) as u64,
                        align: 2,
                        memory_index: 0,
                    }));

                    local_store_index += 1;
                }
                FieldType::Long => {
                    func.instruction(&Instruction::LocalGet(jlocal_box_addr_local));
                    func.instruction(&Instruction::LocalGet(index as u32));
                    func.instruction(&Instruction::I64Const(((u32::MAX as u64) << 32) as i64));
                    func.instruction(&Instruction::I64And);
                    func.instruction(&Instruction::I64Store(MemArg {
                        offset: (local_store_index * size_of::<Operand>()) as u64,
                        align: 3,
                        memory_index: 0,
                    }));
                    
                    func.instruction(&Instruction::LocalGet(jlocal_box_addr_local));
                    func.instruction(&Instruction::LocalGet(index as u32));
                    func.instruction(&Instruction::I64Const(u32::MAX as i64));
                    func.instruction(&Instruction::I64And);
                    func.instruction(&Instruction::I64Store(MemArg {
                        offset: ((local_store_index + 1) * size_of::<Operand>()) as u64,
                        align: 3,
                        memory_index: 0,
                    }));

                    local_store_index += 2;
                }
                FieldType::Double => {
                    func.instruction(&Instruction::LocalGet(jlocal_box_addr_local));
                    func.instruction(&Instruction::LocalGet(index as u32));
                    func.instruction(&Instruction::I64ReinterpretF64);
                    func.instruction(&Instruction::I64Const(((u32::MAX as u64) << 32) as i64));
                    func.instruction(&Instruction::I64And);
                    func.instruction(&Instruction::I64Store(MemArg {
                        offset: (local_store_index * size_of::<Operand>()) as u64,
                        align: 3,
                        memory_index: 0,
                    }));
                    
                    func.instruction(&Instruction::LocalGet(jlocal_box_addr_local));
                    func.instruction(&Instruction::LocalGet(index as u32));
                    func.instruction(&Instruction::I64ReinterpretF64);
                    func.instruction(&Instruction::I64Const(u32::MAX as i64));
                    func.instruction(&Instruction::I64And);
                    func.instruction(&Instruction::I64Store(MemArg {
                        offset: ((local_store_index + 1) * size_of::<Operand>()) as u64,
                        align: 3,
                        memory_index: 0,
                    }));

                    local_store_index += 2;
                }
                FieldType::Float => {
                    func.instruction(&Instruction::LocalGet(jlocal_box_addr_local));
                    func.instruction(&Instruction::LocalGet(index as u32));
                    func.instruction(&Instruction::F32Store(MemArg {
                        offset: (local_store_index * size_of::<Operand>()) as u64,
                        align: 2,
                        memory_index: 0,
                    }));

                    local_store_index += 1;
                }
                _ => unimplemented!("{:?}", arg),
            }
        }

        //Interpret the function
        func.instruction(&Instruction::LocalGet(sig.thread_local));
        func.instruction(&Instruction::Call(interpret));

        let temp = jlocal_box_addr_local;

        match &method.descriptor.return_type {
            ReturnType::FieldType(field_type) => match field_type {
                FieldType::Double => {
                    func.instruction(&Instruction::LocalTee(temp));

                    func.instruction(&Instruction::F64Load(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));

                    func.instruction(&Instruction::LocalGet(temp));
                    func.instruction(&Instruction::I32Const(size_of::<Operand>() as i32));
                    func.instruction(&Instruction::Call(free));
                }
                FieldType::Float => {
                    func.instruction(&Instruction::LocalTee(temp));

                    func.instruction(&Instruction::F32Load(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }));

                    func.instruction(&Instruction::LocalGet(temp));
                    func.instruction(&Instruction::I32Const(size_of::<Operand>() as i32));
                    func.instruction(&Instruction::Call(free));
                }
                FieldType::Long => {
                    func.instruction(&Instruction::LocalTee(temp));

                    func.instruction(&Instruction::I64Load(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));

                    func.instruction(&Instruction::LocalGet(temp));
                    func.instruction(&Instruction::I32Const(size_of::<Operand>() as i32));
                    func.instruction(&Instruction::Call(free));
                }
                FieldType::Class(_) | FieldType::Array { .. } => {
                    func.instruction(&Instruction::LocalTee(temp));
                    func.instruction(&Instruction::I32Load(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }));

                    func.instruction(&Instruction::LocalGet(temp));
                    func.instruction(&Instruction::I32Const(size_of::<Operand>() as i32));
                    func.instruction(&Instruction::Call(free));

                    func.instruction(&Instruction::LocalTee(temp));
                    //Get the reference from the table
                    func.instruction(&Instruction::TableGet(0));
                    
                    func.instruction(&Instruction::LocalGet(temp));
                    //Free the index
                    func.instruction(&Instruction::Call(dealloc_ref));
                }
                //Int
                _ => {
                    func.instruction(&Instruction::LocalTee(temp));

                    func.instruction(&Instruction::I32Load(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }));

                    func.instruction(&Instruction::LocalGet(temp));
                    func.instruction(&Instruction::I32Const(size_of::<Operand>() as i32));
                    func.instruction(&Instruction::Call(free));
                }
            },
            //Drop the returned empty i64 from calling the interpreter
            ReturnType::Void => {
                func.instruction(&Instruction::Drop);
            }
        }

        func.instruction(&Instruction::End);

        let func_index = funcs.len() + func_imports_len;
        let type_index = types.len();

        method_types.push(type_index);

        exports.export(
            method.get_identifier(),
            ExportKind::Func,
            func_index,
        );

        method_func_indices.push(func_index);

        funcs.function(type_index);
        types.function(sig.params, sig.return_type);
        code.function(&func);
    }

    for (func_index, method) in stubbable_methods.iter().enumerate() {
        let mut func = Function::new([(1, ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Any,
        })), (3, ValType::I32), (1, ValType::I64), (1, ValType::F64), (1, ValType::F32)]);

        let mut params: Vec<(bool, FieldType)> = method.descriptor.args.iter().map(|field_type| (false, field_type.clone())).collect();

        if !method.access_flags.contains(AccessFlags::STATIC) {
            //Class name is irrelevant
            params.insert(0, (true, FieldType::Class("".into())))
        }

        let thread_local = 0;
        let ref_local = 1;
        let i32_local = 2;
        let operand_local = 3;
        let frame_stack_local = 4;
        let long_local = 5;
        let double_local = 6;
        let float_local = 7;

        func.instruction(&Instruction::LocalGet(thread_local));
        func.instruction(&Instruction::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        func.instruction(&Instruction::LocalTee(frame_stack_local)); //frame_stack_local: *mut FrameStack := (*thread).frame_stack
        func.instruction(&Instruction::I32Load(MemArg { //(*frame_stack).frame_index
            offset: 4,
            align: 2,
            memory_index: 0,
        }));
        func.instruction(&Instruction::I32Const(size_of::<RawFrame>() as i32));
        func.instruction(&Instruction::I32Mul); //(*frame_stack).frame_index * size_of::<RawFrame>()

        func.instruction(&Instruction::LocalGet(frame_stack_local));
        func.instruction(&Instruction::I32Load(MemArg { //(*frame_stack).frames : *mut RawFrame
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        func.instruction(&Instruction::I32Add); //frames_addr + current_frame_offset
        func.instruction(&Instruction::I32Load(MemArg { //*mut Operand
            offset: 4,
            align: 2,
            memory_index: 0,
        }));
        func.instruction(&Instruction::LocalSet(operand_local));

        let mut index = 0;

        for (is_this, arg) in params.iter() {
            match arg {
                FieldType::Class(_) | FieldType::Array { .. } => {
                    func.instruction(&Instruction::LocalGet(operand_local));
                    
                    func.instruction(&Instruction::I32Load(MemArg {
                        offset: (index * size_of::<Operand>()) as u64,
                        align: 2,
                        memory_index: 0,
                    }));
                    func.instruction(&Instruction::LocalTee(i32_local));
                    func.instruction(&Instruction::TableGet(0));

                    // if matches!(arg, FieldType::Array { .. }) {
                    //     func.instruction(&Instruction::RefCastNullable(HeapType::Array));
                    // }

                    if *is_this {
                        func.instruction(&Instruction::RefCastNonNull(HeapType::Any));
                    }

                    func.instruction(&Instruction::LocalGet(i32_local));
                    func.instruction(&Instruction::Call(dealloc_ref));

                    index += 1;
                }
                FieldType::Double => {
                    func.instruction(&Instruction::LocalGet(operand_local));
                    func.instruction(&Instruction::I32Load(MemArg {
                        offset: (index * size_of::<Operand>()) as u64,
                        align: 2,
                        memory_index: 0,
                    }));
                    func.instruction(&Instruction::I64ExtendI32U);
                    func.instruction(&Instruction::I64Const(32));
                    func.instruction(&Instruction::I64Shl);

                    func.instruction(&Instruction::LocalGet(operand_local));
                    func.instruction(&Instruction::I32Load(MemArg {
                        offset: ((index + 1) * size_of::<Operand>()) as u64,
                        align: 2,
                        memory_index: 0,
                    }));
                    func.instruction(&Instruction::I64ExtendI32U);
                    func.instruction(&Instruction::I64Or);
                    func.instruction(&Instruction::F64ReinterpretI64);

                    index += 2;
                }
                FieldType::Float => {
                    func.instruction(&Instruction::LocalGet(operand_local));
                    func.instruction(&Instruction::F32Load(MemArg {
                        offset: (index * size_of::<Operand>()) as u64,
                        align: 2,
                        memory_index: 0,
                    }));

                    index += 1;
                }
                FieldType::Long => {
                    func.instruction(&Instruction::LocalGet(operand_local));
                    func.instruction(&Instruction::I32Load(MemArg {
                        offset: (index * size_of::<Operand>()) as u64,
                        align: 2,
                        memory_index: 0,
                    }));
                    func.instruction(&Instruction::I64ExtendI32U);
                    func.instruction(&Instruction::I64Const(32));
                    func.instruction(&Instruction::I64Shl);

                    func.instruction(&Instruction::LocalGet(operand_local));
                    func.instruction(&Instruction::I32Load(MemArg {
                        offset: ((index + 1) * size_of::<Operand>()) as u64,
                        align: 2,
                        memory_index: 0,
                    }));
                    func.instruction(&Instruction::I64ExtendI32U);
                    func.instruction(&Instruction::I64Or);

                    index += 2;
                }
                _ => {
                    func.instruction(&Instruction::LocalGet(operand_local));
                    func.instruction(&Instruction::I32Load(MemArg {
                        offset: (index * size_of::<Operand>()) as u64,
                        align: 2,
                        memory_index: 0,
                    }));

                    index += 1;
                }
            }

        }

        func.instruction(&Instruction::LocalGet(thread_local));
        func.instruction(&Instruction::I32Const(func_index as i32 + func_table_offset as i32));
        func.instruction(&Instruction::CallIndirect {
            ty: method_types[func_index],
            table: 1,
        });

        // //Dec the frame index in the FrameStack
        func.instruction(&Instruction::LocalGet(frame_stack_local));
        func.instruction(&Instruction::LocalGet(frame_stack_local));
        func.instruction(&Instruction::I32Load(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }));
        func.instruction(&Instruction::I32Const(-1));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::I32Store(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }));

        let temp = frame_stack_local; //reuse the local

        match &method.descriptor.return_type {
            ReturnType::FieldType(field_type) => {
                match field_type {
                    FieldType::Class(_) | FieldType::Array { .. } => {

                        func.instruction(&Instruction::RefCastNullable(HeapType::Any));
                        func.instruction(&Instruction::LocalSet(ref_local));
                        func.instruction(&Instruction::Call(alloc_ref));
                        func.instruction(&Instruction::LocalTee(i32_local));
                        func.instruction(&Instruction::LocalGet(ref_local));
                        func.instruction(&Instruction::TableSet(0));

                        func.instruction(&Instruction::I32Const(size_of::<Operand>() as i32));
                        func.instruction(&Instruction::Call(malloc));
                        func.instruction(&Instruction::LocalTee(temp));

                        func.instruction(&Instruction::LocalGet(i32_local));
                        func.instruction(&Instruction::I32Store(MemArg {
                            offset: 0,
                            align: 2,
                            memory_index: 0,
                        }));

                        func.instruction(&Instruction::LocalGet(temp));
                    },
                    FieldType::Double => {
                        func.instruction(&Instruction::LocalSet(double_local));

                        func.instruction(&Instruction::I32Const(size_of::<Operand>() as i32));
                        func.instruction(&Instruction::Call(malloc));
                        func.instruction(&Instruction::LocalTee(temp));

                        func.instruction(&Instruction::LocalGet(double_local));
                        func.instruction(&Instruction::F64Store(MemArg {
                            offset: 0,
                            align: 3,
                            memory_index: 0,
                        }));

                        //*OperandTag = 2
                        func.instruction(&Instruction::LocalGet(temp));
                        func.instruction(&Instruction::I32Const(2));
                        func.instruction(&Instruction::I32Store8(MemArg {
                            offset: 8,
                            align: 0,
                            memory_index: 0,
                        }));

                        func.instruction(&Instruction::LocalGet(temp));
                    }
                    FieldType::Float => {
                        func.instruction(&Instruction::LocalSet(float_local));

                        func.instruction(&Instruction::I32Const(size_of::<Operand>() as i32));
                        func.instruction(&Instruction::Call(malloc));
                        func.instruction(&Instruction::LocalTee(temp));
                        func.instruction(&Instruction::LocalGet(float_local));
                        func.instruction(&Instruction::F32Store(MemArg {
                            offset: 0,
                            align: 2,
                            memory_index: 0,
                        }));

                        //*OperandTag = 1
                        func.instruction(&Instruction::LocalGet(temp));
                        func.instruction(&Instruction::I32Const(1));
                        func.instruction(&Instruction::I32Store8(MemArg {
                            offset: 8,
                            align: 0,
                            memory_index: 0,
                        }));

                        func.instruction(&Instruction::LocalGet(temp));
                    }
                    FieldType::Long => {
                        func.instruction(&Instruction::LocalSet(long_local));

                        func.instruction(&Instruction::I32Const(size_of::<Operand>() as i32));
                        func.instruction(&Instruction::Call(malloc));
                        func.instruction(&Instruction::LocalTee(temp));
                        func.instruction(&Instruction::LocalGet(long_local));
                        func.instruction(&Instruction::I64Store(MemArg {
                            offset: 0,
                            align: 3,
                            memory_index: 0,
                        }));

                        //*OperandTag = 2
                        func.instruction(&Instruction::LocalGet(temp));
                        func.instruction(&Instruction::I32Const(2));
                        func.instruction(&Instruction::I32Store8(MemArg {
                            offset: 8,
                            align: 0,
                            memory_index: 0,
                        }));

                        func.instruction(&Instruction::LocalGet(temp));
                    }
                    _ => {
                        func.instruction(&Instruction::LocalSet(i32_local));

                        func.instruction(&Instruction::I32Const(size_of::<Operand>() as i32));
                        func.instruction(&Instruction::Call(malloc));
                        func.instruction(&Instruction::LocalTee(temp));

                        func.instruction(&Instruction::LocalGet(i32_local));
                        func.instruction(&Instruction::I32Store(MemArg {
                            offset: 0,
                            align: 2,
                            memory_index: 0,
                        }));

                        //*OperandTag = 1
                        func.instruction(&Instruction::LocalGet(temp));
                        func.instruction(&Instruction::I32Const(1));
                        func.instruction(&Instruction::I32Store8(MemArg {
                            offset: 8,
                            align: 0,
                            memory_index: 0,
                        }));

                        func.instruction(&Instruction::LocalGet(temp));
                    }
                }
            }
            ReturnType::Void => { func.instruction(&Instruction::I32Const(0)); }
        }

        func.instruction(&Instruction::End);

        exports.export(
            &format!("{}_abi", method.get_identifier()),
            ExportKind::Func,
            funcs.len() + func_imports_len,
        );

        funcs.function(types.len());
        types.function([ValType::I32], [ValType::I32]);
        code.function(&func);
    }

    {
        exports.export("new", ExportKind::Func, funcs.len() + func_imports_len);
        funcs.function(types.len());
        types.function([ValType::I64], [ValType::I32]);

        let mut function = Function::new([(1, ValType::I32)]);

        function.instruction(&Instruction::Call(alloc_ref));
        function.instruction(&Instruction::LocalTee(1));

        function.instruction(&Instruction::I32Const(class as *const Class as i32));
        function.instruction(&Instruction::LocalGet(0));

        for (_, field) in generated_rec.recursive_fields {
            match field.descriptor {
                FieldType::Array { .. } => function.instruction(&Instruction::RefNull(HeapType::Any)),
                FieldType::Byte => function.instruction(&Instruction::I32Const(0)),
                FieldType::Char => function.instruction(&Instruction::I32Const(0)),
                FieldType::Double => function.instruction(&Instruction::F64Const(0.0)),
                FieldType::Float => function.instruction(&Instruction::F32Const(0.0)),
                FieldType::Int => function.instruction(&Instruction::I32Const(0)),
                FieldType::Long => function.instruction(&Instruction::I64Const(0)),
                FieldType::Short => function.instruction(&Instruction::I32Const(0)),
                FieldType::Boolean => function.instruction(&Instruction::I32Const(0)),
                FieldType::Class(_) => function.instruction(&Instruction::RefNull(HeapType::Any)),
            };
        }
        function.instruction(&Instruction::StructNew(generated_rec.type_index));

        function.instruction(&Instruction::RefCastNullable(HeapType::Any));
        function.instruction(&Instruction::TableSet(0));
        function.instruction(&Instruction::LocalGet(1));
        function.instruction(&Instruction::End);

        code.function(&function);
    }

    module.section(&types);
    module.section(&imports);
    module.section(&funcs);
    module.section(&tables);
    module.section(&exports);
    module.section(ElementSection::new().active(
        Some(1),
        &ConstExpr::i32_const(func_table_offset as i32),
        Elements::Functions(&method_func_indices)
    ));
    module.section(&code);

    let bytes = module.clone().finish();

    let wat = wasmprinter::print_bytes(&bytes).unwrap();

    match wasmparser::Validator::new_with_features(WasmFeatures {
        gc: true,
        function_references: true,
        bulk_memory: true,
        ..Default::default()
    })
    .validate_all(&bytes)
    {
        Ok(_) => {}
        Err(error) => {
            let wat = wasmprinter::print_bytes(&bytes).unwrap();

            console_log!("{wat}");
        }
    }

    StubModule {
        wasm: module,
        accessors,
    }
}
