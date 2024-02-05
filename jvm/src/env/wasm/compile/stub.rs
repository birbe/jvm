use wasm_encoder::{BlockType, CodeSection, CompositeType, ConstExpr, Elements, ElementSection, EntityType, ExportKind, ExportSection, Function, FunctionSection, HeapType, ImportSection, Instruction, MemArg, MemoryType, Module, RefType, StorageType, StructType, SubType, TableSection, TableType, TypeSection, ValType};
use std::sync::Arc;
use wasmparser::WasmFeatures;
use crate::classfile::resolved::{AccessFlags, Attribute, Class, Field, FieldType, Method, ReturnType};
use crate::thread::RawFrame;

struct MethodSignature {
    pub params: Vec<ValType>,
    pub return_type: Option<ValType>,
    pub thread_local: u32,
}

impl MethodSignature {
    fn field_type_to_val_type(field_type: &FieldType) -> ValType {
        match field_type {
            FieldType::Array { .. } => ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Array,
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
            return_type,
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
                        heap_type: HeapType::Array,
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

    SubType {
        is_final: false,
        supertype_idx: if index > 0 {
            Some(index - 1)
        } else {
            None
        },
        composite_type: CompositeType::Struct(StructType {
            fields: field_types.into_boxed_slice(),
        }),
    }
}

pub fn create_stub_module(class: &Class) -> StubModule {
    let mut module = Module::new();

    let mut types = TypeSection::new();
    let mut exports = ExportSection::new();

    let mut recursive_fields = vec![];
    let mut sub_types = vec![];

    let parents = class.parents();

    for (index, parent) in parents.iter().rev().enumerate() {
        recursive_fields.extend(
            parent
                .fields
                .iter()
                .filter(|field| !field.access_flags.contains(AccessFlags::STATIC))
                .map(|field| (&**parent, field))
                .collect::<Vec<(&Class, &Field)>>()
        );

        sub_types.push(
            sub_type_from_class(index as u32, recursive_fields.iter().map(|(_, field)| *field))
        );
    }

    recursive_fields.extend(
        class
            .fields
            .iter()
            .filter(|field| !field.access_flags.contains(AccessFlags::STATIC))
            .map(|field| (class, field))
            .collect::<Vec<(&Class, &Field)>>()
    );

    sub_types.push(
        sub_type_from_class(class.parents().len() as u32, recursive_fields.iter().map(|(_, field)| *field))
    );

    types.rec(sub_types);

    let struct_type_index = class.parents().len() as u32;

    let desync: u32 = struct_type_index;

    let reflector = types.len() + desync;
    types.struct_([
        wasm_encoder::FieldType {
            element_type: StorageType::Val(ValType::Ref(RefType { nullable: false, heap_type: HeapType::Any })),
            mutable: false,
        },
        wasm_encoder::FieldType {
            element_type: StorageType::Val(ValType::I32),
            mutable: false,
        },
    ]);

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
        "memory",
        EntityType::Memory(MemoryType {
            minimum: 1,
            maximum: Some(16384),
            memory64: false,
            shared: true,
        }),
    );

    let i32_out = types.len() + desync;
    types.function([], [ValType::I32]);
    let i32_in = types.len() + desync;
    types.function([ValType::I32], []);
    let malloc_type = types.len() + desync;
    types.function([ValType::I32], [ValType::I32]);
    let free_type = types.len() + desync;
    //Size, ptr
    types.function([ValType::I32, ValType::I32], []);
    let interpret_func_type = types.len() + desync;
    types.function([ValType::I32], [ValType::I64]);
    let push_frame_type = types.len() + desync;
    types.function([ValType::I32; 4], [ValType::I32]);
    let debug_type = types.len() + desync;
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

    for (index, (field_class, field)) in recursive_fields.iter()
        .enumerate()
    {
        for is_setter in [true, false] {
            let func_type_index = types.len() + desync;

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
                //Get the reflector instance
                function.instruction(&Instruction::TableGet(0));
                //Cast the (ref null) to a (ref null $reflector)
                function.instruction(&Instruction::RefCastNullable(HeapType::Concrete(reflector)));
                //Get the struct in the reflector
                function.instruction(&Instruction::StructGet { struct_type_index: reflector, field_index: 0 });
                //Cast to this class's struct type
                function.instruction(&Instruction::RefCastNullable(HeapType::Concrete(
                    struct_type_index,
                )));

                //Push the value we want to set onto the stack
                function.instruction(&Instruction::LocalGet(1));

                if is_object {
                    function.instruction(&Instruction::TableGet(0));

                    if matches!(field.descriptor, FieldType::Array { .. }) {
                        function.instruction(&Instruction::RefCastNullable(HeapType::Array));
                    }
                }

                //Set the field
                function.instruction(&Instruction::StructSet {
                    struct_type_index,
                    field_index: index as u32,
                });
            } else {
                types.function([ValType::I32], [field_type]);

                if is_object {
                    function.instruction(&Instruction::Block(BlockType::Empty));
                }

                function.instruction(&Instruction::LocalGet(0));
                function.instruction(&Instruction::TableGet(0));
                function.instruction(&Instruction::RefCastNullable(HeapType::Concrete(reflector)));
                function.instruction(&Instruction::StructGet { struct_type_index: reflector, field_index: 0 });
                function.instruction(&Instruction::RefCastNullable(HeapType::Concrete(
                    struct_type_index,
                )));
                function.instruction(&Instruction::StructGet {
                    struct_type_index,
                    field_index: index as u32,
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
                format!("{}_{}_{}", field_class.this_class, if is_setter { "set" } else { "get" }, &field.name);

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

        {
            func.instruction(&Instruction::I32Const(1));
            func.instruction(&Instruction::LocalGet(jlocal_box_addr_local));
            func.instruction(&Instruction::Call(debug));

            func.instruction(&Instruction::I32Const(2));
            func.instruction(&Instruction::LocalGet(frame_address_local));
            func.instruction(&Instruction::Call(debug));
        }

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
                        offset: (index * 8) as u64,
                        align: 2,
                        memory_index: 0,
                    }));

                    func.instruction(&Instruction::Br(1));

                    func.instruction(&Instruction::End);

                    func.instruction(&Instruction::LocalGet(jlocal_box_addr_local));
                    func.instruction(&Instruction::I32Const(0));
                    func.instruction(&Instruction::I32Store(MemArg {
                        offset: (index * 8) as u64,
                        align: 2,
                        memory_index: 0,
                    }));

                    func.instruction(&Instruction::End);
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
                        offset: (index * 8) as u64,
                        align: 2,
                        memory_index: 0,
                    }));
                }
                FieldType::Long => {
                    func.instruction(&Instruction::LocalGet(jlocal_box_addr_local));
                    func.instruction(&Instruction::LocalGet(index as u32));
                    func.instruction(&Instruction::I64Store(MemArg {
                        offset: (index * 8) as u64,
                        align: 3,
                        memory_index: 0,
                    }));
                }
                FieldType::Double => {
                    func.instruction(&Instruction::LocalGet(jlocal_box_addr_local));
                    func.instruction(&Instruction::LocalGet(index as u32));
                    func.instruction(&Instruction::F64Store(MemArg {
                        offset: (index * 8) as u64,
                        align: 3,
                        memory_index: 0,
                    }));
                }
                FieldType::Float => {
                    func.instruction(&Instruction::LocalGet(jlocal_box_addr_local));
                    func.instruction(&Instruction::LocalGet(index as u32));
                    func.instruction(&Instruction::F32Store(MemArg {
                        offset: (index * 8) as u64,
                        align: 2,
                        memory_index: 0,
                    }));
                }
                _ => unimplemented!("{:?}", arg),
            }
        }

        //Interpret the function
        func.instruction(&Instruction::LocalGet(sig.thread_local));
        func.instruction(&Instruction::Call(interpret));

        match &method.descriptor.return_type {
            ReturnType::FieldType(field_type) => match field_type {
                FieldType::Double => {
                    func.instruction(&Instruction::F64ReinterpretI64);
                }
                FieldType::Float => {
                    func.instruction(&Instruction::I32WrapI64);
                    func.instruction(&Instruction::F32ReinterpretI32);
                }
                //The return type of the interpreter ABI is already an i64
                FieldType::Long => {}
                FieldType::Class(_) | FieldType::Array { .. } => {
                    func.instruction(&Instruction::I32WrapI64);
                    func.instruction(&Instruction::LocalTee(jlocal_box_addr_local));
                    //Get the reference from the table
                    func.instruction(&Instruction::TableGet(0));
                    func.instruction(&Instruction::LocalGet(jlocal_box_addr_local));
                    //Free the index
                    func.instruction(&Instruction::Call(dealloc_ref));

                    if matches!(field_type, FieldType::Array { .. }) {
                        func.instruction(&Instruction::RefCastNullable(HeapType::Array));
                    }
                }
                //Int
                _ => {
                    func.instruction(&Instruction::I32WrapI64);
                }
            },
            //Drop the returned i64 from calling the interpreter
            ReturnType::Void => {
                func.instruction(&Instruction::Drop);
            }
        }

        func.instruction(&Instruction::End);

        let func_index = funcs.len() + func_imports_len;
        let type_index = types.len() + desync;

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

    tables.table(TableType {
        element_type: RefType { nullable: true, heap_type: HeapType::Func },
        minimum: class.methods.len() as u32,
        maximum: Some(class.methods.len() as u32),
    });

    for (func_index, method) in stubbable_methods.iter().enumerate() {
        let mut func = Function::new([(1, ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Any,
        })), (3, ValType::I32)]);

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

        func.instruction(&Instruction::LocalGet(thread_local));
        func.instruction(&Instruction::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        func.instruction(&Instruction::LocalTee(frame_stack_local));
        func.instruction(&Instruction::I32Load(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }));
        func.instruction(&Instruction::I32Const(std::mem::size_of::<RawFrame>() as i32));
        func.instruction(&Instruction::I32Mul); //Offset in bytes from the *mut RawFrame
        func.instruction(&Instruction::LocalGet(frame_stack_local));
        func.instruction(&Instruction::I32Load(MemArg { //*mut RawFrame
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::I32Load(MemArg { //*mut Operand
            offset: 4,
            align: 2,
            memory_index: 0,
        }));
        func.instruction(&Instruction::LocalSet(operand_local));

        for (index, (is_this, arg)) in params.iter().enumerate() {
            match arg {
                FieldType::Class(_) | FieldType::Array { .. } => {
                    func.instruction(&Instruction::LocalGet(operand_local));
                    func.instruction(&Instruction::I32Load(MemArg {
                        offset: (index * 8) as u64,
                        align: 2,
                        memory_index: 0,
                    }));
                    func.instruction(&Instruction::LocalTee(i32_local));
                    func.instruction(&Instruction::TableGet(0));

                    if matches!(arg, FieldType::Array { .. }) {
                        func.instruction(&Instruction::RefCastNullable(HeapType::Array));
                    }

                    if *is_this {
                        func.instruction(&Instruction::RefCastNonNull(HeapType::Any));
                    }

                    func.instruction(&Instruction::LocalGet(i32_local));
                    func.instruction(&Instruction::Call(dealloc_ref));
                }
                FieldType::Double => {
                    func.instruction(&Instruction::LocalGet(operand_local));
                    func.instruction(&Instruction::F64Load(MemArg {
                        offset: (index * 8) as u64,
                        align: 2,
                        memory_index: 0,
                    }));
                }
                FieldType::Float => {
                    func.instruction(&Instruction::LocalGet(operand_local));
                    func.instruction(&Instruction::F32Load(MemArg {
                        offset: (index * 8) as u64,
                        align: 2,
                        memory_index: 0,
                    }));
                }
                FieldType::Long => {
                    func.instruction(&Instruction::LocalGet(operand_local));
                    func.instruction(&Instruction::I64Load(MemArg {
                        offset: (index * 8) as u64,
                        align: 2,
                        memory_index: 0,
                    }));
                }
                _ => {
                    func.instruction(&Instruction::LocalGet(operand_local));
                    func.instruction(&Instruction::I32Load(MemArg {
                        offset: (index * 8) as u64,
                        align: 2,
                        memory_index: 0,
                    }));
                }
            }

        }

        func.instruction(&Instruction::LocalGet(thread_local));
        func.instruction(&Instruction::I32Const(func_index as i32));

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
                        func.instruction(&Instruction::LocalGet(i32_local));
                        func.instruction(&Instruction::I64ExtendI32U);
                    },
                    FieldType::Double => {
                        func.instruction(&Instruction::I64ReinterpretF64);
                    }
                    FieldType::Float => {
                        func.instruction(&Instruction::I32ReinterpretF32);
                        func.instruction(&Instruction::I64ExtendI32U);
                    }
                    FieldType::Long => {}
                    _ => {
                        func.instruction(&Instruction::I64ExtendI32U);
                    }
                }
            }
            ReturnType::Void => { func.instruction(&Instruction::I64Const(0)); }
        }

        func.instruction(&Instruction::End);

        exports.export(
            &format!("{}_abi", method.get_identifier()),
            ExportKind::Func,
            funcs.len() + func_imports_len,
        );

        funcs.function(types.len() + desync);
        types.function([ValType::I32], [ValType::I64]);
        code.function(&func);
    }

    {
        exports.export("new", ExportKind::Func, funcs.len() + func_imports_len);
        funcs.function(types.len() + desync);
        types.function([], [ValType::I32]);

        let mut function = Function::new([(1, ValType::I32)]);

        function.instruction(&Instruction::Call(alloc_ref));
        function.instruction(&Instruction::LocalTee(0));

        for (_, field) in recursive_fields {
            match field.descriptor {
                FieldType::Array { .. } => function.instruction(&Instruction::RefNull(HeapType::Array)),
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
        function.instruction(&Instruction::StructNew(struct_type_index));
        function.instruction(&Instruction::I32Const(class as *const Class as i32));
        function.instruction(&Instruction::StructNew(reflector));

        function.instruction(&Instruction::RefCastNullable(HeapType::Any));
        function.instruction(&Instruction::TableSet(0));
        function.instruction(&Instruction::LocalGet(0));
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
        &ConstExpr::i32_const(0),
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
