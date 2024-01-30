use std::collections::HashMap;
use std::mem::{MaybeUninit, take};
use std::sync::Arc;
use futures_executor::block_on;
use js_sys::WebAssembly;
use once_cell::sync::{Lazy, OnceCell};
use parking_lot::RwLock;
use wasm_bindgen::describe::REF;
use wasm_bindgen::JsValue;
use wasm_bindgen_futures::JsFuture;
use wasm_encoder::{CodeSection, CompositeType, EntityType, ExportKind, ExportSection, Function, FunctionSection, HeapType, ImportSection, Instruction, MemArg, MemoryType, Module, RefType, StorageType, StructType, SubType, TableSection, TableType, TypeSection, ValType};
use wasmparser::WasmFeatures;
use crate::classfile::resolved::{AccessFlags, Class, ClassId, FieldType, Method, MethodDescriptor, NameAndType, Ref, ReturnType};
use crate::env::{Compiler, Environment};
use crate::execution::{ExecutionContext, MethodHandle};
use crate::heap::Object;
use crate::linker::ClassLoader;
use crate::thread::{FrameStack, Operand, RawFrame, Thread, ThreadHandle};

pub static mut REFS: MaybeUninit<RefSlots> = MaybeUninit::uninit();

pub mod compile;
mod scc;
pub mod wasm;
pub mod cfg;

macro_rules! console_log {
    ($($t:tt)*) => (web_sys::console::log_1(&format_args!($($t)*).to_string().into()))
}

pub struct RefSlots {
    pub available: Vec<i32>,
    pub len: u32,
    pub table: WebAssembly::Table
}

type MethodId = u32;

struct ClassReflector {
    pub field_accessors: HashMap<String, u32>,
    pub trampolines: HashMap<String, u32>
}

pub struct WasmEnvironment {
    pub class_function_pointers: RwLock<HashMap<ClassId, HashMap<String, u32>>>,
    pub object_table: WebAssembly::Table
}

impl WasmEnvironment {

    pub fn new(object_table: WebAssembly::Table) -> Self {
        unsafe { REFS.write(RefSlots {
            available: vec![],
            len: 0,
            table: object_table.clone(),
        }); }

        Self {
            class_function_pointers: Default::default(),
            object_table,
        }
    }

}

#[no_mangle]
pub extern "C" fn alloc_ref() -> i32 {
    // let refs_cell = REFS.get().unwrap();
    let mut refs = unsafe { REFS.assume_init_mut() };

    if refs.available.len() == 0 {
        refs.table.grow(1).unwrap();

        let len = refs.len;
        refs.len += 1;

        len as i32
    } else {
        refs.available.pop().unwrap()
    }
}

#[no_mangle]
pub extern "C" fn dealloc_ref(index: i32) {
    let mut refs = unsafe { REFS.assume_init_mut() };

    refs.available.push(index);
}

#[no_mangle]
pub extern "C" fn malloc(size: i32) -> i32 {
    todo!()
}

#[no_mangle]
pub extern "C" fn free(ptr: i32, len: i32) {
    todo!()
}

#[no_mangle]
pub extern "C" fn interpret(frame_stack: i32, thread: i32) -> i64 {
    todo!()
}

impl Environment for WasmEnvironment {

    fn link_class(&self, class: Arc<Class>) {
        let stub = create_stub_module(&class);

        let jvm_import_object = js_sys::Object::new();

        let meta_exports = wasm_bindgen::exports();
        let meta_memory = wasm_bindgen::memory();
        let func_table = WebAssembly::Table::from(wasm_bindgen::function_table());

        for cross_import in [
            "alloc_ref",
            "dealloc_ref",
            "malloc",
            "free",
            "interpret"
        ] {
            let key: JsValue = cross_import.into();

            js_sys::Reflect::set(
                &jvm_import_object,
                &key,
                &js_sys::Reflect::get(&meta_exports,&key).unwrap()
            ).unwrap();
        }

        js_sys::Reflect::set(&jvm_import_object, &"objects".into(), &self.object_table).unwrap();
        js_sys::Reflect::set(&jvm_import_object, &"memory".into(), &meta_memory).unwrap();

        let imports = js_sys::Object::new();

        js_sys::Reflect::set(&imports, &"jvm".into(), &jvm_import_object).unwrap();

        let wasm_bytes = stub.wasm.finish();

        console_log!("Loading class {}\n{}", class.this_class, wasmprinter::print_bytes(&wasm_bytes).unwrap());

        let uint8array = unsafe { js_sys::Uint8Array::view(&wasm_bytes) };

        let module = WebAssembly::Module::new(&uint8array).unwrap();
        let instance = WebAssembly::Instance::new(&module, &imports).unwrap();

        let exports = js_sys::Reflect::get(&instance, &"exports".into()).unwrap();

        let mut funcs_to_link: Vec<String> = class.methods.iter().map(|method| method.get_identifier().to_string()).collect();

        funcs_to_link.extend(stub.accessors.clone());
        funcs_to_link.push("new".into());

        let old_length = WebAssembly::Table::grow(&func_table, funcs_to_link.len() as u32).unwrap();

        let ptrs: HashMap<String, u32> = funcs_to_link.into_iter().enumerate().map(|(index, func_name)| {
            let func = js_sys::Function::from(
                js_sys::Reflect::get(&exports, &func_name.clone().into()).unwrap()
            );

            let indirect_index = (index as u32) + old_length;

            WebAssembly::Table::set(&func_table, indirect_index, &func).unwrap();

            (func_name, indirect_index)
        }).collect();

        let mut cfp = self.class_function_pointers.write();
        cfp.insert(class.get_id(), ptrs);
    }

    fn invoke_handle(&self, thread: &mut Thread, method_handle: &MethodHandle, args: Box<[Operand]>) -> u64 {
        todo!()
    }

    fn register_method_handle(&self, class_loader: &dyn ClassLoader, method: Arc<Ref>, handle: MethodHandle) {

    }

    fn get_object_field(&self, object: Object, class: &Class, field: &Ref) -> Operand {
        // let accessors = self.accessors.read();
        // let class_accessors = accessors.get(&class.get_id()).unwrap();
        // let accessor = *class_accessors.get(field).unwrap();
        //
        // let out = unsafe { (*(accessor as *const fn(u32) -> u64))(object.ptr as u32) };

        todo!()
    }

}

struct MethodSignature {
    pub params: Vec<ValType>,
    pub return_type: Option<ValType>,
    pub thread_local: u32
}

impl MethodSignature {

    fn field_type_to_val_type(field_type: &FieldType) -> ValType {
        match field_type {
            FieldType::Array { .. } => ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Array,
            }),
            FieldType::Byte | FieldType::Char | FieldType::Int | FieldType::Short | FieldType::Boolean => ValType::I32,
            FieldType::Double => ValType::F64,
            FieldType::Float => ValType::F32,
            FieldType::Long => ValType::I64,
            FieldType::Class(_) => ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Any,
            })
        }
    }

    pub fn new(method: &Method) -> Self {
        let mut params: Vec<ValType> = method.descriptor.args.iter().map(Self::field_type_to_val_type).collect();

        //*mut FrameStack
        params.push(ValType::I32);
        //*mut Thread
        params.push(ValType::I32);

        if !method.access_flags.contains(AccessFlags::STATIC) {
            params.insert(0, ValType::Ref(RefType {
                nullable: false,
                heap_type: HeapType::Any,
            }));
        }

        let return_type = match &method.descriptor.return_type {
            ReturnType::FieldType(field_type) => Some(Self::field_type_to_val_type(field_type)),
            ReturnType::Void => None
        };

        Self {
            thread_local: (params.len() - 1) as u32,
            params,
            return_type,
        }
    }

}

pub struct StubModule {
    wasm: Module,
    accessors: Vec<String>
}


pub fn create_stub_module(class: &Class) -> StubModule {
    let mut module = Module::new();

    let mut types = TypeSection::new();
    let mut exports = ExportSection::new();

    types.rec(
        class.parents().iter().map(|class_arc| &**class_arc).rev().chain([class]).enumerate().map(|(index, class)| {
            let parents = class.parents();
            let fields = parents.iter().rev().map(|class| class.fields.iter()).flatten().chain(class.fields.iter());

            console_log!("{:?}", fields.clone().collect::<Vec<_>>());

            SubType {
                is_final: false,
                supertype_idx: if index > 0 {
                    Some((index - 1) as u32)
                } else {
                    None
                },
                composite_type: CompositeType::Struct(StructType {
                    fields: fields.filter(|field| !field.access_flags.contains(AccessFlags::STATIC)).map(|field| {
                        match field.descriptor {
                            FieldType::Array { .. } => wasm_encoder::FieldType {
                                element_type: StorageType::Val(ValType::Ref(RefType {
                                    nullable: true,
                                    heap_type: HeapType::Array,
                                })),
                                mutable: true,
                            },
                            FieldType::Boolean | FieldType::Byte => wasm_encoder::FieldType {
                                element_type: StorageType::Val(ValType::I32),
                                // element_type: StorageType::I8,
                                mutable: true,
                            },
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
                            }
                        }
                    }).collect::<Vec<_>>().into_boxed_slice(),
                }),
            }
        }).collect::<Vec<_>>()
    );

    let struct_type_index = class.parents().len() as u32;

    let desync: u32 = struct_type_index;

    let mut imports = ImportSection::new();

    imports.import("jvm", "objects", EntityType::Table(TableType {
        element_type: RefType { nullable: true, heap_type: HeapType::Any },
        minimum: 0,
        maximum: None,
    }));

    imports.import("jvm", "memory", EntityType::Memory(MemoryType {
        minimum: 0,
        maximum: None,
        memory64: false,
        shared: false,
    }));

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
    types.function([ValType::I32, ValType::I32], [ValType::I64]);

    let alloc_ref = 0;
    imports.import("jvm", "alloc_ref", EntityType::Function(i32_out));
    let dealloc_ref = 1;
    imports.import("jvm", "dealloc_ref", EntityType::Function(i32_in));
    let malloc = 2;
    imports.import("jvm", "malloc", EntityType::Function(malloc_type));
    let free = 3;
    imports.import("jvm", "free", EntityType::Function(free_type));
    let interpret = 4;
    imports.import("jvm", "interpret", EntityType::Function(interpret_func_type));

    let func_imports_len = 5;

    let mut funcs = FunctionSection::new();
    let mut code = CodeSection::new();

    let mut accessors = vec![];

    for (index, field) in class.fields.iter().filter(|field| !field.access_flags.contains(AccessFlags::STATIC)).enumerate() {
        for is_setter in [true, false] {

            let func_type_index = types.len() + desync;

            let field_type = match field.descriptor {
                FieldType::Double => ValType::F64,
                FieldType::Float => ValType::F32,
                FieldType::Char | FieldType::Boolean | FieldType::Short | FieldType::Int | FieldType::Byte => ValType::I32,
                FieldType::Long => ValType::I64,
                FieldType::Class(_) | FieldType::Array { .. } => ValType::I32
            };

            let is_object = matches!(field.descriptor, FieldType::Class(_) | FieldType::Array { .. });

            let mut function = Function::new(if is_object {
                vec![(1u32, ValType::I32)]
            } else {
                vec![]
            });

            if is_setter {
                types.function(
                    [
                        ValType::I32,
                        field_type
                    ],
                    []
                );

                //Push the reference index onto the stack
                function.instruction(
                    &Instruction::LocalGet(0)
                );
                //Push the actual reference onto the stack
                function.instruction(
                    &Instruction::TableGet(0)
                );
                function.instruction(
                    &Instruction::RefCastNullable(HeapType::Concrete(struct_type_index))
                );

                //Push the value onto the stack
                function.instruction(&Instruction::LocalGet(1));

                if is_object {
                    function.instruction(&Instruction::TableGet(0));

                    if matches!(field.descriptor, FieldType::Array { .. }) {
                        function.instruction(&Instruction::RefCastNullable(HeapType::Array));
                    }
                }

                //Set the field
                function.instruction(
                    &Instruction::StructSet {
                        struct_type_index,
                        field_index: index as u32,
                    }
                );
            } else {
                types.function(
                    [
                        ValType::I32
                    ],
                    [
                        field_type
                    ]
                );

                if is_object {
                    function.instruction(&Instruction::Call(alloc_ref));
                    function.instruction(&Instruction::LocalTee(1));
                }

                function.instruction(
                    &Instruction::LocalGet(0)
                );
                function.instruction(&Instruction::TableGet(0));
                function.instruction(&Instruction::RefCastNullable(HeapType::Concrete(struct_type_index)));
                function.instruction(
                    &Instruction::StructGet {
                        struct_type_index,
                        field_index: index as u32,
                    }
                );

                if is_object {
                    function.instruction(&Instruction::RefCastNullable(HeapType::Any));
                    function.instruction(&Instruction::TableSet(0));
                    function.instruction(&Instruction::LocalGet(1));
                }
            }

            function.instruction(
                &Instruction::End
            );

            let accessor_name = format!("{}_{}", if is_setter {
                "set"
            } else {
                "get"
            }, &field.name);

            exports.export(&accessor_name, ExportKind::Func, funcs.len() + func_imports_len);

            accessors.push(accessor_name);

            funcs.function(func_type_index);
            code.function(&function);
        }
    }

    //Trampoline for WASM to interpreter
    for method in &class.methods {
        let sig = MethodSignature::new(&method);
        let mut func = Function::new([(3, ValType::I32)]);

        let frame_stack_local = (sig.params.len() - 2) as u32;

        let frame_address_local = sig.params.len() as u32;
        let box_address_local = sig.params.len() as u32 + 1;
        let i32_helper = sig.params.len() as u32 + 2;

        let box_size = (method.descriptor.args.len() * 8) as i32;

        func.instruction(&Instruction::LocalGet(frame_stack_local));
        func.instruction(&Instruction::I32Load(MemArg { //(*frame_stack).frames
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        func.instruction(&Instruction::LocalGet(frame_stack_local));
        func.instruction(&Instruction::I32Load(MemArg { //(*frame_stack).index
            offset: 4,
            align: 2,
            memory_index: 0,
        }));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Add); //Increment the isize, this is the index for the RawFrame
        func.instruction(&Instruction::I32Const(std::mem::size_of::<RawFrame>() as i32));
        func.instruction(&Instruction::I32Mul); //Calculate the offset in bytes from the *mut [RawFrame]
        func.instruction(&Instruction::I32Add); //Add the offset to the *mut RawFrame
        func.instruction(&Instruction::LocalTee(frame_address_local)); //This is the address for the RawFrame

        func.instruction(&Instruction::LocalGet(frame_address_local));
        func.instruction(&Instruction::LocalGet(frame_address_local));

        func.instruction(&Instruction::I32Const((&**method as *const Method as usize) as i32));
        func.instruction(&Instruction::I32Store(MemArg {
            offset: 8,
            align: 2,
            memory_index: 0,
        }));

        func.instruction(&Instruction::I32Const((class as *const Class as usize) as i32));
        func.instruction(&Instruction::I32Store(MemArg {
            offset: 12,
            align: 2,
            memory_index: 0,
        }));

        func.instruction(&Instruction::I32Const(box_size));
        func.instruction(&Instruction::I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));

        //Box<[i64]> for the arguments
        func.instruction(&Instruction::I32Const(box_size));
        func.instruction(&Instruction::Call(malloc));

        func.instruction(&Instruction::LocalGet(frame_address_local));
        func.instruction(&Instruction::LocalTee(box_address_local)); //Tee the address of the Box<[i64]>
        func.instruction(&Instruction::I32Store(MemArg { //(*raw_frame_addr).locals = box.ptr;
            offset: std::mem::size_of::<usize>() as u64,
            align: 2,
            memory_index: 0,
        }));

        let mut params = method.descriptor.args.clone();

        if !method.access_flags.contains(AccessFlags::STATIC) {
            //Class name is irrelevant
            params.insert(0, FieldType::Class("".into()))
        }

        for (index, arg) in params.iter().enumerate() {

            match arg {
                //Heap type
                FieldType::Array { .. } | FieldType::Class(_) => {
                    //Alloc the ref
                    func.instruction(&Instruction::Call(alloc_ref));
                    func.instruction(&Instruction::LocalTee(i32_helper));
                    func.instruction(&Instruction::LocalGet(index as u32));

                    if matches!(arg, FieldType::Array { .. }) {
                        func.instruction(&Instruction::RefCastNullable(HeapType::Any));
                    }

                    //Store the ref in the table
                    func.instruction(&Instruction::TableSet(0));

                    func.instruction(&Instruction::LocalGet(i32_helper));
                    //Store the index of the ref in the args
                    func.instruction(&Instruction::LocalGet(box_address_local));
                    func.instruction(&Instruction::I32Store(MemArg {
                        offset: (index * 8) as u64,
                        align: 2,
                        memory_index: 0,
                    }));
                },
                //Scalar type
                FieldType::Int | FieldType::Byte | FieldType::Short | FieldType::Char | FieldType::Boolean => {
                    func.instruction(&Instruction::LocalGet(box_address_local));
                    func.instruction(&Instruction::LocalGet(index as u32));
                    func.instruction(&Instruction::I32Store(MemArg {
                        offset: (index * 8) as u64,
                        align: 2,
                        memory_index: 0,
                    }));
                },
                FieldType::Long => {
                    func.instruction(&Instruction::LocalGet(box_address_local));
                    func.instruction(&Instruction::LocalGet(index as u32));
                    func.instruction(&Instruction::I64Store(MemArg {
                        offset: (index * 8) as u64,
                        align: 3,
                        memory_index: 0,
                    }));
                },
                FieldType::Double => {
                    func.instruction(&Instruction::LocalGet(box_address_local));
                    func.instruction(&Instruction::LocalGet(index as u32));
                    func.instruction(&Instruction::I64ReinterpretF64);
                    func.instruction(&Instruction::I64Store(MemArg {
                        offset: (index * 8) as u64,
                        align: 3,
                        memory_index: 0,
                    }));
                }
                FieldType::Float => {
                    func.instruction(&Instruction::LocalGet(box_address_local));
                    func.instruction(&Instruction::LocalGet(index as u32));
                    func.instruction(&Instruction::I32ReinterpretF32);
                    func.instruction(&Instruction::I32Store(MemArg {
                        offset: (index * 8) as u64,
                        align: 2,
                        memory_index: 0,
                    }));
                }
                _ => unimplemented!("{:?}", arg)
            }
        }

        //Interpret the function
        func.instruction(&Instruction::LocalGet(frame_address_local));
        func.instruction(&Instruction::LocalGet(sig.thread_local));
        func.instruction(&Instruction::Call(interpret));
        //Free the *mut [i64]
        func.instruction(&Instruction::I32Const(box_size));
        func.instruction(&Instruction::LocalGet(frame_address_local));
        func.instruction(&Instruction::Call(free));

        //Dec the frame index in the FrameStack
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
            ReturnType::FieldType(field_type) => match field_type {
                FieldType::Double => { func.instruction(&Instruction::F64ReinterpretI64); },
                FieldType::Float => {
                    func.instruction(&Instruction::I32WrapI64);
                    func.instruction(&Instruction::F32ReinterpretI32);
                }
                //The return type of the interpreter ABI is already an i64
                FieldType::Long => {}
                FieldType::Class(_) | FieldType::Array { .. } => {
                    func.instruction(&Instruction::I32WrapI64);
                    //The box was freed and we don't need the old value anymore anyways
                    func.instruction(&Instruction::LocalTee(box_address_local));
                    //Get the reference from the table
                    func.instruction(&Instruction::TableGet(0));
                    func.instruction(&Instruction::LocalGet(box_address_local));
                    //Free the index
                    func.instruction(&Instruction::Call(dealloc_ref));

                    if matches!(field_type, FieldType::Array { .. }) {
                        func.instruction(&Instruction::RefCastNullable(HeapType::Array));
                    }
                },
                //Int
                _ => { func.instruction(&Instruction::I32WrapI64); }
            }
            //Drop the returned i64 from calling the interpreter
            ReturnType::Void => { func.instruction(&Instruction::Drop); }
        }

        func.instruction(&Instruction::End);

        exports.export(method.get_identifier(), ExportKind::Func, funcs.len() + func_imports_len);
        funcs.function(types.len() + desync);
        types.function(sig.params, sig.return_type);
        code.function(&func);

    }

    {
        exports.export("new", ExportKind::Func, funcs.len() + func_imports_len);
        funcs.function(types.len() + desync);
        types.function([], [ValType::I32]);

        let mut function = Function::new([(1, ValType::I32)]);

        function.instruction(&Instruction::Call(alloc_ref));
        function.instruction(&Instruction::LocalTee(0));
        function.instruction(&Instruction::StructNewDefault(struct_type_index));
        function.instruction(&Instruction::RefCastNullable(HeapType::Any));
        function.instruction(&Instruction::TableSet(0));
        function.instruction(&Instruction::LocalGet(0));
        function.instruction(&Instruction::End);

        code.function(&function);
    }

    module.section(&types);
    module.section(&imports);
    module.section(&funcs);
    module.section(&exports);
    module.section(&code);

    let bytes = module.clone().finish();

    match wasmparser::Validator::new_with_features(WasmFeatures {
        gc: true,
        ..Default::default()
    }).validate_all(&bytes) {
        Ok(_) => {}
        Err(error) => {
            let wat = wasmprinter::print_bytes(&bytes).unwrap();

            panic!("Miscompiled WASM stub. Error: {}\nWat:\n{}", error, wat);
        }
    }

    StubModule {
        wasm: module,
        accessors
    }
}
