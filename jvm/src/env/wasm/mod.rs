use std::collections::HashMap;
use std::sync::Arc;
use parking_lot::RwLock;
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_encoder::{CodeSection, CompositeType, EntityType, Function, FunctionSection, HeapType, ImportSection, Instruction, MemArg, Module, RefType, StorageType, StructType, SubType, TableSection, TableType, TypeSection, ValType};
use crate::classfile::resolved::{Class, ClassId, FieldType, Method, Ref};
use crate::env::{Compiler, Environment};
use crate::execution::{ExecutionContext, MethodHandle};
use crate::heap::Object;
use crate::linker::ClassLoader;
use crate::thread::{FrameStack, Operand, RawFrame, Thread, ThreadHandle};

pub mod compile;
mod scc;
pub mod wasm;
pub mod cfg;

#[wasm_bindgen]
extern "C" {

    fn link_module(module: Vec<u8>) -> Vec<u32>;

}

type MethodId = u32;

pub struct WasmEnvironment {
    pub accessors: RwLock<HashMap<ClassId, HashMap<Arc<Ref>, u32>>>
}

impl WasmEnvironment {
    
    pub fn new() -> Self {
        Self {
            accessors: Default::default(),
        }
    }
    
}

impl Environment for WasmEnvironment {
    
    fn link_class(&self, class: Arc<Class>) {
        let stub = create_stub_module(&class);
    }

    fn invoke_handle(&self, thread: &mut Thread, method_handle: &MethodHandle, args: Box<[Operand]>) -> u64 {
        // let stack = FrameStack::new();
        //
        // match method_handle.context {
        //     ExecutionContext::Interpret(_) => {}
        //     ExecutionContext::Compiled => {
        //         (method_handle.ptr)(thread, args)
        //     }
        //     ExecutionContext::Native => {}
        // }

        // 0

        todo!()
    }

    fn register_method_handle(&self, class_loader: &dyn ClassLoader, method: Arc<Ref>, handle: MethodHandle) {

    }

    fn get_object_field(&self, object: Object, class: &Class, field: &Ref) -> Operand {
        let accessors = self.accessors.read();
        let class_accessors = accessors.get(&class.get_id()).unwrap();
        let accessor = *class_accessors.get(field).unwrap();

        let out = unsafe { (*(accessor as *const fn(u32) -> u64))(object.ptr as u32) };

        todo!()
    }

}

struct WasmCompiler;

impl Compiler for WasmCompiler {}

pub fn create_stub_module(class: &Class) -> Module {
    let mut module = Module::new();

    let mut types = TypeSection::new();

    let mut fields = vec![];

    types.rec(
        class.parents().iter().map(|class_arc| &**class_arc).rev().chain([class]).enumerate().map(|(index, class)| {
            fields.extend(class.fields.clone());

            SubType {
                is_final: false,
                supertype_idx: Some(index as u32),
                composite_type: CompositeType::Struct(StructType {
                    fields: fields.iter().map(|field| {
                        match field.descriptor {
                            FieldType::Array { .. } => wasm_encoder::FieldType {
                                element_type: StorageType::Val(ValType::Ref(RefType {
                                    nullable: true,
                                    heap_type: HeapType::Array,
                                })),
                                mutable: true,
                            },
                            FieldType::Boolean | FieldType::Byte => wasm_encoder::FieldType {
                                element_type: StorageType::I8,
                                mutable: true,
                            },
                            FieldType::Char | FieldType::Short => wasm_encoder::FieldType {
                                element_type: StorageType::I16,
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
                                    //Technically all Java objects, including primitive arrays, are instances of java/lang/Object
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

    let mut imports = ImportSection::new();

    imports.import("jvm", "objects", EntityType::Table(TableType {
        element_type: RefType { nullable: false, heap_type: HeapType::Any },
        minimum: 0,
        maximum: None,
    }));

    let i32_out = types.len();
    types.function([], [ValType::I32]);
    let i32_in = types.len();
    types.function([ValType::I32], []);
    let alloc_box_type = types.len();
    types.function([ValType::I32], [ValType::I32]);
    let interpret_func_type = types.len();
    types.function([ValType::I32, ValType::I32], [ValType::I64]);

    imports.import("jvm", "alloc_ref", EntityType::Function(i32_out));
    imports.import("jvm", "dealloc_ref", EntityType::Function(i32_in));
    imports.import("jvm", "alloc_box", EntityType::Function(alloc_box_type));
    imports.import("jvm", "interpret", EntityType::Function(interpret_func_type));

    let struct_type_index = types.len() - 1;

    let mut funcs = FunctionSection::new();
    let mut code = CodeSection::new();

    for (index, field) in class.fields.iter().enumerate() {
        for is_setter in [true, false] {

            let func_type_index = types.len();

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
            } else {
                types.function(
                    [
                        ValType::I32
                    ],
                    [
                        field_type
                    ]
                );
            }

            if is_setter {
                //Push the value onto the stack
                function.instruction(&Instruction::LocalGet(1));
                //Push the reference index onto the stack
                function.instruction(
                    &Instruction::LocalGet(0)
                );
                //Push the actual reference onto the stack
                function.instruction(
                    &Instruction::TableGet(0)
                );
                //Set the field
                function.instruction(
                    &Instruction::StructSet {
                        struct_type_index,
                        field_index: index as u32,
                    }
                );
            } else {
                function.instruction(
                    &Instruction::LocalGet(0)
                );
                function.instruction(&Instruction::TableGet(0));
                function.instruction(
                    &Instruction::StructGet {
                        struct_type_index,
                        field_index: index as u32,
                    }
                );
                if is_object {
                    //Allocate a reference, push the index onto the stack
                    function.instruction(&Instruction::Call(1));
                    //Store the index and keep it on the stack
                    function.instruction(&Instruction::LocalTee(0));
                    //Put the reference into the nth slot in the ref table
                    function.instruction(&Instruction::TableSet(0));
                    //Return the reference index
                    function.instruction(&Instruction::LocalGet(0));
                }
            }

            function.instruction(
                &Instruction::End
            );

            funcs.function(func_type_index);
            code.function(&function);
        }
    }

    for method in &class.methods {
        let mut func = Function::new([(2, ValType::I32)]);

        let frame_address_local = method.descriptor.args.len() as u32;
        let box_address_local = method.descriptor.args.len() as u32 + 1;

        //Allocate a RawFrame onto the heap
        func.instruction(&Instruction::I32Const(std::mem::size_of::<RawFrame>() as i32));
        func.instruction(&Instruction::Call(2));
        func.instruction(&Instruction::LocalSet(frame_address_local));

        //Box<[i64]> for the arguments
        func.instruction(&Instruction::I32Const((method.descriptor.args.len() * 8) as i32));
        func.instruction(&Instruction::Call(2));

        //Tee the address of the Box<[i64]>
        func.instruction(&Instruction::LocalTee(box_address_local));
        func.instruction(&Instruction::LocalGet(frame_address_local));
        //(*raw_frame_addr).locals = box.ptr;
        func.instruction(&Instruction::I32Store(MemArg {
            offset: std::mem::size_of::<usize>() as u64,
            align: 4,
            memory_index: 0,
        }));

        for (index, arg) in method.descriptor.args.iter().enumerate() {
            //Get the argument
            func.instruction(&Instruction::LocalGet(index as u32));

            match arg {
                //Heap type
                FieldType::Array { .. } | FieldType::Class(_) => {
                    //Use the frame_address_local as temporary storage
                    func.instruction(&Instruction::LocalGet(frame_address_local));

                    //Alloc the ref
                    func.instruction(&Instruction::Call(0));
                    func.instruction(&Instruction::LocalTee(frame_address_local));
                    //Store the ref in the table
                    func.instruction(&Instruction::TableSet(0));

                    //Store the index of the ref in the table
                    func.instruction(&Instruction::LocalGet(box_address_local));
                    func.instruction(&Instruction::I32Store(MemArg {
                        offset: (index * 8) as u64,
                        align: 4,
                        memory_index: 0,
                    }));

                    func.instruction(&Instruction::LocalSet(frame_address_local));
                },
                //Scalar type
                FieldType::Int | FieldType::Byte | FieldType::Short | FieldType::Char | FieldType::Boolean => {
                    func.instruction(&Instruction::LocalGet(box_address_local));
                    func.instruction(&Instruction::I32Store(MemArg {
                        offset: (index * 8) as u64,
                        align: 4,
                        memory_index: 0,
                    }));
                },
                _ => unimplemented!()
            }
        }

        //pub type ABIHandlePtr = unsafe extern "C" fn(&mut FrameStack, thread: &mut Thread) -> Operand;

        //Interpret the function
        func.instruction(&Instruction::LocalGet(frame_address_local));
        func.instruction(&Instruction::LocalGet(thread_local));
        func.instruction(&Instruction::Call(3));

        code.function(&func);
        funcs.function(types.len());
        types.function([], []);
    }

    module.section(&imports);
    module.section(&types);
    module.section(&funcs);
    module.section(&code);

    module
}