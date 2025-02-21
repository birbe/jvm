use crate::bytecode::ArrayType;
use crate::classfile::resolved::{
    AccessFlags, Attribute, Class, ClassId, FieldType, Method, Ref, ReturnType,
};
use crate::env::wasm::jit::JitResult;
use crate::env::wasm::native::native_func;
use crate::env::wasm::stub::generate_helper_wasm;
use crate::env::wasm::stub::stub::create_stub_module;
use crate::env::Object;
use crate::env::{nothing, Compiler, Environment, ToAny};
use crate::execution::{ExecutionContext, MethodHandle};
use crate::linker::{routine_select_method, ClassLoader};
use crate::thread::{FrameStack, Operand, RawFrame, Thread, ThreadHandle};
use crate::JVM;
use highway::HighwayHasher;
use js_sys::{Function, WebAssembly};
use once_cell::sync::{Lazy, OnceCell};
use parking_lot::{Mutex, RwLock};
use std::alloc::{alloc_zeroed, dealloc, Layout};
use std::any::Any;
use std::collections::HashMap;
use std::hash::Hasher;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use wasm_bindgen::JsValue;
use wasm_encoder::Module;
use wasmparser::WasmFeatures;

pub static OBJECT_REF_SLOTS: OnceCell<Mutex<RefSlots>> = OnceCell::new();
pub static NULL_TABLE_SLOT: OnceCell<unsafe extern "C" fn(i32)> = OnceCell::new();
pub static OBJECT_REFS: Lazy<Mutex<Vec<u32>>> = Lazy::new(|| Mutex::new(vec![]));

macro_rules! console_log {
    ($($t:tt)*) => (web_sys::console::log_1(&format_args!($($t)*).to_string().into()))
}

mod native;
pub mod stub;

pub mod jit;
mod test;

#[derive(Debug)]
pub struct RefSlots {
    pub available: Vec<i32>,
    pub len: u32,
    pub table: WebAssembly::Table,
}

//TODO bad bad bad
unsafe impl Send for RefSlots {}
unsafe impl Sync for RefSlots {}

type MethodId = u32;

struct ClassReflector {
    pub field_accessors: HashMap<String, u32>,
    pub trampolines: HashMap<String, u32>,
}

#[no_mangle]
pub extern "C" fn init_static(class: &Class, thread: &mut Thread) {
    ThreadHandle::init_static(class, thread);
}

#[no_mangle]
pub extern "C" fn _jit_routine_select_method(
    object_class: &Class,
    thread: &mut Thread,
    method_id: u32,
    class: &Class,
) -> u32 {
    let method = &class.methods[method_id as usize];

    let (selected_method, selected_class) = routine_select_method(method, class, object_class);

    let env = (*thread.jvm.environment)
        .to_any()
        .downcast_ref::<WasmEnvironment>()
        .unwrap();
    let ptr = env.get_linked_function_ptr(selected_class, selected_method.get_identifier());
    ptr
}

#[no_mangle]
pub extern "C" fn alloc_ref() -> i32 {
    let refs_lock = OBJECT_REF_SLOTS.get().unwrap();
    let mut refs = refs_lock.lock();

    assert!(refs.len > 0);
    assert_ne!(refs.available.as_ptr() as i32, 0);

    let out = if refs.available.len() == 0 {
        refs.table.grow(1).unwrap();

        let len = refs.len;
        refs.len += 1;

        assert_ne!(len, 0);

        len as i32
    } else {
        refs.available.pop().unwrap()
    };

    out
}

#[no_mangle]
pub extern "C" fn dealloc_ref(index: *mut ()) {
    let refs_lock = OBJECT_REF_SLOTS.get().unwrap();
    let mut refs = refs_lock.lock();

    if refs.available.contains(&(index as i32)) {
        panic!("double free {}", index as u32);
    }

    let fn_ptr = unsafe { *NULL_TABLE_SLOT.get_unchecked() };
    unsafe {
        fn_ptr(index as i32);
    };

    if index as i32 != 0 {
        refs.available.push(index as i32);
    }
}

#[no_mangle]
pub extern "C" fn malloc(size: i32) -> i32 {
    unsafe { alloc_zeroed(Layout::from_size_align(size as usize, 8).unwrap()) as i32 }
}

#[no_mangle]
pub extern "C" fn push_frame(
    stack: &mut FrameStack,
    locals_length: i32,
    class: &Class,
    method: &Method,
) -> i32 {
    stack.push(RawFrame::new(
        method,
        class,
        (0..locals_length)
            .map(|_| Operand::new_data(0))
            .collect::<Vec<_>>()
            .into_boxed_slice(),
        (0..256)
            .map(|_| Operand::new_data(0))
            .collect::<Vec<_>>()
            .into_boxed_slice(),
    ));
    unsafe { stack.frames.as_mut_ptr().add(stack.frame_index as usize) as i32 }
}

#[no_mangle]
pub extern "C" fn debug(val: i32, _: i32) {
    console_log!("wasm debug {}", val);
}

#[no_mangle]
pub extern "C" fn free(ptr: i32, len: i32) {
    unsafe {
        dealloc(
            ptr as *mut u8,
            Layout::from_size_align(len as usize, 8).unwrap(),
        )
    }
}

pub struct WasmEnvironment {
    pub class_function_pointers: RwLock<HashMap<ClassId, HashMap<String, u32>>>,
    pub object_table: WebAssembly::Table,
    fn_get_object_class: unsafe extern "C" fn(i32) -> i32,
    fn_new_object_array: unsafe extern "C" fn(i32, i32, i64) -> i32,
    fn_set_object_array_elem: unsafe extern "C" fn(i32, i32, i32),
    fn_get_object_array_elem: unsafe extern "C" fn(i32, i32) -> i32,
    fn_array_length: unsafe extern "C" fn(i32) -> i32,
    fn_new_array: unsafe extern "C" fn(i32, i32, i32, i64) -> i32,

    fn_set_int_array_elem: unsafe extern "C" fn(i32, i32, i32),
    fn_set_char_array_elem: unsafe extern "C" fn(i32, i32, i32),
    fn_set_byte_array_elem: unsafe extern "C" fn(i32, i32, i32),
    fn_get_byte_array_elem: unsafe extern "C" fn(i32, i32) -> i64,

    fn_get_int_array_elem: unsafe extern "C" fn(i32, i32) -> i64,
    fn_get_char_array_elem: unsafe extern "C" fn(i32, i32) -> i64,
    fn_is_ref_null: unsafe extern "C" fn(i32) -> i32,
    fn_clone_reference: unsafe extern "C" fn(i32) -> i32,
    fn_ref_eq: unsafe extern "C" fn(i32, i32) -> i32,
    fn_obj_hash: unsafe extern "C" fn(i32) -> i64,

    fn_null_slot: unsafe extern "C" fn(i32),

    primitive_arrays: OnceCell<HashMap<ArrayType, Arc<Class>>>,

    object_id: Box<AtomicU64>,
}

impl WasmEnvironment {
    pub fn new() -> Self {
        let descriptor = js_sys::Object::new();

        js_sys::Reflect::set(&descriptor, &"element".into(), &"anyref".into()).unwrap();
        js_sys::Reflect::set(&descriptor, &"initial".into(), &"1".into()).unwrap();

        let object_table = WebAssembly::Table::new(&descriptor).unwrap();
        let meta_exports = wasm_bindgen::exports();

        OBJECT_REF_SLOTS
            .set(Mutex::new(RefSlots {
                available: vec![],
                len: 1,
                table: object_table.clone(),
            }))
            .unwrap();

        let helper_module = generate_helper_wasm();

        let wasm_bytes = helper_module.finish();

        let mut printer = wasmprinter::Printer::new();
        printer.print_offsets(true);

        let wat = printer.print(&wasm_bytes).unwrap();

        let uint8array = unsafe { js_sys::Uint8Array::view(&wasm_bytes) };

        let module = match WebAssembly::Module::new(&uint8array) {
            Ok(module) => module,
            Err(err) => {
                panic!("{wat}\n{err:?}")
            }
        };

        let imports = js_sys::Object::new();
        let jvm_import_object = js_sys::Object::new();

        js_sys::Reflect::set(&imports, &"jvm".into(), &jvm_import_object).unwrap();
        js_sys::Reflect::set(&jvm_import_object, &"objects".into(), &object_table).unwrap();
        js_sys::Reflect::set(
            &jvm_import_object,
            &"alloc_ref".into(),
            &js_sys::Reflect::get(&meta_exports, &"alloc_ref".into()).unwrap(),
        )
        .unwrap();
        js_sys::Reflect::set(
            &jvm_import_object,
            &"dealloc_ref".into(),
            &js_sys::Reflect::get(&meta_exports, &"dealloc_ref".into()).unwrap(),
        )
        .unwrap();

        let instance = WebAssembly::Instance::new(&module, &imports).unwrap();

        let exports = js_sys::Reflect::get(&instance, &"exports".into()).unwrap();
        let obj_class_func = js_sys::Reflect::get(&exports, &"obj_class".into()).unwrap();
        let new_object_array_func =
            js_sys::Reflect::get(&exports, &"new_object_array".into()).unwrap();
        let set_object_array_elem =
            js_sys::Reflect::get(&exports, &"set_object_array_element".into()).unwrap();
        let get_object_array_elem =
            js_sys::Reflect::get(&exports, &"get_object_array_element".into()).unwrap();
        let is_ref_null = js_sys::Reflect::get(&exports, &"is_ref_null".into()).unwrap();
        let clone_reference = js_sys::Reflect::get(&exports, &"clone_reference".into()).unwrap();
        let ref_eq = js_sys::Reflect::get(&exports, &"ref_eq".into()).unwrap();
        let new_array = js_sys::Reflect::get(&exports, &"new_array".into()).unwrap();
        let ia_store = js_sys::Reflect::get(&exports, &"int_array_store".into()).unwrap();
        let ca_store = js_sys::Reflect::get(&exports, &"char_array_store".into()).unwrap();

        let ia_load = js_sys::Reflect::get(&exports, &"int_array_load".into()).unwrap();
        let ca_load = js_sys::Reflect::get(&exports, &"char_array_load".into()).unwrap();

        let ba_load = js_sys::Reflect::get(&exports, &"byte_array_load".into()).unwrap();
        let ba_store = js_sys::Reflect::get(&exports, &"byte_array_store".into()).unwrap();

        let array_length = js_sys::Reflect::get(&exports, &"get_array_length".into()).unwrap();
        let obj_hash = js_sys::Reflect::get(&exports, &"obj_hash".into()).unwrap();
        let null_slot = js_sys::Reflect::get(&exports, &"null_slot".into()).unwrap();

        let get_object_class = Function::from(obj_class_func);
        let new_object_array = Function::from(new_object_array_func);
        let set_object_array = Function::from(set_object_array_elem);
        let get_object_array = Function::from(get_object_array_elem);
        let is_ref_null = Function::from(is_ref_null);
        let clone_reference = Function::from(clone_reference);
        let ref_eq = Function::from(ref_eq);
        let new_array = Function::from(new_array);
        let ia_store = Function::from(ia_store);
        let ca_store = Function::from(ca_store);
        let ia_load = Function::from(ia_load);
        let ca_load = Function::from(ca_load);
        let ba_load = Function::from(ba_load);
        let ba_store = Function::from(ba_store);
        let array_length = Function::from(array_length);
        let obj_hash = Function::from(obj_hash);
        let null_slot = Function::from(null_slot);

        let func_table = WebAssembly::Table::from(wasm_bindgen::function_table());
        let get_object_class_indirect_index = func_table.grow(17).unwrap();

        WebAssembly::Table::set(
            &func_table,
            get_object_class_indirect_index,
            &get_object_class,
        )
        .unwrap();
        WebAssembly::Table::set(
            &func_table,
            get_object_class_indirect_index + 1,
            &new_object_array,
        )
        .unwrap();
        WebAssembly::Table::set(
            &func_table,
            get_object_class_indirect_index + 2,
            &set_object_array,
        )
        .unwrap();
        WebAssembly::Table::set(&func_table, get_object_class_indirect_index + 3, &new_array)
            .unwrap();
        WebAssembly::Table::set(&func_table, get_object_class_indirect_index + 4, &ia_store)
            .unwrap();
        WebAssembly::Table::set(&func_table, get_object_class_indirect_index + 5, &ca_store)
            .unwrap();
        WebAssembly::Table::set(
            &func_table,
            get_object_class_indirect_index + 6,
            &array_length,
        )
        .unwrap();
        WebAssembly::Table::set(&func_table, get_object_class_indirect_index + 7, &ia_load)
            .unwrap();
        WebAssembly::Table::set(&func_table, get_object_class_indirect_index + 8, &ca_load)
            .unwrap();
        WebAssembly::Table::set(
            &func_table,
            get_object_class_indirect_index + 9,
            &get_object_array,
        )
        .unwrap();
        WebAssembly::Table::set(
            &func_table,
            get_object_class_indirect_index + 10,
            &is_ref_null,
        )
        .unwrap();
        WebAssembly::Table::set(
            &func_table,
            get_object_class_indirect_index + 11,
            &clone_reference,
        )
        .unwrap();
        WebAssembly::Table::set(&func_table, get_object_class_indirect_index + 12, &ref_eq)
            .unwrap();
        WebAssembly::Table::set(&func_table, get_object_class_indirect_index + 13, &ba_store)
            .unwrap();
        WebAssembly::Table::set(&func_table, get_object_class_indirect_index + 14, &ba_load)
            .unwrap();
        WebAssembly::Table::set(&func_table, get_object_class_indirect_index + 15, &obj_hash)
            .unwrap();
        WebAssembly::Table::set(
            &func_table,
            get_object_class_indirect_index + 16,
            &null_slot,
        )
        .unwrap();

        let fn_null_slot = unsafe { std::mem::transmute(get_object_class_indirect_index + 16) };
        NULL_TABLE_SLOT.set(fn_null_slot).unwrap();

        Self {
            class_function_pointers: RwLock::new(HashMap::new()),
            object_table,
            fn_get_object_class: unsafe { std::mem::transmute(get_object_class_indirect_index) },
            fn_obj_hash: unsafe { std::mem::transmute(get_object_class_indirect_index + 15) },
            fn_new_object_array: unsafe {
                std::mem::transmute(get_object_class_indirect_index + 1)
            },
            fn_set_object_array_elem: unsafe {
                std::mem::transmute(get_object_class_indirect_index + 2)
            },
            fn_get_object_array_elem: unsafe {
                std::mem::transmute(get_object_class_indirect_index + 9)
            },
            fn_new_array: unsafe { std::mem::transmute(get_object_class_indirect_index + 3) },
            fn_array_length: unsafe { std::mem::transmute(get_object_class_indirect_index + 6) },

            fn_set_int_array_elem: unsafe {
                std::mem::transmute(get_object_class_indirect_index + 4)
            },
            fn_set_char_array_elem: unsafe {
                std::mem::transmute(get_object_class_indirect_index + 5)
            },
            fn_set_byte_array_elem: unsafe {
                std::mem::transmute(get_object_class_indirect_index + 13)
            },
            fn_get_byte_array_elem: unsafe {
                std::mem::transmute(get_object_class_indirect_index + 14)
            },
            fn_get_int_array_elem: unsafe {
                std::mem::transmute(get_object_class_indirect_index + 7)
            },
            fn_get_char_array_elem: unsafe {
                std::mem::transmute(get_object_class_indirect_index + 8)
            },
            fn_is_ref_null: unsafe { std::mem::transmute(get_object_class_indirect_index + 10) },
            fn_clone_reference: unsafe {
                std::mem::transmute(get_object_class_indirect_index + 11)
            },
            fn_null_slot,
            fn_ref_eq: unsafe { std::mem::transmute(get_object_class_indirect_index + 12) },
            primitive_arrays: OnceCell::new(),
            object_id: Box::new(AtomicU64::new(1)),
        }
    }

    pub fn get_linked_function_ptr(&self, class: &Class, identifier: &str) -> u32 {
        let cfps = self.class_function_pointers.read();
        let class_pointers = cfps.get(&class.get_id()).unwrap();
        *class_pointers.get(identifier).unwrap()
    }

    fn link_jit(&self, jvm: &JVM, jit: JitResult) {
        let jvm_import_object = js_sys::Object::new();

        let meta_exports = wasm_bindgen::exports();
        let func_table = WebAssembly::Table::from(wasm_bindgen::function_table());

        for cross_import in ["init_static", "_jit_routine_select_method"] {
            let key: JsValue = cross_import.into();

            js_sys::Reflect::set(
                &jvm_import_object,
                &key,
                &js_sys::Reflect::get(&meta_exports, &key).unwrap(),
            )
            .unwrap();
        }

        js_sys::Reflect::set(
            &jvm_import_object,
            &"functions".into(),
            &wasm_bindgen::function_table(),
        )
        .unwrap();
        js_sys::Reflect::set(&jvm_import_object, &"objects".into(), &self.object_table).unwrap();
        js_sys::Reflect::set(
            &jvm_import_object,
            &"memory".into(),
            &wasm_bindgen::memory(),
        )
        .unwrap();

        let imports = js_sys::Object::new();

        js_sys::Reflect::set(&imports, &"jvm".into(), &jvm_import_object).unwrap();

        let wasm_bytes = jit.module.finish();

        let mut printer = wasmprinter::Printer::new();
        printer.print_offsets(true);

        let wat = printer.print(&wasm_bytes).unwrap();

        match wasmparser::Validator::new_with_features(WasmFeatures {
            gc: true,
            function_references: true,
            bulk_memory: true,
            ..Default::default()
        })
        .validate_all(&wasm_bytes)
        {
            Ok(_) => {}
            Err(error) => {
                console_log!("{error}\n{wat}");
            }
        }

        let uint8array = unsafe { js_sys::Uint8Array::view(&wasm_bytes) };

        let module = match WebAssembly::Module::new(&uint8array) {
            Ok(module) => module,
            Err(err) => {
                panic!("{wat}\n{err:?}")
            }
        };

        let instance = WebAssembly::Instance::new(&module, &imports).unwrap();

        let exports = js_sys::Reflect::get(&instance, &"exports".into()).unwrap();

        for (method, class, key) in jit.functions {
            let old_index = self.get_linked_function_ptr(class, method.get_identifier());
            console_log!(
                "linked jit func {} as {} @ {}",
                method.name,
                &key,
                old_index
            );

            let func = js_sys::Function::from(js_sys::Reflect::get(&exports, &key.into()).unwrap());

            WebAssembly::Table::set(&func_table, old_index, &func).unwrap();
        }
    }
}

impl Environment for WasmEnvironment {
    fn get_object_hash(&self, object: &Object) -> i32 {
        let mut hasher = HighwayHasher::new(Default::default());
        let id = unsafe { (self.fn_obj_hash)(object.ptr as i32) as i32 };
        hasher.write_i32(id);
        hasher.finish() as i32
    }

    fn store_primitive_classes(&self, primitives: HashMap<ArrayType, Arc<Class>>) {
        self.primitive_arrays.set(primitives).unwrap();
    }

    fn link_class(&self, class: Arc<Class>) {
        let jvm_import_object = js_sys::Object::new();

        let meta_exports = wasm_bindgen::exports();
        let meta_memory = wasm_bindgen::memory();
        let func_table = WebAssembly::Table::from(wasm_bindgen::function_table());

        let stubbable_methods = class
            .methods
            .iter()
            .filter(|method| !method.access_flags.contains(AccessFlags::NATIVE))
            .count();
        let stubbable_methods_offset =
            WebAssembly::Table::grow(&func_table, stubbable_methods as u32).unwrap();

        let stub = create_stub_module(&class, stubbable_methods_offset);

        for cross_import in [
            "alloc_ref",
            "dealloc_ref",
            "malloc",
            "free",
            "interpret",
            "debug_framestack",
            "push_frame",
            "debug",
        ] {
            let key: JsValue = cross_import.into();

            js_sys::Reflect::set(
                &jvm_import_object,
                &key,
                &js_sys::Reflect::get(&meta_exports, &key).unwrap(),
            )
            .unwrap();
        }

        js_sys::Reflect::set(&jvm_import_object, &"functions".into(), &func_table).unwrap();
        js_sys::Reflect::set(&jvm_import_object, &"objects".into(), &self.object_table).unwrap();
        js_sys::Reflect::set(&jvm_import_object, &"memory".into(), &meta_memory).unwrap();

        let imports = js_sys::Object::new();

        js_sys::Reflect::set(&imports, &"jvm".into(), &jvm_import_object).unwrap();

        let wasm_bytes = stub.wasm.finish();

        let uint8array = unsafe { js_sys::Uint8Array::view(&wasm_bytes) };

        let module = WebAssembly::Module::new(&uint8array).unwrap();

        //The WASM module contains an ElementSection which will automatically initialize the method function refs
        let instance = WebAssembly::Instance::new(&module, &imports).unwrap();

        let exports = js_sys::Reflect::get(&instance, &"exports".into()).unwrap();

        let mut funcs_to_link: Vec<String> = class
            .methods
            .iter()
            .filter(|method| !method.access_flags.contains(AccessFlags::NATIVE))
            .map(|method| {
                let identifier = method.get_identifier();

                format!("{}_abi", identifier)
            })
            .collect();

        funcs_to_link.extend(stub.accessors.clone());
        funcs_to_link.push("new".into());

        let table_length =
            WebAssembly::Table::grow(&func_table, funcs_to_link.len() as u32).unwrap();

        let mut ptrs: HashMap<String, u32> = funcs_to_link
            .into_iter()
            .enumerate()
            .map(|(index, func_name)| {
                let func = js_sys::Function::from(
                    js_sys::Reflect::get(&exports, &func_name.clone().into()).unwrap(),
                );

                let indirect_index = (index as u32) + table_length;
                WebAssembly::Table::set(&func_table, indirect_index, &func).unwrap();

                (func_name, indirect_index)
            })
            .collect();

        class
            .methods
            .iter()
            .filter(|method| !method.access_flags.contains(AccessFlags::NATIVE))
            .enumerate()
            .for_each(|(index, method)| {
                ptrs.insert(
                    (*method.identifier).clone(),
                    index as u32 + stubbable_methods_offset,
                );
            });

        let mut cfp = self.class_function_pointers.write();
        cfp.insert(class.get_id(), ptrs);
    }

    fn invoke_handle(
        &self,
        thread: &mut Thread,
        method_handle: &MethodHandle,
        mut args: Box<[Operand]>,
    ) -> Option<Operand> {
        let operand = match &method_handle.context {
            ExecutionContext::Interpret(_) => unreachable!(), //Unused on WASM
            ExecutionContext::Compiled => {
                let code = method_handle.method.attributes.get("Code").unwrap();

                let (max_locals, max_stack) = if let Attribute::Code(code) = code {
                    (code.max_locals as usize, code.max_stack as usize)
                } else {
                    panic!()
                };

                let mut locals = (0..max_locals)
                    .map(|_| Operand::new_data(0))
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                let stack = (0..max_stack)
                    .map(|_| Operand::new_data(0))
                    .collect::<Vec<_>>()
                    .into_boxed_slice();

                let (mut index, instance_index_offset) = if !method_handle
                    .method
                    .access_flags
                    .contains(AccessFlags::STATIC)
                {
                    //Instance method
                    locals[0] = std::mem::replace(&mut args[0], Operand::new_ref(0 as _));

                    (1, 1)
                } else {
                    (0, 0)
                };

                for (arg_index, type_) in method_handle.method.descriptor.args.iter().enumerate() {
                    match type_ {
                        FieldType::Double | FieldType::Long => {
                            let op = std::mem::replace(
                                &mut args[arg_index + instance_index_offset],
                                Operand::new_data(0),
                            )
                            .data();

                            locals[index] =
                                Operand::new_data((op & ((u32::MAX as u64) << 32)) as u32);
                            locals[index + 1] = Operand::new_data((op & (u32::MAX as u64)) as u32);

                            index += 2;
                        }
                        _ => {
                            let op = std::mem::replace(
                                &mut args[arg_index + instance_index_offset],
                                Operand::new_ref(0 as _),
                            );
                            locals[index] = op;

                            index += 1;
                        }
                    }
                }

                let frame =
                    RawFrame::new(&method_handle.method, &method_handle.class, locals, stack);
                thread.frame_stack.push(frame);

                let out = unsafe { method_handle.ptr.unwrap()(thread) };

                out
            }
            ExecutionContext::Native => Some(Box::new(native_func(&method_handle, thread, args))),
        };

        if matches!(
            method_handle.method.descriptor.return_type,
            ReturnType::Void
        ) {
            None
        } else {
            Some(*operand.unwrap())
        }
    }

    fn create_method_handle(
        &self,
        ref_: Arc<Ref>,
        method: Arc<Method>,
        class: Arc<Class>,
        class_loader: &dyn ClassLoader,
    ) -> MethodHandle {
        if method.access_flags.contains(AccessFlags::NATIVE) {
            return MethodHandle {
                ptr: None,
                context: ExecutionContext::Native,
                class,
                method,
                class_loader: class_loader.get_id(),
            };
        }

        let cfps = self.class_function_pointers.read();
        let class_pointers = cfps.get(&class.get_id()).unwrap();

        MethodHandle {
            ptr: unsafe {
                Some(std::mem::transmute(
                    *class_pointers
                        .get(&format!("{}_abi", method.get_identifier()))
                        .unwrap(),
                ))
            },
            context: ExecutionContext::Compiled,
            class,
            method,
            class_loader: 0,
        }
    }

    fn get_object_class<'a, 'b>(&'a self, object: &'b Object) -> &'b Class {
        assert!(!self.is_null(object));

        unsafe { std::mem::transmute((self.fn_get_object_class)(object.ptr as i32)) }
    }

    unsafe fn get_object_field(
        &self,
        object: &Object,
        class: &Class,
        field_name: &str,
        field_descriptor: &str,
    ) -> Operand {
        let ptrs = self.class_function_pointers.read();
        let class_ptrs = ptrs.get(&class.get_id()).unwrap();
        let get = class_ptrs.get(&format!("get_{}", field_name)).unwrap();

        match &field_descriptor[0..1] {
            "L" | "[" => {
                let get: unsafe fn(i32) -> i32 = unsafe { std::mem::transmute(*get) };
                let field_value = unsafe { get(object.ptr as i32) };
                Operand::new_ref(field_value as *mut ())
            }
            "I" | "Z" | "S" | "C" | "B" => {
                let get: unsafe fn(i32) -> i32 = unsafe { std::mem::transmute(*get) };
                let field_value = unsafe { get(object.ptr as i32) };
                Operand::new_data(field_value as u32)
            }
            "J" => {
                let get: unsafe fn(i32) -> i64 = unsafe { std::mem::transmute(*get) };
                let field_value = unsafe { get(object.ptr as i32) };
                Operand::new_data2(field_value as u64)
            }
            "D" => {
                let get: unsafe fn(i32) -> f64 = unsafe { std::mem::transmute(*get) };
                let field_value = unsafe { get(object.ptr as i32) };
                Operand::new_data2(field_value as u64)
            }
            "F" => {
                let get: unsafe fn(i32) -> f32 = unsafe { std::mem::transmute(*get) };
                let field_value = unsafe { get(object.ptr as i32) };
                Operand::new_data(field_value.to_bits())
            }
            _ => unimplemented!("field type getter {field_name} ({field_descriptor})"),
        }
    }

    unsafe fn set_object_field(
        &self,
        object: &Object,
        class: &Class,
        field_name: &str,
        field_descriptor: &str,
        value: Operand,
    ) {
        let ptrs = self.class_function_pointers.read();
        let class_ptrs = ptrs.get(&class.get_id()).unwrap();

        let set = class_ptrs.get(&format!("set_{}", field_name)).unwrap();

        match &field_descriptor[0..1] {
            "L" | "[" => {
                let set: unsafe fn(i32, i32) = unsafe { std::mem::transmute(*set) };
                unsafe { set(object.ptr as i32, value.ref_() as i32) };
            }
            "I" => {
                let set: unsafe fn(i32, i32) = unsafe { std::mem::transmute(*set) };
                unsafe { set(object.ptr as i32, value.data() as i32) };
            }
            "F" => {
                let set: unsafe fn(i32, f32) = unsafe { std::mem::transmute(*set) };
                unsafe { set(object.ptr as i32, f32::from_bits(value.data() as u32)) };
            }
            "J" => {
                let set: unsafe fn(i32, i64) = unsafe { std::mem::transmute(*set) };
                unsafe { set(object.ptr as i32, value.data() as i64) };
            }
            "Z" | "B" => {
                let set: unsafe fn(i32, i32) = unsafe { std::mem::transmute(*set) };
                unsafe { set(object.ptr as i32, value.data() as i32) };
            }
            _ => unimplemented!(
                "field type setter {} {}",
                &field_descriptor[0..1],
                field_name
            ),
        }
    }

    fn new_object_array(&self, class: &Class, size: i32) -> Object {
        assert!(class.is_array);

        let object_hash = self.object_id.fetch_add(1, Ordering::Relaxed);

        unsafe {
            Object::from_raw(
                (self.fn_new_object_array)(size, class as *const Class as i32, object_hash as i64)
                    as *mut (),
                dealloc_ref,
            )
        }
    }

    fn clone_reference(&self, object: &Object) -> Object {
        // unsafe { Object::from_raw(object.ptr, nothing) }

        unsafe {
            Object::from_raw(
                (self.fn_clone_reference)(object.ptr as i32) as *mut (),
                dealloc_ref,
            )
        }
    }

    fn is_null(&self, object: &Object) -> bool {
        unsafe { (self.fn_is_ref_null)(object.ptr as i32) == 1 }
    }

    fn new_array(&self, type_: ArrayType, size: i32) -> Object {
        let primitive_class = self.primitive_arrays.get().unwrap();
        let primitive = primitive_class.get(&type_).unwrap();

        let object_hash = self.object_id.fetch_add(1, Ordering::Relaxed);

        unsafe {
            Object::from_raw(
                (self.fn_new_array)(
                    (type_ as u8 - 4) as i32,
                    size,
                    &**primitive as *const Class as i32,
                    object_hash as i64,
                ) as *mut (),
                dealloc_ref,
            )
        }
    }

    fn get_array_length(&self, array: &Object) -> i32 {
        unsafe { (self.fn_array_length)(array.ptr as i32) }
    }

    unsafe fn set_array_element(
        &self,
        array_type: ArrayType,
        array: &Object,
        index: i32,
        value: Operand,
    ) {
        match array_type {
            ArrayType::Char => unsafe {
                (self.fn_set_char_array_elem)(array.ptr as i32, index, value.data() as i32);
            },
            ArrayType::Int => unsafe {
                (self.fn_set_int_array_elem)(array.ptr as i32, index, value.data() as i32);
            },
            ArrayType::Byte => unsafe {
                (self.fn_set_byte_array_elem)(array.ptr as i32, index, value.data() as i32);
            },
            _ => unimplemented!("{array_type:?}"),
        }
    }

    unsafe fn get_array_element(
        &self,
        array_type: ArrayType,
        array: &Object,
        index: i32,
    ) -> Operand {
        match array_type {
            ArrayType::Char => {
                Operand::new_data((self.fn_get_char_array_elem)(array.ptr as i32, index) as u32)
            }
            ArrayType::Int => {
                Operand::new_data((self.fn_get_int_array_elem)(array.ptr as i32, index) as u32)
            }
            ArrayType::Byte => {
                Operand::new_data((self.fn_get_byte_array_elem)(array.ptr as i32, index) as u32)
            }
            _ => unimplemented!("{array_type:?}"),
        }
    }

    unsafe fn set_object_array_element(&self, array: &Object, index: i32, value: Operand) {
        unsafe { (self.fn_set_object_array_elem)(array.ptr as i32, index, value.ref_() as i32) }
    }

    unsafe fn get_object_array_element(&self, array: &Object, index: i32) -> Object {
        unsafe {
            Object::from_raw(
                (self.fn_get_object_array_elem)(array.ptr as i32, index) as *mut (),
                dealloc_ref,
            )
        }
    }

    fn new_object(&self, class: &Class) -> Object {
        assert!(!class.is_array);

        let ptrs = self.class_function_pointers.read();
        let class_ptrs = ptrs.get(&class.get_id()).unwrap();
        let new = class_ptrs.get("new").unwrap();

        let hash_id = self.object_id.fetch_add(1, Ordering::Relaxed);

        let new_func: fn(i64) -> i32 = unsafe { std::mem::transmute(*new) };
        let object_id = new_func(hash_id as i64);
        unsafe { Object::from_raw(object_id as *mut (), dealloc_ref) }
    }

    unsafe fn object_from_operand(&self, operand: Operand) -> Object {
        Object {
            ptr: operand.ref_(),
            padding: [],
            drop: dealloc_ref,
        }
    }

    unsafe fn object_ref_from_operand(&self, operand: &Operand) -> &Object {
        &*(operand as *const Operand as *const Object)
    }

    fn object_ref_eq(&self, a: &Object, b: &Object) -> bool {
        (unsafe { (self.fn_ref_eq)(a.ptr as i32, b.ptr as i32) }) != 0
    }
}
