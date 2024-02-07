use std::alloc::{alloc, Layout};
use crate::classfile::resolved::{AccessFlags, Class, ClassId, Method, Ref, ReturnType};
use crate::env::{Compiler, Environment};
use crate::execution::{ExecutionContext, MethodHandle};
use crate::env::Object;
use crate::linker::ClassLoader;
use crate::thread::{FrameStack, Operand, RawFrame, Thread, ThreadHandle};
use js_sys::{Function, WebAssembly};
use parking_lot::{Mutex, RwLock};
use std::collections::HashMap;
use std::mem::MaybeUninit;
use std::sync::Arc;
use once_cell::sync::Lazy;
use wasm_bindgen::JsValue;
use wasm_encoder::Encode;
use crate::env::wasm::compile::generate_helper_wasm;
use crate::env::wasm::compile::stub::create_stub_module;
use crate::env::wasm::native::native_func;

pub static mut OBJECT_REF_SLOTS: MaybeUninit<RefSlots> = MaybeUninit::uninit();
pub static OBJECT_REFS: Lazy<Mutex<Vec<u32>>> = Lazy::new(|| Mutex::new(vec![]));

macro_rules! console_log {
    ($($t:tt)*) => (web_sys::console::log_1(&format_args!($($t)*).to_string().into()))
}

pub mod cfg;
pub mod compile;
mod scc;
pub mod wasm;
mod native;

mod test;

pub struct RefSlots {
    pub available: Vec<i32>,
    pub len: u32,
    pub table: WebAssembly::Table,
}

type MethodId = u32;

struct ClassReflector {
    pub field_accessors: HashMap<String, u32>,
    pub trampolines: HashMap<String, u32>,
}

#[no_mangle]
pub extern "C" fn alloc_ref() -> i32 {
    let mut refs = unsafe { OBJECT_REF_SLOTS.assume_init_mut() };

    assert!(refs.len > 0);
    assert_ne!(refs.available.as_ptr() as i32, 0);

    if refs.available.len() == 0 {
        refs.table.grow(1).unwrap();

        let len = refs.len;
        refs.len += 1;

        assert_ne!(len, 0);

        len as i32
    } else {
        refs.available.pop().unwrap()
    }
}

#[no_mangle]
pub extern "C" fn dealloc_ref(index: *mut ()) {
    let mut refs = unsafe { OBJECT_REF_SLOTS.assume_init_mut() };

    if index as i32 != 0 {
        refs.available.push(index as i32);
    }
}

#[no_mangle]
pub extern "C" fn malloc(size: i32) -> i32 {
    unsafe { alloc(Layout::from_size_align(size as usize, 8).unwrap()) as i32 }
}

#[no_mangle]
pub extern "C" fn push_frame(stack: &mut FrameStack, locals_length: i32, class: &Class, method: &Method) -> i32 {
    stack.push(RawFrame::new(method, class, vec![Operand { data: 0 }; locals_length as usize].into_boxed_slice(), vec![Operand { data: 0 }; 256].into_boxed_slice()));
    unsafe { stack.frames.as_mut_ptr().add(stack.frame_index as usize) as i32 }
}

#[no_mangle]
pub extern "C" fn debug(loc: i32, val: i32) {
}

#[no_mangle]
pub extern "C" fn free(ptr: i32, len: i32) {
    todo!()
}

pub struct WasmEnvironment {
    pub class_function_pointers: RwLock<HashMap<ClassId, HashMap<String, u32>>>,
    pub object_table: WebAssembly::Table,
    fn_get_object_class: unsafe fn(i32) -> i32,
    fn_new_object_array: unsafe fn(i32, i32) -> i32,
    fn_set_object_array_elem: unsafe fn(i32, i32, i32),
    fn_array_length: unsafe fn(i32) -> i32,
    fn_new_array: unsafe fn(i32, i32) -> i32,

    fn_set_int_array_elem: unsafe fn(i32, i32, i32),
    fn_set_char_array_elem: unsafe fn(i32, i32, i32)
}

impl WasmEnvironment {
    pub fn new() -> Self {
        let descriptor = js_sys::Object::new();

        js_sys::Reflect::set(&descriptor, &"element".into(), &"anyref".into()).unwrap();
        js_sys::Reflect::set(&descriptor, &"initial".into(), &"1".into()).unwrap();

        let object_table = js_sys::WebAssembly::Table::new(&descriptor).unwrap();
        let meta_exports = wasm_bindgen::exports();

        unsafe {
            OBJECT_REF_SLOTS.write(RefSlots {
                available: vec![],
                len: 1,
                table: object_table.clone(),
            });
        }

        let helper_module = generate_helper_wasm();

        let wasm_bytes = helper_module.finish();

        let wat = wasmprinter::print_bytes(&wasm_bytes).unwrap();

        let uint8array = unsafe { js_sys::Uint8Array::view(&wasm_bytes) };

        let module = WebAssembly::Module::new(&uint8array).unwrap();

        let imports = js_sys::Object::new();
        let jvm_import_object = js_sys::Object::new();

        js_sys::Reflect::set(&imports, &"jvm".into(), &jvm_import_object).unwrap();
        js_sys::Reflect::set(&jvm_import_object, &"objects".into(), &object_table).unwrap();
        js_sys::Reflect::set(&jvm_import_object, &"alloc_ref".into(), &js_sys::Reflect::get(&meta_exports, &"alloc_ref".into()).unwrap()).unwrap();

        let instance = WebAssembly::Instance::new(&module, &imports).unwrap();

        let exports = js_sys::Reflect::get(&instance, &"exports".into()).unwrap();
        let obj_class_func = js_sys::Reflect::get(&exports, &"obj_class".into()).unwrap();
        let new_object_array_func = js_sys::Reflect::get(&exports, &"new_object_array".into()).unwrap();
        let set_object_array_elem = js_sys::Reflect::get(&exports, &"set_object_array_element".into()).unwrap();
        let new_array = js_sys::Reflect::get(&exports, &"new_array".into()).unwrap();
        let ia_store = js_sys::Reflect::get(&exports, &"int_array_store".into()).unwrap();
        let ca_store = js_sys::Reflect::get(&exports, &"char_array_store".into()).unwrap();

        let array_length = js_sys::Reflect::get(&exports, &"get_array_length".into()).unwrap();

        let get_object_class = Function::from(obj_class_func);
        let new_object_array = Function::from(new_object_array_func);
        let set_object_array = Function::from(set_object_array_elem);
        let new_array = Function::from(new_array);
        let ia_store = Function::from(ia_store);
        let ca_store = Function::from(ca_store);
        let array_length = Function::from(array_length);
        
        let func_table = WebAssembly::Table::from(wasm_bindgen::function_table());
        let get_object_class_indirect_index = func_table.grow(7).unwrap();
        
        WebAssembly::Table::set(&func_table, get_object_class_indirect_index, &get_object_class).unwrap();
        WebAssembly::Table::set(&func_table, get_object_class_indirect_index + 1, &new_object_array).unwrap();
        WebAssembly::Table::set(&func_table, get_object_class_indirect_index + 2, &set_object_array).unwrap();
        WebAssembly::Table::set(&func_table, get_object_class_indirect_index + 3, &new_array).unwrap();
        WebAssembly::Table::set(&func_table, get_object_class_indirect_index + 4, &ia_store).unwrap();
        WebAssembly::Table::set(&func_table, get_object_class_indirect_index + 5, &ca_store).unwrap();
        WebAssembly::Table::set(&func_table, get_object_class_indirect_index + 6, &array_length).unwrap();
        
        Self {
            class_function_pointers: RwLock::new(HashMap::new()),
            object_table,
            fn_get_object_class: unsafe { std::mem::transmute(get_object_class_indirect_index) },
            fn_new_object_array: unsafe { std::mem::transmute(get_object_class_indirect_index + 1) },
            fn_set_object_array_elem: unsafe { std::mem::transmute(get_object_class_indirect_index + 2) },
            fn_new_array: unsafe { std::mem::transmute(get_object_class_indirect_index + 3) },
            fn_array_length: unsafe { std::mem::transmute(get_object_class_indirect_index + 6) },

            fn_set_int_array_elem: unsafe { std::mem::transmute(get_object_class_indirect_index + 4) },
            fn_set_char_array_elem: unsafe { std::mem::transmute(get_object_class_indirect_index + 5) },
        }
    }
}

impl Environment for WasmEnvironment {
    fn link_class(&self, class: Arc<Class>) {
        console_log!(
            "Loading class {}",
            class.this_class
        );

        let stub = create_stub_module(&class);

        let jvm_import_object = js_sys::Object::new();

        let meta_exports = wasm_bindgen::exports();
        let meta_memory = wasm_bindgen::memory();
        let func_table = WebAssembly::Table::from(wasm_bindgen::function_table());

        for cross_import in ["alloc_ref", "dealloc_ref", "malloc", "free", "interpret", "debug_framestack", "push_frame", "debug"] {
            let key: JsValue = cross_import.into();

            js_sys::Reflect::set(
                &jvm_import_object,
                &key,
                &js_sys::Reflect::get(&meta_exports, &key).unwrap(),
            )
            .unwrap();
        }

        js_sys::Reflect::set(&jvm_import_object, &"objects".into(), &self.object_table).unwrap();
        js_sys::Reflect::set(&jvm_import_object, &"memory".into(), &meta_memory).unwrap();

        let imports = js_sys::Object::new();

        js_sys::Reflect::set(&imports, &"jvm".into(), &jvm_import_object).unwrap();

        let wasm_bytes = stub.wasm.finish();

        let uint8array = unsafe { js_sys::Uint8Array::view(&wasm_bytes) };

        let module = WebAssembly::Module::new(&uint8array).unwrap();
        let instance = WebAssembly::Instance::new(&module, &imports).unwrap();

        let exports = js_sys::Reflect::get(&instance, &"exports".into()).unwrap();

        let mut funcs_to_link: Vec<String> = class
            .methods
            .iter()
            .filter(|method| !method.access_flags.contains(AccessFlags::NATIVE))
            .map(|method| {
                let identifier = method.get_identifier();

                [identifier.to_string(), format!("{}_abi", identifier)]
            })
            .flatten()
            .collect();

        funcs_to_link.extend(stub.accessors.clone());
        funcs_to_link.push("new".into());

        let old_length = WebAssembly::Table::grow(&func_table, funcs_to_link.len() as u32).unwrap();

        let ptrs: HashMap<String, u32> = funcs_to_link
            .into_iter()
            .enumerate()
            .map(|(index, func_name)| {
                let func = js_sys::Function::from(
                    js_sys::Reflect::get(&exports, &func_name.clone().into()).unwrap(),
                );

                let indirect_index = (index as u32) + old_length;

                WebAssembly::Table::set(&func_table, indirect_index, &func).unwrap();

                (func_name, indirect_index)
            })
            .collect();

        let mut cfp = self.class_function_pointers.write();
        cfp.insert(class.get_id(), ptrs);
    }

    fn invoke_handle(
        &self,
        thread: &mut Thread,
        method_handle: &MethodHandle,
        args: Box<[Operand]>,
    ) -> Option<Operand> {
        let operand = match &method_handle.context {
            ExecutionContext::Interpret(_) => unreachable!(), //Unused on WASM
            ExecutionContext::Compiled => {
                let frame = RawFrame::new(&method_handle.method, &method_handle.class, args, Box::new([
                    Operand { data: 0 }; 10
                ]));
                thread.frame_stack.push(frame);

                let out = unsafe { (method_handle.ptr)(thread) };

                out
            }
            ExecutionContext::Native => {
                native_func(&method_handle, thread)
            }
        };

        if matches!(method_handle.method.descriptor.return_type, ReturnType::Void) {
            None
        } else {
            Some(operand)
        }
    }

    fn create_method_handle(&self, class_loader: &dyn ClassLoader, ref_: Arc<Ref>, method: Arc<Method>, class: Arc<Class>) -> MethodHandle {
        if method.access_flags.contains(AccessFlags::NATIVE) {
            return MethodHandle {
                ptr: unsafe { std::mem::transmute(0) },
                context: ExecutionContext::Native,
                class,
                method,
            };
        }

        let cfps = self.class_function_pointers.read();
        let class_pointers = cfps.get(&class.get_id()).unwrap();

        MethodHandle {
            ptr: unsafe { std::mem::transmute(*class_pointers.get(&format!("{}_abi", method.get_identifier())).unwrap()) },
            context: ExecutionContext::Compiled,
            class,
            method,
        }
    }

    fn new_string(&self, contents: &str, class_loader: &dyn ClassLoader, thread: &mut Thread) -> Object {
        let jvm = thread.jvm.clone();
        let string_class = jvm.find_class("java/lang/String", class_loader ,thread).unwrap();

        let string_object = self.new_object(&string_class);
        string_object
    }

    fn get_object_class<'a, 'b>(&'a self, object: &'b Object) -> &'b Class {
        unsafe { std::mem::transmute((self.fn_get_object_class)(object.ptr as i32)) }
    }

    unsafe fn get_object_field(&self, object: &Object, class: &Class, field_name: &str, field_descriptor: &str) -> Operand {
        let ptrs = self.class_function_pointers.read();
        let class_ptrs = ptrs.get(&class.get_id()).unwrap();
        let get = class_ptrs.get(&format!("get_{}", field_name)).unwrap();

        match &field_descriptor[0..1] {
            "L" => {
                let get: unsafe fn(i32) -> i32 = unsafe { std::mem::transmute(*get) };
                let field_value = unsafe { get(object.ptr as i32) };
                Operand {
                    objectref: field_value as *mut ()
                }
            },
            "I" => {
                let get: unsafe fn(i32) -> i32 = unsafe { std::mem::transmute(*get) };
                let field_value = unsafe { get(object.ptr as i32) };
                Operand {
                    data: field_value as u64
                }
            },
            _ => unimplemented!("field type getter {}", field_name)
        }
    }

    unsafe fn set_object_field(&self, object: &Object, class: &Class, field_name: &str, field_descriptor: &str, value: Operand) {
        let ptrs = self.class_function_pointers.read();
        let class_ptrs = ptrs.get(&class.get_id()).unwrap();

        let set = class_ptrs.get(&format!("set_{}", field_name)).unwrap();

        match &field_descriptor[0..1] {
            "L" | "[" => {
                let set: unsafe fn(i32, i32) = unsafe { std::mem::transmute(*set) };
                unsafe { set(object.ptr as i32, unsafe { value.objectref as i32 }) };
            },
            "I" => {
                let set: unsafe fn(i32, i32) = unsafe { std::mem::transmute(*set) };
                unsafe { set(object.ptr as i32, unsafe { value.data as i32 }) };
            },
            _ => unimplemented!("field type setter {} {}", &field_descriptor[0..1], field_name)
        }
    }

    fn new_object_array(&self, class: &Class, size: i32) -> Object {
        unsafe { Object::from_raw((self.fn_new_object_array)(size, class as *const Class as i32) as *mut (), None) }
    }

    fn new_array(&self, type_: i32, size: i32) -> Object {
        unsafe { Object::from_raw((self.fn_new_array)(type_, size) as *mut (), None) }
    }

    fn get_array_length(&self, array: &Object) -> i32 {
        unsafe { (self.fn_array_length)(array.ptr as i32) }
    }

    fn set_array_element(&self, array_type: u8, array: &Object, index: i32, value: Operand) {
        match array_type {
            5 => unsafe { (self.fn_set_char_array_elem)(array.ptr as i32, index, value.data as i32); }
            10 => unsafe { (self.fn_set_int_array_elem)(array.ptr as i32, index, value.data as i32); },
            4 | 5 | 6 | 7 | 8 | 9 | 11 => unimplemented!(),
            _ => unreachable!("Invalid array type")
        }
    }


    fn get_array_element(&self, array: &Object, index: i32) -> Operand {
        todo!()
    }

    fn set_object_array_element(&self, array: &Object, index: i32, value: Operand) {
        unsafe {
            (self.fn_set_object_array_elem)(array.ptr as i32, index, value.objectref as i32)
        }
    }

    fn new_object(&self, class: &Class) -> Object {
        let ptrs = self.class_function_pointers.read();
        let class_ptrs = ptrs.get(&class.get_id()).unwrap();
        let new = class_ptrs.get("new").unwrap();

        let new_func: fn() -> i32 = unsafe { std::mem::transmute(*new) };
        let object_id = new_func();

        unsafe { Object::from_raw(object_id as *mut (), Some(dealloc_ref)) }
    }

    unsafe fn object_from_operand(&self, operand: &Operand) -> Object {
        Object {
            ptr: unsafe { operand.objectref },
            drop: None,
        }
    }
}