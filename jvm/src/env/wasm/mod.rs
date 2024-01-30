use std::alloc::{alloc, Layout};
use crate::classfile::resolved::{AccessFlags, Class, ClassId, Method, Ref, ReturnType};
use crate::env::{Compiler, Environment};
use crate::execution::{ExecutionContext, MethodHandle};
use crate::heap::Object;
use crate::linker::ClassLoader;
use crate::thread::{FrameStack, Operand, RawFrame, Thread};
use js_sys::WebAssembly;
use parking_lot::RwLock;
use std::collections::HashMap;
use std::mem::MaybeUninit;
use std::sync::Arc;
use wasm_bindgen::JsValue;
use wasm_encoder::Encode;
use crate::env::wasm::native::native_func;

pub static mut OBJECT_REF_SLOTS: MaybeUninit<RefSlots> = MaybeUninit::uninit();

pub mod cfg;
pub mod compile;
mod scc;
pub mod wasm;
mod stub;
mod native;

macro_rules! console_log {
    ($($t:tt)*) => (web_sys::console::log_1(&format_args!($($t)*).to_string().into()))
}

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
    // let refs_cell = REFS.get().unwrap();
    let mut refs = unsafe { OBJECT_REF_SLOTS.assume_init_mut() };

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
    let mut refs = unsafe { OBJECT_REF_SLOTS.assume_init_mut() };

    refs.available.push(index);
}

#[no_mangle]
pub extern "C" fn malloc(size: i32) -> i32 {
    unsafe { alloc(Layout::from_size_align(size as usize, 8).unwrap()) as i32 }
}

#[no_mangle]
pub extern "C" fn push_frame(stack: &mut FrameStack, locals_length: i32, class: &Class, method: &Method) -> i32 {
    stack.push(RawFrame::new(method, class, vec![Operand { data: 0 }; locals_length as usize].into_boxed_slice(), vec![Operand { data: 0 }; 256].into_boxed_slice()));
    unsafe { stack.frames.as_mut_ptr().add(1) as i32 }
}

#[no_mangle]
pub extern "C" fn free(ptr: i32, len: i32) {
    todo!()
}

pub struct WasmEnvironment {
    pub class_function_pointers: RwLock<HashMap<ClassId, HashMap<String, u32>>>,
    pub object_table: WebAssembly::Table
}

impl WasmEnvironment {
    pub fn new(object_table: WebAssembly::Table) -> Self {
        unsafe {
            OBJECT_REF_SLOTS.write(RefSlots {
                available: vec![],
                len: 0,
                table: object_table.clone(),
            });
        }

        Self {
            class_function_pointers: RwLock::new(HashMap::new()),
            object_table
        }
    }
}

impl Environment for WasmEnvironment {
    fn link_class(&self, class: Arc<Class>) {
        let stub = stub::create_stub_module(&class);

        let jvm_import_object = js_sys::Object::new();

        let meta_exports = wasm_bindgen::exports();
        let meta_memory = wasm_bindgen::memory();
        let func_table = WebAssembly::Table::from(wasm_bindgen::function_table());

        for cross_import in ["alloc_ref", "dealloc_ref", "malloc", "free", "interpret", "debug_framestack", "push_frame"] {
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

        console_log!(
            "Loading class {}",
            class.this_class
        );

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
                thread.frame_stack.push(RawFrame::new(&method_handle.method, &method_handle.class, args, Box::new([
                    Operand { data: 0 }; 512
                ])));

                let out = unsafe { (method_handle.ptr)(thread) };

                out
            }
            ExecutionContext::Native => {
                native_func(&method_handle)
            }
        };

        if matches!(method_handle.method.descriptor.return_type, ReturnType::Void) {
            None
        } else {
            Some(operand)
        }
    }

    fn create_method_handle(&self, class_loader: &dyn ClassLoader, ref_: Arc<Ref>, method: Arc<Method>, class: Arc<Class>) -> MethodHandle {
        let cfps = self.class_function_pointers.read();
        let class_pointers = cfps.get(&class.get_id()).unwrap();

        MethodHandle {
            ptr: unsafe { std::mem::transmute(*class_pointers.get(&format!("{}_abi", method.get_identifier())).unwrap()) },
            context: ExecutionContext::Compiled,
            class,
            method,
        }
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
