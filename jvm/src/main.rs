use std::collections::HashMap;
use std::fmt::{Debug, format, Formatter};
use std::fs;
use std::io::Cursor;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Instant;
use parking_lot::RwLock;
use wasm_encoder::{CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction, Module, TypeSection, ValType};
use wasmtime::{Engine, Instance, Store};
use jvm::classfile::ClassFile;
use jvm::classfile::resolved::Class;
use jvm::jit::wasm::{compile_class, compile_method};
use jvm::{ClassLoadError, JVM};
use jvm::bytecode::JValue;
use jvm::heap::Object;
use jvm::linker::ClassLoader;
use jvm::thread::{Thread, ThreadHandle};
use jvm_types::JParse;

extern crate jvm;

fn run(wasm: &[u8]) {
    let engine = Engine::default();
    let module = wasmtime::Module::from_binary(&engine, wasm).unwrap();
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module,&[]).unwrap();

    let main_func = instance.get_func(&mut store, "main").unwrap();
    let main = main_func.typed::<i32, i32>(&store).unwrap();
    // main.call(&mut store, 0);
}

struct NativeClassLoader {
    pub classes: RwLock<HashMap<String, Arc<Class>>>,
}

impl Debug for NativeClassLoader {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "NativeClassLoader")
    }
}

impl NativeClassLoader {

    unsafe fn dropper(ptr: *const ()) {
        Arc::from_raw(ptr as *const NativeClassLoader);
    }

}

impl ClassLoader for NativeClassLoader {

    fn get_bytes(&self, classpath: &str) -> Option<Vec<u8>> {
        let mut path = PathBuf::new();
        path.push("./jvm/test_classes");
        path.push(format!("{classpath}.class"));

        fs::read(path).ok()
    }

    fn register_class(&self, classpath: &str, class: Arc<Class>) {
        let mut classes = self.classes.write();
        classes.insert(classpath.into(), class);
    }

    fn get_class(&self, classpath: &str) -> Option<Arc<Class>> {
        let classes = self.classes.read();
        classes.get(classpath).cloned()
    }

    fn id(&self) -> usize {
        0
    }
}

fn main() {
    let mock = NativeClassLoader {
        classes: Default::default(),
    };

    let bootstrapper = Arc::new(mock);

    let jvm = JVM::new(bootstrapper.clone() as Arc<dyn ClassLoader<>>);

    let string = jvm.heap.allocate_string("Hello world!", &jvm);

    unsafe {
        dbg!(&(*(*string.value).body.value).body.body[..]);
    }

    let class = jvm.find_class("Main", bootstrapper.clone()).unwrap();

    let mut handle = jvm.create_thread(bootstrapper.clone());
    let now = Instant::now();

    let result = handle.call("Main", "main", "([Ljava/lang/String;)[Ljava/lang/String;", &[
        JValue::Reference(Object::NULL)
    ]).unwrap();

    println!("main(null) = {:?} in {}ms", result, Instant::now().duration_since(now).as_millis());

    // let compiled = compile_class(&class, &jvm);
    // let wasm = compiled.module.finish();

    // let wat = wasmprinter::print_bytes(&wasm).unwrap();

    // wasmparser::validate(&wasm).unwrap();

    // run(&wasm);

}