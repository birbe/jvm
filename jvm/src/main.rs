use jvm::bytecode::JValue;
use jvm::classfile::resolved::Class;

use jvm::heap::{Object};

use jvm::linker::ClassLoader;

use jvm::{JVM};

use parking_lot::{Mutex, RwLock};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::fs;
use std::io::{stdout};
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Instant;

use wasmtime::{Engine, Instance, Store};

extern crate jvm;

fn run(wasm: &[u8]) {
    let engine = Engine::default();
    let module = wasmtime::Module::from_binary(&engine, wasm).unwrap();
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[]).unwrap();

    let main_func = instance.get_func(&mut store, "main").unwrap();
    let _main = main_func.typed::<i32, i32>(&store).unwrap();
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
    let jvm = JVM::new(
        bootstrapper.clone() as Arc<dyn ClassLoader>,
        Mutex::new(Box::new(stdout())),
    );
    let _class = jvm
        .find_class("Main", bootstrapper.clone())
        // .find_class("net/minecraft/bundler/Main", bootstrapper.clone())
        .unwrap();

    let mut handle = jvm.create_thread(bootstrapper.clone());
    let now = Instant::now();

    let string = jvm.heap.allocate_string("OwO", &jvm);

    let string_array = jvm.heap.allocate_raw_object_array(&string.class, 1);

    unsafe { (&mut *string_array).body.body[0] = string.value };

    let array_object = unsafe { Object::from_raw(string_array) };

    println!("{}", array_object.get_raw() as usize);

    let result = handle
        .call(
            "Main",
            "main",
            "([Ljava/lang/String;)[Ljava/lang/String;",
            &[JValue::Reference(array_object)],
        )
        .unwrap();

    println!(
        "main(null) = {:?} in {}ms",
        result,
        Instant::now().duration_since(now).as_millis()
    );

    // let compiled = compile_class(&class, &jvm);
    // let wasm = compiled.module.finish();
    //
    // let wat = wasmprinter::print_bytes(&wasm).unwrap();
    //
    // wasmparser::validate(&wasm).unwrap();
    //
    // run(&wasm);
}
