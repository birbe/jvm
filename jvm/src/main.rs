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

#[derive(Debug)]
struct NativeClassLoader {
    pub classes: RwLock<HashMap<String, Arc<Class>>>,
    pub this: RwLock<Option<Arc<NativeClassLoader>>>
}

impl ClassLoader for NativeClassLoader {
    fn get_class(&self, classpath: &str) -> Option<Arc<Class>> {
        let classes = self.classes.read();
        classes.get(classpath).cloned()
    }

    fn find_class(&self, classpath: &str, jvm: &JVM) -> Result<Arc<Class>, ClassLoadError> {
        let get = self.get_class(classpath);
        match get {
            None => self.generate_class(classpath, jvm),
            Some(get) => Ok(get)
        }
    }

    fn generate_class(&self, classpath: &str, jvm: &JVM) -> Result<Arc<Class>, ClassLoadError> {
        let mut path = PathBuf::new();
        path.push("./jvm/test_classes");
        path.push(format!("{classpath}.class"));

        let bytes = fs::read(path).map_err(|_| ClassLoadError::ClassDefNotFound(classpath.into()))?;

        let this = self.this.read();
        let this = (*this).as_ref().unwrap().clone();
        let class = jvm.generate_class(&bytes, this)?;

        {
            let mut classes = self.classes.write();
            classes.insert(classpath.into(), class.clone());
        }

        jvm.register_refs(self, &class);

        Ok(class)
    }

    fn id(&self) -> usize {
        0
    }
}

fn main() {
    let mock = NativeClassLoader {
        classes: Default::default(),
        this: RwLock::new(None),
    };

    let mock = Arc::new(mock);
    *mock.this.write() = Some(mock.clone());

    let jvm = JVM::new(mock.clone() as Arc<dyn ClassLoader>);

    let class = mock.find_class("Main", &jvm).unwrap();

    let mut handle = jvm.create_thread(mock.clone());
    let result = handle.call("Main", "main", "([Ljava/lang/String;)[Ljava/lang/String;", &[
        JValue::Reference(0 as *mut ())
    ]).unwrap();
    println!("{:?}", result);

    let now = Instant::now();
    let compiled = compile_class(&class, &jvm);
    println!("{} microseconds to compile class", Instant::now().duration_since(now).as_micros());

    let wasm = compiled.module.finish();

    let wat = wasmprinter::print_bytes(&wasm).unwrap();

    // wasmparser::validate(&wasm).unwrap();

    run(&wasm);

}