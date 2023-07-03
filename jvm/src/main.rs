use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::io::Cursor;
use std::sync::Arc;
use parking_lot::RwLock;
use wasm_encoder::{CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction, Module, TypeSection, ValType};
use wasmtime::{Engine, Instance, Store};
use jvm::classfile::ClassFile;
use jvm::classfile::resolved::Class;
use jvm::jit::wasm::{compile_class, compile_method};
use jvm::{ClassLoader, ClassLoadError, JVM};
use jvm_types::JParse;

extern crate jvm;

fn run(wasm: &[u8]) {
    let engine = Engine::default();
    let module = wasmtime::Module::from_binary(&engine, wasm).unwrap();
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module,&[]).unwrap();

    let approximate_sqrt = instance.get_func(&mut store, "approximate_sqrt").unwrap();
    let approximate_sqrt = approximate_sqrt.typed::<i32, i32>(&store).unwrap();

    let lcm = instance.get_func(&mut store, "lcm").unwrap();
    let lcm = lcm.typed::<(i32, i32), i32>(&store).unwrap();

    let input = 10;
    let result = approximate_sqrt.call(&mut store, input).unwrap();
    println!("approximate_sqrt({}) = {}", input, result);

    let input = (4892, 50000);
    let result = lcm.call(&mut store, input).unwrap();
    println!("lcm({}, {}) = {}", input.0, input.1, result);
}

#[derive(Debug)]
struct NativeClassLoader {
    pub files: HashMap<String, Vec<u8>>,
    pub classes: RwLock<HashMap<String, Arc<Class>>>,
    pub this: RwLock<Option<Arc<NativeClassLoader>>>
}

impl ClassLoader for NativeClassLoader {
    fn get_class(&self, classpath: &str, jvm: &JVM) -> Option<Arc<Class>> {
        let classes = self.classes.read();
        classes.get(classpath).cloned()
    }

    fn find_class(&self, classpath: &str, jvm: &JVM) -> Result<Arc<Class>, ClassLoadError> {
        let get = self.get_class(classpath, jvm);
        match get {
            None => self.generate_class(classpath, jvm),
            Some(get) => Ok(get)
        }
    }

    fn generate_class(&self, classpath: &str, jvm: &JVM) -> Result<Arc<Class>, ClassLoadError> {
        let bytes = self.files.get(classpath).unwrap();
        let this = self.this.read();
        let this = (*this).as_ref().unwrap().clone();
        jvm.generate_class(&bytes, this)
    }
}

fn main() {
    let mut files = HashMap::new();

    files.insert("java/lang/Object".to_string(), Vec::from(&include_bytes!("../test_classes/java/lang/Object.class")[..]));
    files.insert("java/lang/String".to_string(), Vec::from(&include_bytes!("../test_classes/java/lang/Object.class")[..]));
    files.insert("Main".to_string(), Vec::from(&include_bytes!("../test_classes/Main.class")[..]));


    let mock = NativeClassLoader {
        files,
        classes: Default::default(),
        this: RwLock::new(None),
    };

    let mock = Arc::new(mock);
    *mock.this.write() = Some(mock.clone());

    let jvm = JVM::new(mock.clone() as Arc<dyn ClassLoader>);

    let class = mock.find_class("Main", &jvm).unwrap();

    let compiled = compile_class(&class, &jvm);
    let wasm = compiled.module.finish();

    let wat = wasmprinter::print_bytes(&wasm).unwrap();
    println!("{}", wat);

    wasmparser::validate(&wasm).unwrap();

    run(&wasm);

}