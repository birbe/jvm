use std::collections::HashMap;
use std::io::Cursor;
use wasm_encoder::{CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction, Module, TypeSection, ValType};
use wasmtime::{Engine, Instance, Store};
use jvm::classfile::ClassFile;
use jvm::classfile::resolved::Class;
use jvm::jit::wasm::{compile_class, compile_method};
use jvm::{ClassProvider, JVM};
use jvm_types::JParse;

extern crate jvm;

fn run(wasm: &[u8]) {
    let engine = Engine::default();
    let module = wasmtime::Module::from_binary(&engine, wasm).unwrap();
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module,&[]).unwrap();

    let java_func = instance.get_func(&mut store, "java").unwrap();
    let func_ref = java_func.typed::<i32, i32>(&store).unwrap();
    let result = func_ref.call(&mut store, 10).unwrap();
    println!("{}", result);
}

struct MockClassProvider {
    pub files: HashMap<String, Vec<u8>>
}

impl ClassProvider for MockClassProvider {

    fn get_class(&self, classpath: &str) -> Option<Vec<u8>> {
        self.files.get(classpath).cloned()
    }

}

fn main() {
    let mut files = HashMap::new();

    files.insert("java/lang/Object".to_string(), Vec::from(&include_bytes!("../test_classes/java/lang/Object.class")[..]));
    files.insert("java/lang/String".to_string(), Vec::from(&include_bytes!("../test_classes/java/lang/Object.class")[..]));
    files.insert("Main".to_string(), Vec::from(&include_bytes!("../test_classes/Main.class")[..]));


    let mock = MockClassProvider {
        files
    };

    let jvm = JVM::new(Box::new(mock));

    let class = jvm.load_class("Main").unwrap();

    let module = compile_class(&class);

    let wat = wasmprinter::print_bytes(&module.finish()).unwrap();
    println!("{}", wat);
}