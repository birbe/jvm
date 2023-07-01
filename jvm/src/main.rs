use std::io::Cursor;
use wasm_encoder::{CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction, Module, TypeSection, ValType};
use wasmtime::{Engine, Instance, Store};
use jvm::classfile::ClassFile;
use jvm::classfile::resolved::Class;
use jvm::jit::wasm::new_method;
use jvm_types::JParse;

extern crate jvm;

fn parse_classfile() -> Vec<u8> {
    let file = include_bytes!("../test_classes/Main.class");

    let class_file = ClassFile::from_bytes(Cursor::new(file)).unwrap();

    let class = Class::init(&class_file).unwrap();

    let mut module = Module::new();
    let mut type_section = TypeSection::new();
    let mut function_section = FunctionSection::new();
    let mut code_section = CodeSection::new();

    let type_index = type_section.len();
    type_section.function([], [ValType::I32]);

    let func_index = code_section.len();

    new_method(&class.methods[1], &mut module, &mut function_section, &mut code_section, &mut type_section);

    let mut export_section = ExportSection::new();

    export_section.export("java", ExportKind::Func, func_index);

    module.section(&type_section);
    module.section(&function_section);
    module.section(&export_section);
    module.section(&code_section);

    let bytes = module.finish();

    let wat = wasmprinter::print_bytes(&bytes).unwrap();

    println!("{}", wat);

    wasmparser::validate(&bytes).unwrap();

    return bytes;
}

fn run(wasm: &[u8]) {
    let engine = Engine::default();
    let module = wasmtime::Module::from_binary(&engine, wasm).unwrap();
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module,&[]).unwrap();

    let java_func = instance.get_func(&mut store, "java").unwrap();
    let func_ref = java_func.typed::<(), i32>(&store).unwrap();
    let result = func_ref.call(&mut store, ()).unwrap();
    println!("{}", result);
}

fn main() {
    let wasm = parse_classfile();
    run(&wasm);
}