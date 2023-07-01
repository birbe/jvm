use std::io::Cursor;
use wasm_encoder::{CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction, Module, TypeSection, ValType};
use jvm::classfile::ClassFile;
use jvm::classfile::resolved::Class;
use jvm::jit::wasm::new_method;
use jvm_types::JParse;

extern crate jvm;

fn parse_classfile() {
    let file = include_bytes!("../test_classes/Main.class");

    let class_file = ClassFile::from_bytes(Cursor::new(file)).unwrap();

    let class = Class::init(&class_file).unwrap();

    let mut module = Module::new();

    new_method(&class.methods[1], &mut module);

    let mut functions = FunctionSection::new();
    functions.function(0);

    module.section(&functions);

    let bytes = module.finish();
    let wat = wasmprinter::print_bytes(bytes).unwrap();
    println!("{}", wat);
}

fn main() {
    parse_classfile();
}