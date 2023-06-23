#![cfg(test)]

use crate::bytecode::Bytecode;
use crate::classfile::resolved::attribute::Instruction;
use crate::classfile::resolved::{Attribute, Class};
use crate::classfile::ClassFile;
use crate::jit::detect_irreducible_cfg;
use jvm_types::JParse;
use parse_macro::JParse;
use std::io::{stdout, Cursor};
use std::time::Instant;

#[derive(JParse, Debug, PartialEq, Eq, Clone)]
struct TestStruct {
    pub test_field: u16,
    #[prefix = 2]
    pub vec: Vec<u16>,
}

#[test]
fn test_jparse() {
    let test_struct = TestStruct::from_bytes(Cursor::new([0, 15, 0, 1, 0, 12])).unwrap();

    assert_eq!(
        test_struct,
        TestStruct {
            test_field: 15,
            vec: vec![12],
        }
    );

    assert_eq!(
        test_struct,
        TestStruct::from_bytes(Cursor::new(test_struct.to_bytes())).unwrap()
    );
}

// #[test]
fn parse_classfile() {
    let file = include_bytes!("../test_classes/Main.class");

    let class_file = ClassFile::from_bytes(Cursor::new(file)).unwrap();

    let class = Class::init(&class_file).unwrap();

    let debug = format!("{:#?}", class);

    std::fs::write("C:/Users/birb/Downloads/out.txt", debug).unwrap();
}

// #[test]
fn jit_test() {
    let file = include_bytes!("../test_classes/Main.class");

    let class_file = ClassFile::from_bytes(Cursor::new(file)).unwrap();

    let class = Class::init(&class_file).unwrap();

    let method = &class.methods[1];

    let attribute = if let Attribute::Code(code) = method.attributes.get("Code").unwrap() {
        code
    } else {
        unreachable!()
    };

    // detect_irreducible_cfg(&attribute.instructions);
}

#[test]
fn jit_test_manual() {
    detect_irreducible_cfg(&[
        //goto a;
        Instruction {
            bytecode: Bytecode::Goto(5),
            bytes_index: 0,
            bytecode_index: 0,
        },
        Instruction {
            bytecode: Bytecode::Aaload,
            bytes_index: 3,
            bytecode_index: 1,
        },
        //loop {
        Instruction {
            bytecode: Bytecode::Aaload,
            bytes_index: 4,
            bytecode_index: 2,
        },
        //label a;
        Instruction {
            bytecode: Bytecode::Aaload,
            bytes_index: 5,
            bytecode_index: 3,
        },
        Instruction {
            bytecode: Bytecode::Goto(-2),
            bytes_index: 6,
            bytecode_index: 4,
        },
        // }
    ]);
}
