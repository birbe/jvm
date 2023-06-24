#![cfg(test)]

use crate::bytecode::Bytecode;
use crate::classfile::resolved::attribute::Instruction;
use crate::classfile::resolved::{Attribute, Class};
use crate::classfile::ClassFile;
use crate::jit::{find_loops, ComponentNode, label_nodes};
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

// #[test]
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

#[test]
fn parse_classfile() {
    let file = include_bytes!("../test_classes/aab.class");

    let class_file = ClassFile::from_bytes(Cursor::new(file)).unwrap();

    let class = Class::init(&class_file).unwrap();

    let attribute = class.methods[3].attributes.get("Code").unwrap();

    if let Attribute::Code(code) = attribute {
        let (loops, nodes, scc_s) = find_loops(&code.instructions);

        let now = Instant::now();

        label_nodes(&loops, &nodes, &scc_s);

        println!("{} microseconds", Instant::now().duration_since(now).as_micros());
    }

    let debug = format!("{:#?}", class);

    std::fs::write("C:/Users/birb/Downloads/out.txt", debug).unwrap();
}

// #[test]
fn jit_test_manual() {
    let (loops, nodes, scc_s) = find_loops(&[
        Instruction {
            bytecode: Bytecode::Goto(5),
            bytes_index: 0,
            bytecode_index: 0,
        },
        Instruction {
            bytecode: Bytecode::Nop,
            bytes_index: 3,
            bytecode_index: 1,
        },
        Instruction {
            bytecode: Bytecode::Nop,
            bytes_index: 4,
            bytecode_index: 2,
        },
        Instruction {
            bytecode: Bytecode::Goto(-1),
            bytes_index: 5,
            bytecode_index: 3,
        },
        // Instruction {
        //     bytecode: Bytecode::Goto(-3),
        //     bytes_index: 5,
        //     bytecode_index: 3,
        // },
    ]);

    label_nodes(loops, nodes, scc_s);
}