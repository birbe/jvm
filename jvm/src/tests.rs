#![cfg(test)]

use crate::classfile::resolved::Class;
use crate::classfile::ClassFile;
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

#[test]
fn parse_classfile() {
    let file = include_bytes!("../test_classes/Main.class");

    let class_file = ClassFile::from_bytes(Cursor::new(file)).unwrap();

    let class = Class::init(&class_file).unwrap();
    println!("{:#?}", class_file);
    println!("{:#?}", class);
}
