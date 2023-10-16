#![cfg(test)]

use jvm_types::JParse;
use parse_macro::JParse;
use std::io::Cursor;

#[test]
fn test_jparse() {
    #[derive(JParse, Debug, PartialEq, Eq, Clone)]
    struct TestStruct {
        pub test_field: u16,
        #[prefix = 2]
        pub vec: Vec<u16>,
    }

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
