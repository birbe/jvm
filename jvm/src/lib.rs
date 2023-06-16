struct JVM {

}

#[cfg(test)]
mod tests {
    use std::io::Cursor;
    use parse_macro::JParse;
    use jvm_types::JParse;

    #[derive(Debug, PartialEq, Eq, Clone, JParse)]
    struct TestStruct {
        pub test_field: u16,
        #[prefix = 2] pub vec: Vec<u16>
    }

    #[test]
    fn test() {
        let bytes = [0, 15, 0, 1, 0, 12];
        let test_struct = TestStruct::from_bytes(Cursor::new([0, 15, 0, 1, 0, 12])).unwrap();
        assert_eq!(test_struct, TestStruct {
            test_field: 15,
            vec: vec![12],
        });

        assert_eq!(test_struct, TestStruct::from_bytes(Cursor::new(test_struct.to_bytes())).unwrap());
    }

}