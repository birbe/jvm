pub mod resolved;

use byteorder::{BigEndian, ReadBytesExt};
use jvm_types::JParse;
use parse_macro::JParse;
use std::collections::HashMap;
use std::io::{Error, Read};

#[derive(JParse, Clone, Debug, PartialEq)]
pub struct ClassFile {
    pub magic: u32,
    pub minor_version: u16,
    pub major_version: u16,
    pub constant_pool: ConstantInfoPool,
    pub access_flags: u16,
    pub this_class: u16,
    pub super_class: u16,
    pub interfaces: Vec<constant::ClassInfo>,
    pub fields: Vec<FieldInfo>,
    pub methods: Vec<MethodInfo>,
    pub attributes: Vec<AttributeInfo>,
}

#[derive(JParse, Clone, Debug, PartialEq)]
pub struct MethodInfo {
    pub access_flags: u16,
    pub name_index: u16,
    pub descriptor_index: u16,
    pub attributes: Vec<AttributeInfo>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConstantInfoPool {
    pub constants: HashMap<u16, ConstantInfo>,
}

impl JParse for ConstantInfoPool {
    type Output = ConstantInfoPool;

    fn from_bytes_prefixed<R: Read, const PREFIX: usize>(mut r: R) -> Result<Self::Output, Error> {
        let max_index = r.read_u16::<BigEndian>().unwrap() - 1;

        let mut constants = HashMap::new();

        let mut slot = 0;

        loop {
            let constant = ConstantInfo::from_bytes(&mut r)?;

            let inc = if matches!(constant, ConstantInfo::Long(_)) || matches!(constant, ConstantInfo::Double(_)) {
                2
            } else {
                1
            };

            constants.insert(slot + 1, constant);
            slot += inc;

            if max_index == slot { break };
        }

        Ok(Self { constants })
    }

    fn to_bytes_prefixed<const PREFIX: usize>(&self) -> Vec<u8> {
        let mut out = Vec::new();
        out.extend((self.constants.len() as u16 + 1u16).to_be_bytes());

        for index in 0..self.constants.len() {
            let constant = self.constants.get(&(index as u16 + 1)).unwrap();
            out.extend(constant.to_bytes());
        }

        out
    }
}

pub mod constant {
    use crate::classfile::AttributeInfo;
    use cesu8str::Cesu8String;
    use jvm_types::JParse;
    use parse_macro::JParse;
    use std::fmt::{Debug, Formatter};
    use std::io::{Error, Read};
    use std::sync::Arc;

    #[derive(JParse, Clone, Debug, PartialEq)]
    pub struct ClassInfo {
        pub name_index: u16,
    }

    #[derive(JParse, Clone, Debug, PartialEq)]
    pub struct RefInfo {
        pub class_index: u16,
        pub name_and_type_index: u16,
    }

    #[derive(JParse, Clone, Debug, PartialEq)]
    pub struct StringInfo {
        pub string_index: u16,
    }

    #[derive(JParse, Clone, Debug, PartialEq)]
    pub struct NameAndTypeInfo {
        pub name_index: u16,
        pub descriptor_index: u16,
    }

    #[derive(Clone, PartialEq)]
    pub struct Utf8Info {
        pub string: Arc<String>,
    }

    impl JParse for Utf8Info {
        type Output = Utf8Info;

        fn from_bytes_prefixed<R: Read, const PREFIX: usize>(
            mut r: R,
        ) -> Result<Self::Output, Error> {
            let vec = <Vec<u8>>::from_bytes(&mut r)?;
            //TODO proper error handling for JParse
            let string = Cesu8String::try_from_bytes(vec.clone()).unwrap();

            Ok(Self {
                string: Arc::new(string.into()),
            })
        }

        fn to_bytes_prefixed<const PREFIX: usize>(&self) -> Vec<u8> {
            let bytes = Cesu8String::from(&(*self.string)[..]).into_bytes();
            let mut out: Vec<u8> = (bytes.len() as u16).to_be_bytes().into();
            out.extend(bytes);
            out
        }
    }

    impl Debug for Utf8Info {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "Utf8Info {{ string: \"{}\" }}", self.string.to_string())
        }
    }

    #[derive(JParse, Clone, Debug, PartialEq)]
    pub struct MethodHandleInfo {
        pub reference_kind: u8,
        pub reference_index: u16,
    }

    #[derive(JParse, Clone, Debug, PartialEq)]
    pub struct MethodTypeInfo {
        pub descriptor_index: u16,
    }

    #[derive(JParse, Clone, Debug, PartialEq)]
    pub struct DynamicInfo {
        pub bootstrap_method_attr_index: u16,
        pub name_and_type_index: u16,
    }

    #[derive(JParse, Clone, Debug, PartialEq)]
    pub struct ModuleInfo {
        pub name_index: u16,
    }

    #[derive(JParse, Clone, Debug, PartialEq)]
    pub struct PackageInfo {
        pub name_index: u16,
    }

    #[derive(JParse, Clone, Debug, PartialEq)]
    pub struct FieldInfo {
        pub name_index: u16,
        pub descriptor_index: u16,
        pub attributes: Vec<AttributeInfo>,
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstantInfo {
    Class(constant::ClassInfo),
    FieldRef(constant::RefInfo),
    MethodRef(constant::RefInfo),
    InterfaceMethodRef(constant::RefInfo),
    String(constant::StringInfo),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    NameAndType(constant::NameAndTypeInfo),
    Utf8(constant::Utf8Info),
    MethodHandle(constant::MethodHandleInfo),
    MethodType(constant::MethodTypeInfo),
    Dynamic(constant::DynamicInfo),
    InvokeDynamic(constant::DynamicInfo),
    Module(constant::ModuleInfo),
    Package(constant::PackageInfo),
}

impl JParse for ConstantInfo {
    type Output = Self;

    fn from_bytes_prefixed<R: Read, const PREFIX: usize>(mut r: R) -> Result<Self::Output, Error> {
        let tag = r.read_u8()?;

        Ok(match tag {
            7 => Self::Class(constant::ClassInfo::from_bytes(&mut r)?),
            9 => Self::FieldRef(constant::RefInfo::from_bytes(&mut r)?),
            10 => Self::MethodRef(constant::RefInfo::from_bytes(&mut r)?),
            11 => Self::InterfaceMethodRef(constant::RefInfo::from_bytes(&mut r)?),
            8 => Self::String(constant::StringInfo::from_bytes(&mut r)?),
            3 => Self::Integer(i32::from_bytes(&mut r)?),
            4 => Self::Float(f32::from_bytes(&mut r)?),
            5 => Self::Long(i64::from_bytes(&mut r)?),
            6 => Self::Double(f64::from_bytes(&mut r)?),
            12 => Self::NameAndType(constant::NameAndTypeInfo::from_bytes(&mut r)?),
            1 => Self::Utf8(constant::Utf8Info::from_bytes(&mut r)?),
            15 => Self::MethodHandle(constant::MethodHandleInfo::from_bytes(&mut r)?),
            16 => Self::MethodType(constant::MethodTypeInfo::from_bytes(&mut r)?),
            17 => Self::Dynamic(constant::DynamicInfo::from_bytes(&mut r)?),
            18 => Self::Dynamic(constant::DynamicInfo::from_bytes(&mut r)?),
            19 => Self::Module(constant::ModuleInfo::from_bytes(&mut r)?),
            20 => Self::Package(constant::PackageInfo::from_bytes(&mut r)?),
            _ => Err(Error::from_raw_os_error(0))?,
        })
    }

    fn to_bytes_prefixed<const PREFIX: usize>(&self) -> Vec<u8> {
        todo!()
    }
}

#[derive(JParse, Clone, Debug, PartialEq)]
pub struct FieldInfo {
    pub access_flags: u16,
    pub name_index: u16,
    pub descriptor_index: u16,
    pub attributes: Vec<AttributeInfo>,
}

#[derive(JParse, Clone, Debug, PartialEq)]
pub struct AttributeInfo {
    pub name_index: u16,
    #[prefix = 4]
    pub info: Vec<u8>,
}

pub mod attribute_info {
    use crate::classfile::AttributeInfo;
    use parse_macro::JParse;

    #[derive(JParse, Clone, Debug, PartialEq)]
    pub struct CodeAttributeInfo {
        pub max_stack: u16,
        pub max_locals: u16,
        #[prefix = 4]
        pub code: Vec<u8>,
        pub exception_table: Vec<ExceptionTableInfo>,
        pub attributes: Vec<AttributeInfo>,
    }

    #[derive(JParse, Clone, Debug, PartialEq)]
    pub struct ExceptionTableInfo {
        pub start_pc: u16,
        pub end_pc: u16,
        pub handler_pc: u16,
        pub catch_type: u16,
    }
}
