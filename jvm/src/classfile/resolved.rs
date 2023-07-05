use crate::classfile::attribute_info::CodeAttributeInfo;
use crate::classfile::constant::{
    ClassInfo, DynamicInfo, MethodHandleInfo, MethodTypeInfo, ModuleInfo, NameAndTypeInfo,
    PackageInfo, RefInfo, StringInfo, Utf8Info,
};
use crate::classfile::resolved::attribute::Code;
use crate::classfile::{
    AttributeInfo, ClassFile, ConstantInfo, ConstantInfoPool, MethodInfo,
};
use bitflags::{bitflags, Flags};

use discrim::FromDiscriminant;
use jvm_types::JParse;
use std::collections::HashMap;
use std::io::Cursor;
use std::sync::Arc;
use indexmap::IndexMap;
use crate::JVM;
use crate::linker::ClassLoader;

bitflags! {

    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct AccessFlags: u16 {
        const PUBLIC = 0x0001;
        const PRIVATE = 0x0002;
        const PROTECTED = 0x0004;
        const STATIC = 0x0008;
        const FINAL = 0x0010;
        const SYNCHRONIZED = 0x0020;
        const BRIDGE = 0x0040;
        const VARARGS = 0x0080;
        const NATIVE = 0x0100;
        const STRICT = 0x0800;
        const SUPER = 0x0020;
        const INTERFACE = 0x0300;
        const ABSTRACT = 0x0400;
        const SYNTHETIC = 0x1000;
        const ANNOTATION = 0x2000;
        const ENUM = 0x4000;
        const MODULE = 0x8000;
    }

}

#[derive(Debug)]
pub struct Class {
    pub constant_pool: ConstantPool,
    pub this_class: Arc<String>,
    pub super_class: Option<Arc<Class>>,
    pub access_flags: AccessFlags,
    // pub interfaces: Vec<Interface>,
    // pub fields: Vec<Field>,
    pub methods: Vec<Method>,

    pub class_loader: Arc<dyn ClassLoader>
    // pub attributes: Vec<Attribute>
}

impl Class {
    pub fn init(classfile: &ClassFile, jvm: &JVM, class_loader: Arc<dyn ClassLoader>) -> Option<Self> {
        let constant_pool =
            ConstantPool::from_constant_info_pool(classfile, &classfile.constant_pool)?;

        let this_class = resolve_string(&classfile.constant_pool, classfile.this_class)?;

        Some(Self {
            super_class: if &*this_class != "java/lang/Object" {
                Some(class_loader.find_class(&resolve_string(&classfile.constant_pool, classfile.super_class)?, jvm).unwrap())
            } else {
                None
            },
            this_class,
            access_flags: AccessFlags::from_bits(classfile.access_flags)?,
            methods: classfile
                .methods
                .iter()
                .map(|method_info| {
                    Method::new(method_info, &constant_pool, classfile)
                })
                .collect::<Option<Vec<Method>>>()?,
            constant_pool,
            class_loader
        })
    }

    pub fn get_method(&self, name_and_type: &NameAndType) -> Option<&Method> {
        self.methods.iter().find(|method| method.name == name_and_type.name && method.descriptor.string == *name_and_type.descriptor)
    }

}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum FieldType {
    Array {
        type_: Box<FieldType>,
        dimensions: usize
    },
    Byte,
    Char,
    Double,
    Float,
    Int,
    Long,
    Short,
    Boolean,
    Class(String)
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum ReturnType {
    FieldType(FieldType),
    Void
}

impl FieldType {

    pub fn from_str(slice: &str) -> (Self, usize) {
        (match &slice[0..1] {
            "B" => FieldType::Byte,
            "C" => FieldType::Char,
            "D" => FieldType::Double,
            "F" => FieldType::Float,
            "I" => FieldType::Int,
            "J" => FieldType::Long,
            "S" => FieldType::Short,
            "Z" => FieldType::Boolean,
            "[" => {
                let dimensions = slice.split("").skip(1).take_while(|char| *char == "[").map(|_| 1).sum();
                let inner = FieldType::from_str(&slice[dimensions..]);

                return (FieldType::Array {
                    type_: Box::new(inner.0),
                    dimensions,
                }, inner.1 + dimensions)
            }
            "L" => {
                let end = slice.find(";").unwrap();
                return (FieldType::Class(String::from(&slice[1..end])), end+1);
            },
            _ => panic!("Malformed method descriptor")
        }, 1)
    }

}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MethodDescriptor {
    pub args: Vec<FieldType>,
    pub return_type: ReturnType,
    pub string: String
}

impl TryFrom<&str> for MethodDescriptor {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let parameters_end = value.find(")").ok_or(())?;
        let parameters_str = &value[1..parameters_end];

        let mut idx = 1;

        let mut args = vec![];

        loop {
            if idx == parameters_end { break; }

            let (field_type, length) = FieldType::from_str(&value[idx..]);
            args.push(field_type);
            idx += length;
        }

        idx += 1;

        let return_type = match &value[idx..idx+1] {
            "V" => ReturnType::Void,
            _ => ReturnType::FieldType(FieldType::from_str(&value[idx..]).0),
        };

        Ok(Self {
            args,
            return_type,
            string: value.to_string(),
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Method {
    pub access_flags: AccessFlags,
    pub name: Arc<String>,
    pub descriptor: MethodDescriptor,
    pub attributes: HashMap<String, Attribute>,
}

impl Method {
    pub fn new(
        method_info: &MethodInfo,
        constant_pool: &ConstantPool,
        _classfile: &ClassFile,
    ) -> Option<Self> {
        Some(Self {
            access_flags: AccessFlags::from_bits(method_info.access_flags)?,
            name: constant_pool
                .constants
                .get(&method_info.name_index)?
                .as_string()?
                .clone(),
            descriptor: (&**constant_pool
                .constants
                .get(&method_info.descriptor_index)?
                .as_string()?)[..].try_into().ok()?,
            attributes: method_info
                .attributes
                .iter()
                .map(|attr| {
                    let descriptor_kind =
                        (**(constant_pool.constants.get(&attr.name_index)?.as_string()?)).clone();

                    let attribute = Attribute::new(&descriptor_kind, &attr, constant_pool)?;

                    Some((descriptor_kind, attribute))
                })
                .collect::<Option<HashMap<String, Attribute>>>()?,
        })
    }

    pub fn is_instance_initialization(&self, classpath: &str, class_loader: &dyn ClassLoader, jvm: &JVM) -> bool {
        !class_loader.find_class(classpath, jvm).unwrap().access_flags.contains(AccessFlags::INTERFACE)
            && &*self.name == "<init>"
            && self.descriptor.return_type == ReturnType::Void
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Attribute {
    ConstantValue,
    Code(Code),
    StackMapTable,
    BootstrapMethods,
    NestHost,
    NestMembers,
    PermittedSubclasses,
    Exceptions,
    InnerClasses,
    EnclosingMethod,
    Synthetic,
    Signature,
    Record,
    SourceFile,
    LineNumberTable,
    LocalVariableTable,
    LocalVariableTypeTable,
    Other(String, Vec<u8>),
}

impl Attribute {
    pub fn new(
        kind: &str,
        attribute_info: &AttributeInfo,
        constant_pool: &ConstantPool,
    ) -> Option<Self> {
        let mut cursor = Cursor::new(&attribute_info.info);

        Some(match kind {
            "Code" => Self::Code(Code::new(
                &CodeAttributeInfo::from_bytes(&mut cursor).ok()?,
                constant_pool,
            )?),
            _ => Self::Other(kind.to_string(), attribute_info.info.clone()),
        })
    }
}

pub mod attribute {
    use crate::bytecode::Bytecode;
    use crate::classfile::attribute_info::{CodeAttributeInfo, ExceptionTableInfo};
    use crate::classfile::resolved::{Attribute, ConstantPool};
    use std::io::Cursor;

    #[derive(Clone, Debug, PartialEq)]
    pub struct Instruction {
        pub bytecode: Bytecode,
        pub bytes_index: u32,
        pub bytecode_index: u32,
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct Code {
        pub attribute_name: &'static str,
        pub max_stack: u16,
        pub max_locals: u16,
        pub instructions: Vec<Instruction>,
        pub raw_instructions: Vec<u8>,
        pub exception_table: Vec<ExceptionTable>,
        pub attributes: Vec<Attribute>,
    }

    impl Code {
        pub fn new(code_info: &CodeAttributeInfo, constant_pool: &ConstantPool) -> Option<Self> {
            let mut cursor = Cursor::new(&code_info.code);
            let mut instructions = Vec::new();

            loop {
                let index = cursor.position();

                instructions.push(match Bytecode::from_bytes(&mut cursor) {
                    Ok(bytecode) => Instruction {
                        bytecode,
                        bytes_index: index as u32,
                        bytecode_index: instructions.len() as u32,
                    },
                    Err(_) => break,
                });
            }

            Some(Self {
                attribute_name: "Code",
                max_stack: code_info.max_stack,
                max_locals: code_info.max_locals,
                instructions,
                raw_instructions: code_info.code.clone(),
                exception_table: code_info
                    .exception_table
                    .iter()
                    .map(|exception_table_info| {
                        ExceptionTable::new(exception_table_info, constant_pool)
                    })
                    .collect::<Option<Vec<ExceptionTable>>>()?,
                attributes: code_info
                    .attributes
                    .iter()
                    .map(|attribute_info| {
                        let attribute_kind = constant_pool
                            .constants
                            .get(&attribute_info.name_index)?
                            .as_string()?;

                        Attribute::new(attribute_kind, attribute_info, constant_pool)
                    })
                    .collect::<Option<Vec<Attribute>>>()?,
            })
        }
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct ExceptionTable {
        pub start_pc: u16,
        pub end_pc: u16,
        pub handler_pc: u16,
        pub catch_type: String,
    }

    impl ExceptionTable {
        pub fn new(info: &ExceptionTableInfo, constant_pool: &ConstantPool) -> Option<Self> {
            Some(Self {
                start_pc: info.start_pc,
                end_pc: info.end_pc,
                handler_pc: info.handler_pc,
                catch_type: (**constant_pool.constants.get(&info.catch_type)?.as_string()?).clone(),
            })
        }
    }
}

pub fn resolve_string(constant_info_pool: &ConstantInfoPool, index: u16) -> Option<Arc<String>> {
    let constant = constant_info_pool.constants.get(&index)?;

    match constant {
        ConstantInfo::Class(ClassInfo { name_index }) => {
            resolve_string(&constant_info_pool, *name_index)
        }
        ConstantInfo::FieldRef(RefInfo {
            class_index: _,
            name_and_type_index: _,
        }) => None,
        ConstantInfo::MethodRef(RefInfo {
            class_index: _,
            name_and_type_index: _,
        }) => None,
        ConstantInfo::InterfaceMethodRef(RefInfo {
            class_index: _,
            name_and_type_index: _,
        }) => None,
        ConstantInfo::String(StringInfo { string_index }) => {
            resolve_string(&constant_info_pool, *string_index)
        }
        ConstantInfo::Integer(_)
        | ConstantInfo::Float(_)
        | ConstantInfo::Double(_)
        | ConstantInfo::Long(_) => None,
        ConstantInfo::NameAndType(_) => None,
        ConstantInfo::Utf8(Utf8Info { string }) => Some(string.clone()),
        ConstantInfo::MethodHandle(MethodHandleInfo {
            reference_kind: _,
            reference_index: _,
        }) => None,
        ConstantInfo::MethodType(MethodTypeInfo { descriptor_index }) => {
            resolve_string(&constant_info_pool, *descriptor_index)
        }
        ConstantInfo::Dynamic(_) | ConstantInfo::InvokeDynamic(_) => None,
        ConstantInfo::Module(ModuleInfo { name_index }) => {
            resolve_string(&constant_info_pool, *name_index)
        }
        ConstantInfo::Package(PackageInfo { name_index }) => {
            resolve_string(&constant_info_pool, *name_index)
        }
    }
}

fn resolve_constant(
    _classfile: &ClassFile,
    cip: &ConstantInfoPool,
    new_constants: &mut HashMap<u16, Constant>,
    slot: u16,
) -> Option<Constant> {
    let constant = cip.constants.get(&slot)?;

    if let Some(constant) = new_constants.get(&slot) {
        return Some(constant.clone());
    }

    let insert = match constant {
        ConstantInfo::Class(ClassInfo { name_index }) => {
            Constant::Class(resolve_string(cip, *name_index)?)
        }
        ConstantInfo::FieldRef(_) => Constant::FieldRef(Ref::resolve(cip, new_constants, slot)?),
        ConstantInfo::MethodRef(_) => Constant::MethodRef(Ref::resolve(cip, new_constants, slot)?),
        ConstantInfo::InterfaceMethodRef(_) => {
            Constant::MethodRef(Ref::resolve(cip, new_constants, slot)?)
        }
        ConstantInfo::String(StringInfo { string_index }) => {
            Constant::String(resolve_string(cip, *string_index)?)
        }
        ConstantInfo::Integer(int) => Constant::Integer(*int),
        ConstantInfo::Float(float) => Constant::Float(*float),
        ConstantInfo::Long(long) => Constant::Long(*long),
        ConstantInfo::Double(double) => Constant::Double(*double),
        ConstantInfo::NameAndType(_) => {
            Constant::NameAndType(NameAndType::resolve(cip, new_constants, slot)?)
        }
        ConstantInfo::Utf8(Utf8Info { string }) => Constant::Utf8(string.clone()),
        ConstantInfo::MethodHandle(MethodHandleInfo {
            reference_kind,
            reference_index,
        }) => Constant::MethodHandle(MethodHandle {
            reference_kind: ReferenceKind::from_discriminant(*reference_kind).ok()?,
            reference: Ref::resolve(cip, new_constants, *reference_index).unwrap(),
        }),
        ConstantInfo::MethodType(MethodTypeInfo { descriptor_index: _ }) => {
            Constant::MethodType(resolve_string(cip, slot)?)
        }
        ConstantInfo::Dynamic(DynamicInfo {
            bootstrap_method_attr_index: _,
            name_and_type_index,
        }) => Constant::Dynamic(Dynamic {
            bootstrap_method_attr: (),
            name_and_type: NameAndType::resolve(cip, new_constants, *name_and_type_index)?,
        }),
        ConstantInfo::InvokeDynamic(_) => todo!(),
        ConstantInfo::Module(_) => todo!(),
        ConstantInfo::Package(_) => todo!(),
    };

    new_constants.insert(slot, insert.clone());

    Some(insert)
}

#[derive(Debug)]
pub struct ConstantPool {
    pub constants: HashMap<u16, Constant>,
}

impl ConstantPool {
    pub fn from_constant_info_pool(classfile: &ClassFile, cip: &ConstantInfoPool) -> Option<Self> {
        let mut new_constants = HashMap::new();

        for (index, _constant_info) in &cip.constants {
            let constant = resolve_constant(classfile, cip, &mut new_constants, *index)?;
            new_constants.insert(*index, constant);
        }

        Some(Self {
            constants: new_constants,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    Class(Arc<String>),
    FieldRef(Arc<Ref>),
    MethodRef(Arc<Ref>),
    InterfaceMethodRef(Arc<Ref>),
    String(Arc<String>),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    NameAndType(Arc<NameAndType>),
    Utf8(Arc<String>),
    MethodHandle(MethodHandle),
    MethodType(Arc<String>),
    Dynamic(Dynamic),
    InvokeDynamic(Dynamic),
    // Module(constant::ModuleInfo),
    // Package(constant::PackageInfo)
}

impl Constant {
    pub fn as_string(&self) -> Option<&Arc<String>> {
        match self {
            Constant::String(string)
            | Constant::Class(string)
            | Constant::Utf8(string)
            | Constant::MethodType(string) => Some(string),
            _ => None,
        }
    }

    pub fn as_ref(&self) -> Option<&Arc<Ref>> {
        match self {
            Constant::MethodRef(ref_)
            | Constant::FieldRef(ref_)
            | Constant::InterfaceMethodRef(ref_) => {
                Some(ref_)
            }
            _ => None
        }
    }

}

#[derive(Clone, Debug, PartialEq)]
pub struct Dynamic {
    pub bootstrap_method_attr: (),
    pub name_and_type: Arc<NameAndType>,
}

#[derive(Copy, Clone, Ord, PartialOrd, PartialEq, Eq, Debug, FromDiscriminant)]
#[repr(u8)]
pub enum ReferenceKind {
    GetField = 1,
    GetStatic = 2,
    PutField = 3,
    PutStatic = 4,
    InvokeVirtual = 5,
    InvokeStatic = 6,
    InvokeSpecial = 7,
    NewInvokeSpecial = 8,
    InvokeInterface = 9,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MethodHandle {
    pub reference_kind: ReferenceKind,
    pub reference: Arc<Ref>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ref {
    pub class: Arc<String>,
    pub name_and_type: Arc<NameAndType>,
}

impl Ref {
    fn resolve(
        cip: &ConstantInfoPool,
        new_constants: &mut HashMap<u16, Constant>,
        slot: u16,
    ) -> Option<Arc<Self>> {
        if let Some(Constant::FieldRef(ref_))
        | Some(Constant::MethodRef(ref_))
        | Some(Constant::InterfaceMethodRef(ref_)) = new_constants.get(&slot)
        {
            return Some(ref_.clone());
        }

        let constant = cip.constants.get(&slot)?;

        Some(match constant {
            ConstantInfo::InterfaceMethodRef(RefInfo {
                class_index,
                name_and_type_index,
            })
            | ConstantInfo::FieldRef(RefInfo {
                class_index,
                name_and_type_index,
            })
            | ConstantInfo::MethodRef(RefInfo {
                class_index,
                name_and_type_index,
            }) => Arc::new(Ref {
                class: resolve_string(cip, *class_index)?,
                name_and_type: NameAndType::resolve(cip, new_constants, *name_and_type_index)?,
            }),
            _ => unreachable!(),
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NameAndType {
    pub name: Arc<String>,
    pub descriptor: Arc<String>,
}

impl NameAndType {
    fn resolve(
        cip: &ConstantInfoPool,
        new_constants: &mut HashMap<u16, Constant>,
        slot: u16,
    ) -> Option<Arc<Self>> {
        if let Some(Constant::NameAndType(arc)) = new_constants.get(&slot) {
            return Some(arc.clone());
        }

        let constant = cip.constants.get(&slot)?;

        Some(match constant {
            ConstantInfo::NameAndType(NameAndTypeInfo {
                name_index,
                descriptor_index,
            }) => Arc::new(NameAndType {
                name: resolve_string(cip, *name_index)?,
                descriptor: resolve_string(cip, *descriptor_index)?,
            }),
            _ => unreachable!(),
        })
    }
}
