use crate::classfile::attribute_info::{BootstrapMethodsAttributeInfo, CodeAttributeInfo};
use crate::classfile::constant::{
    ClassInfo, DynamicInfo, MethodHandleInfo, MethodTypeInfo, ModuleInfo, NameAndTypeInfo,
    PackageInfo, RefInfo, StringInfo, Utf8Info,
};
use crate::classfile::resolved::attribute::{BootstrapMethods, Code};
use crate::classfile::{
    AttributeInfo, ClassFile, ConstantInfo, ConstantInfoPool, FieldInfo, MethodInfo,
};
use bitflags::{bitflags, Flags};
use std::alloc::{alloc, Layout};

use crate::linker::ClassLoaderObject;
use crate::thread::{Operand, Thread};
use crate::{ClassContext, JVM};
use discrim::FromDiscriminant;
use jvm_types::JParse;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::io::Cursor;
use std::mem::size_of;
use std::sync::atomic::{AtomicU32, AtomicU8};
use std::sync::Arc;
use once_cell::sync::OnceCell;

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
        const INTERFACE = 0x0200;
        const ABSTRACT = 0x0400;
        const SYNTHETIC = 0x1000;
        const ANNOTATION = 0x2000;
        const ENUM = 0x4000;
        const MODULE = 0x8000;
    }

}

pub type ClassId = u32;

#[derive(Debug)]
pub struct Class {
    pub constant_pool: ConstantPool,
    pub this_class: Arc<String>,
    pub super_class: Option<Arc<Class>>,
    pub access_flags: AccessFlags,
    pub interfaces: Vec<Arc<Class>>,
    pub fields: Vec<Field>,
    pub methods: Vec<Arc<Method>>,

    pub statics: usize,
    pub static_init_state: AtomicU32,
    pub attributes: Vec<Attribute>,

    pub heap_size: usize,
    pub class_loader: u32,
    internal_id: ClassId,
}

impl Class {
    pub fn init(
        classfile: &ClassFile,
        class_context: &dyn ClassContext,
        internal_id: ClassId,
    ) -> Option<Self> {
        let constant_pool =
            ConstantPool::from_constant_info_pool(classfile, &classfile.constant_pool)?;

        let this_class = resolve_string(&classfile.constant_pool, classfile.this_class)?;

        let super_class = if &*this_class != "java/lang/Object" {
            Some(
                class_context.get_class(
                    &resolve_string(&classfile.constant_pool, classfile.super_class).unwrap()
                ).unwrap()
            )
        } else {
            None
        };

        let interfaces = classfile
            .interfaces
            .iter()
            .map(|classinfo| {
                let interface_classpath =
                    resolve_string(&classfile.constant_pool, classinfo.name_index).unwrap();
                class_context.get_class(
                    &interface_classpath
                ).unwrap()
            })
            .collect();

        let super_heap_size = match &super_class {
            None => 0,
            Some(super_class) => super_class.heap_size,
        };

        let mut index = 0;

        let fields = classfile
            .fields
            .iter()
            .map(|field_info| {
                let field = Field::new(
                    field_info,
                    &constant_pool,
                    super_heap_size + index * size_of::<Operand>(),
                );

                if !AccessFlags::from_bits(field_info.access_flags)
                    .unwrap()
                    .contains(AccessFlags::STATIC)
                {
                    index += 1;
                }

                field
            })
            .collect::<Option<Vec<Field>>>()?;

        let static_fields: Vec<&Field> = fields
            .iter()
            .filter(|field| field.access_flags.contains(AccessFlags::STATIC))
            .collect();

        let heap_size =
            super_heap_size + (fields.len() - static_fields.len()) * size_of::<Operand>();

        let static_alloc = unsafe {
            if static_fields.len() > 0 {
                let layout = Layout::from_size_align(
                    static_fields.len() * size_of::<Operand>(),
                    size_of::<Operand>(),
                )
                .unwrap();
                alloc(layout) as usize
            } else {
                0
            }
        };

        Some(Self {
            super_class,
            this_class,
            access_flags: AccessFlags::from_bits(classfile.access_flags)?,
            interfaces,
            fields,
            methods: classfile
                .methods
                .iter()
                .map(|method_info| {
                    Method::new(method_info, &constant_pool, classfile).map(Arc::new)
                })
                .collect::<Option<Vec<Arc<Method>>>>()?,
            statics: static_alloc,
            static_init_state: AtomicU32::new(0),
            attributes: classfile
                .attributes
                .iter()
                .map(|attribute_info| {
                    let attribute_kind = constant_pool
                        .constants
                        .get(&attribute_info.name_index)?
                        .as_string()?;

                    Some(Attribute::new(attribute_kind, attribute_info, &constant_pool).unwrap())
                })
                .collect::<Option<Vec<Attribute>>>()?,
            constant_pool,
            heap_size,
            class_loader: class_context.id(),
            internal_id
        })
    }

    pub fn get_method(&self, name_and_type: &NameAndType) -> Option<&Arc<Method>> {
        self.methods.iter().find(|method| {
            method.name == name_and_type.name
                && method.descriptor.string == *name_and_type.descriptor
        })
    }

    pub fn get_field(&self, name_and_type: &NameAndType) -> Option<&Field> {
        self.fields.iter().find(|field| {
            field.name == name_and_type.name && field.descriptor_string == *name_and_type.descriptor
        })
    }

    fn does_method_override(&self, ma: &Method, ca: &Class, mc: &Method) -> bool {
        let otherwise = (ma.access_flags & (AccessFlags::PUBLIC | AccessFlags::PROTECTED | AccessFlags::PRIVATE)).is_empty()
            && {
            let ca_last_slash = ca.this_class.rfind("/").unwrap_or(0);
            let this_last_slash = self.this_class.rfind("/").unwrap_or(0);

            let is_in_same_package = &ca.this_class[..ca_last_slash] == &self.this_class[..this_last_slash];

            is_in_same_package || todo!()
        };

        !mc.access_flags.contains(AccessFlags::STATIC)
            && mc.name == ma.name
            && mc.descriptor == ma.descriptor
            && (ma.access_flags.contains(AccessFlags::PUBLIC)
            || ma.access_flags.contains(AccessFlags::PROTECTED)
            || otherwise )
    }

    pub fn find_overriding_method(&self, ma: &Method, ca: &Class) -> Option<&Arc<Method>> {
        self.methods.iter().find(|mc| {
            self.does_method_override(ma, ca, &mc)
        })
    }

    pub fn parents(&self) -> Vec<Arc<Class>> {
        let mut parents = vec![];

        let mut current = self.super_class.as_ref();
        loop {
            if let Some(super_) = current {
                parents.push(super_.clone());
                current = super_.super_class.as_ref();
            } else {
                break;
            }
        }

        parents
    }

    pub(crate) fn get_id(&self) -> ClassId {
        self.internal_id
    }
}

impl Hash for Class {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write(self.this_class.as_bytes())
    }
}

impl PartialEq<Self> for Class {
    fn eq(&self, other: &Self) -> bool {
        &self.this_class == &other.this_class
    }
}

impl Eq for Class {}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum FieldType {
    Array {
        type_: Box<FieldType>,
        dimensions: usize,
    },
    Byte,
    Char,
    Double,
    Float,
    Int,
    Long,
    Short,
    Boolean,
    Class(String),
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum ReturnType {
    FieldType(FieldType),
    Void,
}

impl FieldType {
    pub fn from_str(slice: &str) -> (Self, usize) {
        (
            match &slice[0..1] {
                "B" => FieldType::Byte,
                "C" => FieldType::Char,
                "D" => FieldType::Double,
                "F" => FieldType::Float,
                "I" => FieldType::Int,
                "J" => FieldType::Long,
                "S" => FieldType::Short,
                "Z" => FieldType::Boolean,
                "[" => {
                    let dimensions = slice
                        .split("")
                        .skip(1)
                        .take_while(|char| *char == "[")
                        .map(|_| 1)
                        .sum();
                    let inner = FieldType::from_str(&slice[dimensions..]);

                    return (
                        FieldType::Array {
                            type_: Box::new(inner.0),
                            dimensions,
                        },
                        inner.1 + dimensions,
                    );
                }
                "L" => {
                    let end = slice.find(";").unwrap();
                    return (FieldType::Class(String::from(&slice[1..end])), end + 1);
                }
                _ => panic!("Malformed method descriptor {}", slice),
            },
            1,
        )
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MethodDescriptor {
    pub args: Vec<FieldType>,
    pub return_type: ReturnType,
    pub string: String,
}

impl TryFrom<&str> for MethodDescriptor {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let parameters_end = value.find(")").ok_or(())?;
        let _parameters_str = &value[1..parameters_end];

        let mut idx = 1;

        let mut args = vec![];

        loop {
            if idx == parameters_end {
                break;
            }

            let (field_type, length) = FieldType::from_str(&value[idx..]);
            args.push(field_type);
            idx += length;
        }

        idx += 1;

        let return_type = match &value[idx..idx + 1] {
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
    identifier: String,
}

impl Method {
    pub fn new(
        method_info: &MethodInfo,
        constant_pool: &ConstantPool,
        _classfile: &ClassFile,
    ) -> Option<Self> {
        let descriptor_string = (&**constant_pool
            .constants
            .get(&method_info.descriptor_index)?
            .as_string()?);

        let method_name = constant_pool
            .constants
            .get(&method_info.name_index)?
            .as_string()?;

        Some(Self {
            access_flags: AccessFlags::from_bits(method_info.access_flags)?,
            name: method_name.clone(),
            descriptor: descriptor_string[..].try_into().ok()?,
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
            identifier: format!("{}{}", method_name, descriptor_string),
        })
    }

    pub fn is_init(&self, classpath: &str, thread: &mut Thread) -> bool {
        let jvm = thread.jvm.clone();
        let class_loader = thread.class_loader.clone();

        !jvm.retrieve_class(classpath, &*class_loader, thread)
            .unwrap()
            .access_flags
            .contains(AccessFlags::INTERFACE)
            && &*self.name == "<init>"
            && self.descriptor.return_type == ReturnType::Void
    }

    pub fn is_signature_polymorphic(&self, classpath: &str) -> bool {
        classpath == "java/lang/invoke/MethodHandle"
            || classpath == "java/lang/invoke/VarHandle"
                && matches!(self.descriptor.args.first(), Some(FieldType::Class(classpath)) if classpath == "java/lang/Object")
                && self
                    .access_flags
                    .contains(AccessFlags::VARARGS | AccessFlags::NATIVE)
    }

    pub fn get_identifier(&self) -> &str {
        &self.identifier
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Attribute {
    ConstantValue,
    Code(Code),
    StackMapTable,
    BootstrapMethods(BootstrapMethods),
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
            "BootstrapMethods" => Self::BootstrapMethods(BootstrapMethods::new(
                &BootstrapMethodsAttributeInfo::from_bytes(&mut cursor)
                    .expect(&format!("{:?}", cursor)),
                constant_pool,
            )?),
            _ => Self::Other(kind.to_string(), attribute_info.info.clone()),
        })
    }
}

pub mod attribute {
    use crate::bytecode::Bytecode;
    use crate::classfile::attribute_info::{
        BootstrapMethodsAttributeInfo, CodeAttributeInfo, ExceptionTableInfo,
    };
    use crate::classfile::resolved::{Attribute, Constant, ConstantPool, MethodHandle};
    use std::io::Cursor;

    #[derive(Clone, Debug, PartialEq)]
    pub struct BootstrapMethod {
        pub method_ref: MethodHandle,
        pub arguments: Vec<Constant>,
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct BootstrapMethods {
        pub attribute_name: &'static str,
        pub bootstrap_methods: Vec<BootstrapMethod>,
    }

    impl BootstrapMethods {
        pub fn new(
            info: &BootstrapMethodsAttributeInfo,
            constant_pool: &ConstantPool,
        ) -> Option<Self> {
            Some(Self {
                attribute_name: "BootstrapMethods",
                bootstrap_methods: info
                    .bootstrap_methods
                    .iter()
                    .map(|info| {
                        Some(BootstrapMethod {
                            method_ref: constant_pool
                                .constants
                                .get(&info.bootstrap_method_ref)?
                                .as_method_handle()?
                                .clone(),
                            arguments: info
                                .bootstrap_arguments
                                .iter()
                                .map(|index| constant_pool.constants.get(index).unwrap().clone())
                                .collect(),
                        })
                    })
                    .collect::<Option<Vec<BootstrapMethod>>>()?,
            })
        }
    }

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
        pub fn new(info: &ExceptionTableInfo, _constant_pool: &ConstantPool) -> Option<Self> {
            Some(Self {
                start_pc: info.start_pc,
                end_pc: info.end_pc,
                handler_pc: info.handler_pc,
                // catch_type: (**constant_pool.constants.get(&info.catch_type)?.as_string()?).clone(),
                catch_type: String::new(),
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
            let string = resolve_string(cip, *name_index)?;
            let mut path = string.replace("[", "");

            let depth = (string.len() - path.len()) as u8;

            if depth > 0 {
                path = String::from(path.trim_start_matches("L"));
            }

            Constant::Class { path, depth }
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
            reference: Ref::resolve(cip, new_constants, *reference_index)?,
        }),
        ConstantInfo::MethodType(MethodTypeInfo {
            descriptor_index: _,
        }) => Constant::MethodType(resolve_string(cip, slot)?),
        ConstantInfo::Dynamic(DynamicInfo {
            bootstrap_method_attr_index,
            name_and_type_index,
        }) => Constant::Dynamic(Dynamic {
            bootstrap_method_attr: *bootstrap_method_attr_index,
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
    Class { path: String, depth: u8 },
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
            Constant::String(string) | Constant::Utf8(string) | Constant::MethodType(string) => {
                Some(string)
            }
            _ => None,
        }
    }

    pub fn as_ref(&self) -> Option<&Arc<Ref>> {
        match self {
            Constant::MethodRef(ref_)
            | Constant::FieldRef(ref_)
            | Constant::InterfaceMethodRef(ref_) => Some(ref_),
            _ => None,
        }
    }

    pub fn as_dynamic(&self) -> Option<&Dynamic> {
        match self {
            Constant::Dynamic(dynamic) | Constant::InvokeDynamic(dynamic) => Some(dynamic),
            _ => None,
        }
    }

    pub fn as_method_handle(&self) -> Option<&MethodHandle> {
        match self {
            Constant::MethodHandle(method_handle) => Some(method_handle),
            _ => None,
        }
    }

}

#[derive(Clone, Debug, PartialEq)]
pub struct Dynamic {
    pub bootstrap_method_attr: u16,
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

#[derive(Clone, Debug, PartialEq)]
pub struct Field {
    pub access_flags: AccessFlags,
    pub name: Arc<String>,
    pub descriptor: FieldType,
    pub descriptor_string: String,
    pub attributes: HashMap<String, Attribute>,
    pub heap_offset: usize,
}

impl Field {
    fn new(
        field_info: &FieldInfo,
        constant_pool: &ConstantPool,
        heap_offset: usize,
    ) -> Option<Self> {
        Some(Self {
            access_flags: AccessFlags::from_bits(field_info.access_flags)?,
            name: constant_pool
                .constants
                .get(&field_info.name_index)?
                .as_string()?
                .clone(),
            descriptor: FieldType::from_str(
                constant_pool
                    .constants
                    .get(&field_info.descriptor_index)?
                    .as_string()?,
            )
            .0,
            descriptor_string: (**constant_pool
                .constants
                .get(&field_info.descriptor_index)?
                .as_string()?)
            .clone(),
            attributes: field_info
                .attributes
                .iter()
                .map(|attr| {
                    let descriptor_kind =
                        (**(constant_pool.constants.get(&attr.name_index)?.as_string()?)).clone();

                    let attribute = Attribute::new(&descriptor_kind, &attr, constant_pool)?;

                    Some((descriptor_kind, attribute))
                })
                .collect::<Option<HashMap<String, Attribute>>>()?,
            heap_offset,
        })
    }
}
