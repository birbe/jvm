use std::collections::HashMap;
use std::sync::Arc;
use cesu8str::{Cesu8Str, Cesu8String};
use discrim::FromDiscriminant;
use crate::classfile::{ClassFile, constant, ConstantInfo, ConstantInfoPool};
use crate::classfile::constant::{ClassInfo, DynamicInfo, MethodHandleInfo, MethodTypeInfo, ModuleInfo, NameAndTypeInfo, PackageInfo, RefInfo, StringInfo, Utf8Info};

#[derive(Debug)]
pub struct Class {
    pub constant_pool: ConstantPool,
    pub this_class: Arc<String>,
    pub super_class: Arc<String>,
    // pub interfaces: Vec<Interface>,
    // pub fields: Vec<Field>,
    // pub methods: Vec<Method>,
    // pub attributes: Vec<Attribute>
}

fn resolve_string(constant_info_pool: &ConstantInfoPool, index: u16) -> Option<Arc<String>> {
    let constant = constant_info_pool.constants.get(&index)?;

    match constant {
        ConstantInfo::Class(ClassInfo { name_index }) => resolve_string(&constant_info_pool, *name_index),
        ConstantInfo::FieldRef(RefInfo { class_index, name_and_type_index }) => None,
        ConstantInfo::MethodRef(RefInfo { class_index, name_and_type_index }) => None,
        ConstantInfo::InterfaceMethodRef(RefInfo { class_index, name_and_type_index }) => None,
        ConstantInfo::String(StringInfo { string_index }) => resolve_string(&constant_info_pool, *string_index),
        ConstantInfo::Integer(_) | ConstantInfo::Float(_) | ConstantInfo::Double(_) | ConstantInfo::Long(_) => None,
        ConstantInfo::NameAndType(_) => None,
        ConstantInfo::Utf8(Utf8Info { string }) => Some(string.clone()),
        ConstantInfo::MethodHandle(MethodHandleInfo { reference_kind, reference_index }) => None,
        ConstantInfo::MethodType(MethodTypeInfo { descriptor_index }) => resolve_string(&constant_info_pool, *descriptor_index),
        ConstantInfo::Dynamic(_) | ConstantInfo::InvokeDynamic(_) => None,
        ConstantInfo::Module(ModuleInfo { name_index }) => resolve_string(&constant_info_pool, *name_index),
        ConstantInfo::Package(PackageInfo { name_index }) => resolve_string(&constant_info_pool, *name_index)
    }
}

impl Class {

    pub fn init(classfile: &ClassFile) -> Option<Self> {
        
        Some(Self {
            constant_pool: ConstantPool::from_constant_info_pool(classfile, &classfile.constant_pool)?,
            this_class: resolve_string(&classfile.constant_pool, classfile.this_class)?,
            super_class: resolve_string(&classfile.constant_pool, classfile.super_class)?,
        })

    }

}

fn resolve_constant(classfile: &ClassFile, cip: &ConstantInfoPool, new_constants: &mut HashMap<u16, Constant>, slot: u16) -> Option<Constant> {
    let constant = cip.constants.get(&slot)?;

    if let Some(constant ) = new_constants.get(&slot) {
        return Some(constant.clone());
    }

    let insert = match constant {
        ConstantInfo::Class(ClassInfo { name_index }) => Constant::Class(resolve_string(cip, *name_index)?),
        ConstantInfo::FieldRef(_) => Constant::FieldRef(Ref::resolve(cip, new_constants, slot)?),
        ConstantInfo::MethodRef(_) => Constant::MethodRef(Ref::resolve(cip, new_constants, slot)?),
        ConstantInfo::InterfaceMethodRef(_) => Constant::MethodRef(Ref::resolve(cip, new_constants, slot)?),
        ConstantInfo::String(StringInfo { string_index }) => Constant::String(resolve_string(cip, *string_index)?),
        ConstantInfo::Integer(int) => Constant::Integer(*int),
        ConstantInfo::Float(float) => Constant::Float(*float),
        ConstantInfo::Long(long) => Constant::Long(*long),
        ConstantInfo::Double(double) => Constant::Double(*double),
        ConstantInfo::NameAndType(_) => Constant::NameAndType(NameAndType::resolve(cip, new_constants, slot)?),
        ConstantInfo::Utf8(Utf8Info { string }) => Constant::Utf8(string.clone()),
        ConstantInfo::MethodHandle(MethodHandleInfo { reference_kind, reference_index }) => Constant::MethodHandle(MethodHandle {
            reference_kind: ReferenceKind::from_discriminant(*reference_kind).ok()?,
            reference: Ref::resolve(cip, new_constants, *reference_index).unwrap(),
        }),
        ConstantInfo::MethodType(MethodTypeInfo { descriptor_index }) => Constant::MethodType(resolve_string(cip, slot)?),
        ConstantInfo::Dynamic(DynamicInfo { bootstrap_method_attr_index, name_and_type_index }) => Constant::Dynamic(Dynamic {
            bootstrap_method_attr: (),
            name_and_type: NameAndType::resolve(cip, new_constants, *name_and_type_index)?,
        }),
        ConstantInfo::InvokeDynamic(_) => todo!(),
        ConstantInfo::Module(_) => todo!(),
        ConstantInfo::Package(_) => todo!()
    };

    new_constants.insert(slot, insert.clone());

    Some(insert)
}

#[derive(Debug)]
pub struct ConstantPool {
    pub constants: HashMap<u16, Constant>
}

impl ConstantPool {

    pub fn from_constant_info_pool(classfile: &ClassFile, cip: &ConstantInfoPool) -> Option<Self> {
        let mut new_constants = HashMap::new();

        for (index, constant_info) in &cip.constants {
            let constant = resolve_constant(classfile, cip, &mut new_constants, *index)?;
            new_constants.insert(*index, constant);
        }

        Some(Self {
            constants: new_constants
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

#[derive(Clone, Debug, PartialEq)]
pub struct Dynamic {
    pub bootstrap_method_attr: (),
    pub name_and_type: Arc<NameAndType>
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
    InvokeInterface = 9
}

#[derive(Clone, Debug, PartialEq)]
pub struct MethodHandle {
    pub reference_kind: ReferenceKind,
    pub reference: Arc<Ref>
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ref {
    pub class: Arc<String>,
    pub name_and_type: Arc<NameAndType>
}

impl Ref {

    fn resolve(cip: &ConstantInfoPool, new_constants: &mut HashMap<u16, Constant>, slot: u16) -> Option<Arc<Self>> {
        if let Some(Constant::FieldRef(ref_)) | Some(Constant::MethodRef(ref_)) | Some(Constant::InterfaceMethodRef(ref_))= new_constants.get(&slot) {
            return Some(ref_.clone());
        }

        let constant = cip.constants.get(&slot)?;

        Some(match constant {
            ConstantInfo::InterfaceMethodRef(RefInfo { class_index, name_and_type_index })
            | ConstantInfo::FieldRef(RefInfo { class_index, name_and_type_index })
            | ConstantInfo::MethodRef(RefInfo { class_index, name_and_type_index }) =>
                Arc::new(Ref {
                    class: resolve_string(cip, *class_index)?,
                    name_and_type: NameAndType::resolve(cip, new_constants, *name_and_type_index)?,
                }),
            _ => unreachable!()
        })
    }

}

#[derive(Clone, Debug, PartialEq)]
pub struct NameAndType {
    pub name: Arc<String>,
    pub descriptor: Arc<String>
}

impl NameAndType {

    fn resolve(cip: &ConstantInfoPool, new_constants: &mut HashMap<u16, Constant>, slot: u16) -> Option<Arc<Self>> {
        if let Some(Constant::NameAndType(arc)) = new_constants.get(&slot) {
            return Some(arc.clone());
        }

        let constant = cip.constants.get(&slot)?;

        Some(match constant {
            ConstantInfo::NameAndType(NameAndTypeInfo { name_index, descriptor_index }) => Arc::new(NameAndType {
                name: resolve_string(cip, *name_index)?,
                descriptor: resolve_string(cip, *descriptor_index)?,
            }),
            _ => unreachable!()
        })
    }

}