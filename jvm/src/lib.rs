extern crate core;

use crate::bytecode::Bytecode;
use crate::classfile::resolved::attribute::Instruction;
use crate::classfile::resolved::{Attribute, Class, Method, Ref};
use crate::execution::MethodHandleStore;
use crate::thread::{MethodIdentifier, Thread};
use parking_lot::{Mutex, RwLock};
use std::collections::HashMap;
use std::fmt::Debug;
use std::fs::DirEntry;
use std::io::{Cursor, Stdout, stdout, Write};
use std::marker::PhantomData;
use std::pin::Pin;
use std::sync::Arc;
use jvm_types::JParse;
use crate::classfile::{ClassFile, ConstantInfo};

pub mod classfile;
mod tests;

pub mod bytecode;
pub mod execution;
pub mod jit;
pub mod thread;

pub struct Heap {
    pub data: Pin<Box<[u8]>>,
}

impl Heap {
    
    pub fn new() -> Self {
        Self {
            data: Pin::new(Vec::new().into_boxed_slice()),
        }
    }
    
}

pub trait ClassLoader: Send + Sync + Debug {

    fn get_class(&self, classpath: &str, jvm: &JVM) -> Option<Arc<Class>>;

    fn find_class(&self, classpath: &str, jvm: &JVM) -> Result<Arc<Class>, ClassLoadError>;

    fn generate_class(&self, classpath: &str, jvm: &JVM) -> Result<Arc<Class>, ClassLoadError>;

}

#[derive(Clone, Debug)]
pub enum ClassLoadError {
    ClassDefNotFound(String),
    MalformedClassDef,
    ParserError
}

pub enum RuntimeError {
    ClassNotLoaded,
    ClassLoadError(ClassLoadError)
}

pub struct JVM<'jvm> {
    pub phantom: PhantomData<&'jvm ()>,
    pub stdout: Mutex<Box<dyn Write>>,
    pub threads: RwLock<Vec<Thread>>,
    pub method_handles: MethodHandleStore<'jvm>,
    pub classes: RwLock<HashMap<String, Arc<Class>>>,
    pub heap: Heap,
    pub main_class_loader: Arc<dyn ClassLoader>
}

impl<'jvm> JVM<'jvm> {

    pub fn new(class_loader: Arc<dyn ClassLoader>) -> Self {
        Self {
            phantom: PhantomData::default(),
            stdout: Mutex::new(Box::new(Cursor::new(vec![]))),
            threads: Default::default(),
            method_handles: MethodHandleStore::new(),
            classes: Default::default(),
            heap: Heap::new(),
            main_class_loader: class_loader,
        }
    }

    ///This is not an instanceof check. This returns if the provided child class is specifically *below* the parent class in the inheritance graph.
    pub fn is_subclass(child: &Class, parent: &Class) -> bool {
        if &*child.this_class == "java/lang/Object" {
            return false;
        }

        if &*parent.this_class == "java/lang/Object" {
            return true;
        }

        let super_class = match &child.super_class {
            None => "",
            Some(super_class) => &super_class.this_class
        };

        if &*parent.this_class == super_class {
            return true
        } else {
            Self::is_subclass(&child.super_class.as_ref().unwrap(), parent)
        }
    }

    pub fn generate_class(&self, bytes: &[u8], class_loader: Arc<dyn ClassLoader>) -> Result<Arc<Class>, ClassLoadError> {
        let classfile = ClassFile::from_bytes(Cursor::new(bytes)).map_err(|_| ClassLoadError::ParserError)?;

        let class = Arc::new(Class::init(&classfile, &self, class_loader).unwrap());

        Ok(class)
    }

}

pub fn get_method_bytecode(jvm: &JVM, identifier: &MethodIdentifier) -> Vec<Instruction> {
    let classes = jvm.classes.read();
    let class = classes.get(identifier.class.to_str().unwrap()).unwrap();

    let method = class
        .methods
        .iter()
        .find(|method| *method.name == identifier.method.to_str().unwrap())
        .unwrap();

    let code = method.attributes.get("Code").unwrap();

    if let Attribute::Code(code) = code {
        code.instructions.clone()
    } else {
        unreachable!()
    }
}

#[repr(C)]
//Some of these functions are only designated for usage in Rust. If this is the case, they're marked as such
pub struct JVMPtrs<'jvm> {
    pub jvm: *mut JVM<'jvm>,
    //Rust only
    pub get_method_bytecode: fn(*mut JVM, MethodIdentifier) -> (Vec<Bytecode>, Vec<u8>),
}

pub fn routine_resolve_invokespecial(class: &Class, method_ref: &Arc<Ref>) {
    match class.get_method(&method_ref.name_and_type) {
        None => {}
        Some(other_method) => {

        }
    }
}