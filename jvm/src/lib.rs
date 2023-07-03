use crate::bytecode::Bytecode;
use crate::classfile::resolved::attribute::Instruction;
use crate::classfile::resolved::{Attribute, Class};
use crate::execution::MethodHandleStore;
use crate::thread::{MethodIdentifier, Thread};
use parking_lot::{Mutex, RwLock};
use std::collections::HashMap;
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

pub trait ClassProvider {

    fn get_class(&self, classpath: &str) -> Option<Vec<u8>>;

}

pub struct JVM<'jvm> {
    pub phantom: PhantomData<&'jvm ()>,
    pub stdout: Mutex<Box<dyn Write>>,
    pub threads: RwLock<Vec<Thread>>,
    pub method_handles: MethodHandleStore<'jvm>,
    pub classes: RwLock<HashMap<String, Arc<Class>>>,
    pub heap: Heap,
    pub fs: Box<dyn ClassProvider + Send + Sync>
}

impl<'jvm> JVM<'jvm> {

    pub fn new(class_provider: Box<dyn ClassProvider + Send + Sync>) -> Self {
        Self {
            phantom: PhantomData::default(),
            stdout: Mutex::new(Box::new(Cursor::new(vec![]))),
            threads: Default::default(),
            method_handles: MethodHandleStore::new(),
            classes: Default::default(),
            heap: Heap::new(),
            fs: class_provider,
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

    pub fn get_class(&self, classpath: &str) -> Option<Arc<Class>> {
        let classes = self.classes.read();
        classes.get(classpath).cloned()
    }

    ///Will call get_class, and if it returns None, will call load_class
    pub fn find_class(&self, classpath: &str) -> Result<Arc<Class>, ClassLoadError> {
        match self.get_class(classpath) {
            Some(class) => Ok(class),
            None => self.load_class(classpath)
        }
    }

    ///Will always load a class, even if one with the same classpath was already loaded. This will also recursively load the superclasses if they aren't already loaded
    pub fn load_class(&self, classpath: &str) -> Result<Arc<Class>, ClassLoadError> {
        let bytes = self.fs.get_class(classpath).ok_or(ClassLoadError::ClassDefNotFound(classpath.to_string()))?;
        let classfile = ClassFile::from_bytes(Cursor::new(bytes)).map_err(|_| ClassLoadError::ParserError)?;

        let class = Arc::new(Class::init(&classfile, &self).unwrap());
        let mut classes = self.classes.write();
        classes.insert(classpath.to_string(), class.clone());

        Ok(class)
    }

}

pub fn get_method_bytecode(jvm: &JVM, identifier: &MethodIdentifier) -> Vec<Instruction> {
    let classes = jvm.classes.read();
    let class = classes.get(identifier.class.to_str().unwrap()).unwrap();

    let method = class
        .methods
        .iter()
        .find(|method| &*method.name == identifier.method.to_str().unwrap())
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
