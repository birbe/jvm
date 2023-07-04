#![feature(slice_ptr_get)]
#![feature(slice_ptr_len)]
extern crate core;

use crate::bytecode::Bytecode;
use crate::classfile::resolved::attribute::Instruction;
use crate::classfile::resolved::{Attribute, Class, Method, Ref};
use crate::execution::{MethodHandle};
use crate::thread::{FrameStore, Thread, ThreadHandle};
use parking_lot::{Mutex, RwLock};
use std::collections::HashMap;
use std::fmt::Debug;
use std::fs::DirEntry;
use std::io::{Cursor, Stdout, stdout, Write};
use std::marker::PhantomData;
use std::pin::Pin;
use std::sync::Arc;
use jvm_types::JParse;
use linker::ClassLoader;
use crate::classfile::{ClassFile, ConstantInfo};

pub mod classfile;
mod tests;

pub mod bytecode;
pub mod execution;
pub mod jit;
pub mod thread;
pub mod linker;

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

pub struct ClassLoaderStore {
    pub loader: Arc<dyn ClassLoader>,
    pub method_refs: RwLock<HashMap<Ref, MethodHandle>>
}

pub struct JVM {
    pub stdout: Mutex<Box<dyn Write>>,
    pub threads: RwLock<Vec<Arc<Mutex<Thread>>>>,
    pub heap: Heap,
    pub class_loaders: RwLock<Vec<ClassLoaderStore>>,
}

impl JVM {

    pub fn new(class_loader: Arc<dyn ClassLoader>) -> Arc<Self> {
        Arc::new(Self {
            stdout: Mutex::new(Box::new(Cursor::new(vec![]))),
            threads: Default::default(),
            heap: Heap::new(),
            class_loaders: RwLock::new(vec![ClassLoaderStore {
                loader: class_loader,
                method_refs: RwLock::new(HashMap::new()),
            }]),
        })
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

    pub fn create_thread(self: &Arc<JVM>, class_loader: Arc<dyn ClassLoader>) -> ThreadHandle {
        let thread = Arc::new(Mutex::new(Thread {
            jvm: self.clone(),
            class_loader,
            frame_store: Pin::new(Box::new(FrameStore::new())),
            killed: false,
        }));

        let mut threads = self.threads.write();
        threads.push(thread.clone());

        std::mem::forget(thread.lock());

        ThreadHandle {
            thread: thread.data_ptr(),
            guard: thread
        }
    }

}

pub fn routine_resolve_invokespecial(class: &Class, method_ref: &Arc<Ref>) {
    match class.get_method(&method_ref.name_and_type) {
        None => {}
        Some(other_method) => {

        }
    }
}