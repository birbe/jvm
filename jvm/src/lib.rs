#![feature(slice_ptr_get)]
#![feature(slice_ptr_len)]

use crate::bytecode::Bytecode;
use crate::classfile::resolved::attribute::Instruction;
use crate::classfile::resolved::{AccessFlags, Attribute, Class, Method, NameAndType, Ref};
use crate::execution::{ExecutionContext, MethodHandle};
use crate::thread::{FrameStore, RawFrame, Thread, ThreadHandle};
use parking_lot::{Mutex, RwLock};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::fs::DirEntry;
use std::io::{Cursor, Stdout, stdout, Write};
use std::marker::PhantomData;
use std::pin::Pin;
use std::sync::Arc;
use bitflags::Flags;
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
    pub refs: RwLock<HashSet<Pin<Arc<Ref>>>>,
    pub method_refs: RwLock<HashMap<Arc<Ref>, MethodHandle>>
}

pub struct JVM {
    pub stdout: Mutex<Box<dyn Write>>,
    pub threads: RwLock<Vec<Arc<Mutex<Thread>>>>,
    pub heap: Heap,
    pub class_loaders: RwLock<Vec<Pin<Box<ClassLoaderStore>>>>,
}

impl JVM {

    pub fn new(class_loader: Arc<dyn ClassLoader>) -> Arc<Self> {
        Arc::new(Self {
            stdout: Mutex::new(Box::new(Cursor::new(vec![]))),
            threads: Default::default(),
            heap: Heap::new(),
            class_loaders: RwLock::new(vec![Pin::new(Box::new(ClassLoaderStore {
                loader: class_loader,
                refs: RwLock::new(HashSet::new()),
                method_refs: RwLock::new(HashMap::new()),
            }))]),
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

    pub fn register_refs(&self, loader: &dyn ClassLoader, class: &Class) {
        let mut stores = self.class_loaders.read();

        let store = &stores[loader.id()];
        let mut method_refs = store.method_refs.write();
        let mut refs = store.refs.write();

        for method in &class.methods {
            let ref_ = Arc::new(Ref {
                class: class.this_class.clone(),
                name_and_type: Arc::new(NameAndType { name: method.name.clone(), descriptor: Arc::new(method.descriptor.string.clone()) }),
            });

            refs.insert(Pin::new(ref_.clone()));

            let (max_stack, max_locals) = if let Some(Attribute::Code(code)) = method.attributes.get("Code") {
                (code.max_stack as usize, code.max_locals as usize)
            } else {
                (0, 0)
            };

            let ref_clone = ref_.clone();

            if !method.access_flags.contains(AccessFlags::NATIVE) {
                let method_handle = MethodHandle {
                    ptr: ThreadHandle::interpret_trampoline,
                    context: ExecutionContext::Interpret(Box::new(move |args| RawFrame::new(
                        &ref_clone.clone(),
                        args.iter().copied().chain(std::iter::repeat(0).take(max_locals - args.len())).collect::<Vec<u64>>().into_boxed_slice(),
                        vec![0u64; max_stack].into_boxed_slice(),
                    ))),
                    method_ref: ref_.clone()
                };

                method_refs.insert(ref_.clone(), method_handle);
            } else {
                eprintln!("Method {:?} was native and was ignored (not yet linked)", ref_);
            }
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