#![feature(slice_ptr_get)]
#![feature(slice_ptr_len)]

use crate::bytecode::Bytecode;
use crate::classfile::resolved::attribute::Instruction;
use crate::classfile::resolved::{AccessFlags, Attribute, Class, Method, NameAndType, Ref};
use crate::execution::{ABIHandlePtr, ExecutionContext, MethodHandle};
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
pub mod native;

pub trait ObjectInternal {}

#[repr(C, u64)]
pub enum JArrayTag {
    Byte,
    Short,
    Int,
    Long,
    Float,
    Double,
    Class(*const Class)
}

#[repr(C)]
pub struct JArrayType {
    pub elements_are_array_reference: bool,
    pub tag: JArrayTag
}

#[repr(C)]
pub struct Object<T: ?Sized + ObjectInternal> {
    pub class: *const Class,
    pub body: T
}

#[repr(C)]
pub struct Array<T: ObjectInternal> {
    pub length: i32,
    pub data_type: JArrayType,
    pub body: *mut T
}

impl<T: ObjectInternal> ObjectInternal for Array<T> {}

impl ObjectInternal for f32 {}
impl ObjectInternal for f64 {}
impl ObjectInternal for i64 {}
impl ObjectInternal for u64 {}
impl ObjectInternal for i32 {}
impl ObjectInternal for u32 {}
impl ObjectInternal for i16 {}
impl ObjectInternal for u16 {}
impl ObjectInternal for i8 {}
impl ObjectInternal for u8 {}

pub struct Heap {
    pub data: *mut [u8],
}

impl Heap {
    
    pub fn new() -> Self {
        let mut box_ = Vec::<u8>::with_capacity(1024).into_boxed_slice();
        let ptr = Box::into_raw(box_);

        Self {
            data: ptr,
        }
    }

    pub fn allocate_object_typed<T: ?Sized + ObjectInternal>(class: &Class) -> *mut Object<T> {
        todo!()
    }

    pub fn allocate_class() {

    }
    
}

impl Drop for Heap {
    fn drop(&mut self) {
        unsafe {
            drop(Box::from_raw(self.data));
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

    pub fn generate_class(&self, classpath: &str, class_loader: Arc<dyn ClassLoader>) -> Result<Arc<Class>, ClassLoadError> {
        let bytes = class_loader.get_bytes(classpath).ok_or(ClassLoadError::ClassDefNotFound(classpath.into()))?;

        let classfile = ClassFile::from_bytes(Cursor::new(bytes)).map_err(|_| ClassLoadError::ParserError)?;

        let class = Arc::new(Class::init(&classfile, &self, class_loader.clone()).unwrap());

        class_loader.register_class(classpath, class.clone());

        self.register_refs(&*class_loader, &class);

        Ok(class)
    }

    pub fn find_class(&self, classpath: &str, class_loader: Arc<dyn ClassLoader>) -> Result<Arc<Class>, ClassLoadError> {
        let get = class_loader.get_class(classpath);
        match get {
            None => self.generate_class(classpath, class_loader.clone()),
            Some(get) => Ok(get)
        }
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

    fn register_refs(&self, loader: &dyn ClassLoader, class: &Class) {
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
                match native::link(&ref_) {
                    None => eprintln!("{ref_:?} could not be immediately linked and was ignored"),
                    Some(ptr) => {
                        method_refs.insert(ref_.clone(), MethodHandle {
                            ptr,
                            context: ExecutionContext::Native,
                            method_ref: ref_.clone(),
                        });
                    }
                }
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