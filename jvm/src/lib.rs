#![feature(slice_ptr_get)]
#![feature(slice_ptr_len)]
#![feature(ptr_metadata)]
#![feature(pointer_byte_offsets)]

use crate::classfile::resolved::{AccessFlags, Attribute, Class, Field, NameAndType, Ref};
use crate::classfile::{ClassFile};
use crate::execution::{ExecutionContext, MethodHandle};
use crate::thread::{FrameStack, Operand, RawFrame, Thread, ThreadHandle};
use bitflags::Flags;

use heap::{Heap};
use jvm_types::JParse;
use linker::ClassLoader;
use parking_lot::{Mutex, RwLock};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;

use std::io::{Cursor, Write};



use std::pin::Pin;

use std::sync::Arc;

pub mod classfile;
mod tests;

pub mod bytecode;
pub mod execution;
pub mod heap;
pub mod jit;
pub mod linker;
pub mod native;
pub mod thread;

#[derive(Clone, Debug)]
pub enum ClassLoadError {
    ClassDefNotFound(String),
    MalformedClassDef,
    ParserError,
}

pub enum RuntimeError {
    ClassNotLoaded,
    ClassLoadError(ClassLoadError),
}

pub struct ClassLoaderStore {
    pub loader: Arc<dyn ClassLoader>,
    pub refs: RwLock<HashSet<Pin<Arc<Ref>>>>,
    pub method_refs: RwLock<HashMap<Arc<Ref>, Arc<MethodHandle>>>,
}

pub struct JVM {
    pub stdout: Mutex<Box<dyn Write>>,
    pub threads: RwLock<Vec<Arc<Mutex<Thread>>>>,
    pub heap: Heap,
    pub class_loaders: RwLock<Vec<Pin<Box<ClassLoaderStore>>>>,
}

impl JVM {
    pub fn new(class_loader: Arc<dyn ClassLoader>, stdout: Mutex<Box<dyn Write>>) -> Arc<Self> {
        Arc::new(Self {
            stdout,
            threads: Default::default(),
            heap: Heap::new(),
            class_loaders: RwLock::new(vec![Pin::new(Box::new(ClassLoaderStore {
                loader: class_loader,
                refs: RwLock::new(HashSet::new()),
                method_refs: RwLock::new(HashMap::new()),
            }))]),
        })
    }

    ///This is not an instanceof check. This returns if the provided child class is specifically *below* the parent class in the inheritance tree.
    pub fn is_subclass(child: &Class, parent: &Class) -> bool {
        if &*child.this_class == "java/lang/Object" {
            return false;
        }

        if &*parent.this_class == "java/lang/Object" {
            return true;
        }

        let super_class = match &child.super_class {
            None => "",
            Some(super_class) => &super_class.this_class,
        };

        if &*parent.this_class == super_class {
            return true;
        } else {
            Self::is_subclass(&child.super_class.as_ref().unwrap(), parent)
        }
    }

    pub fn generate_class(
        &self,
        classpath: &str,
        class_loader: Arc<dyn ClassLoader>,
    ) -> Result<Arc<Class>, ClassLoadError> {
        let bytes = class_loader
            .get_bytes(classpath)
            .ok_or(ClassLoadError::ClassDefNotFound(classpath.into()))?;

        let classfile =
            ClassFile::from_bytes(Cursor::new(bytes)).map_err(|_| ClassLoadError::ParserError)?;

        let class = Arc::new(
            Class::init(&classfile, &self, class_loader.clone())
                .ok_or(ClassLoadError::MalformedClassDef)?,
        );

        class_loader.register_class(classpath, class.clone());

        self.register_refs(&*class_loader, &class);

        Ok(class)
    }

    pub fn find_class(
        &self,
        classpath: &str,
        class_loader: Arc<dyn ClassLoader>,
    ) -> Result<Arc<Class>, ClassLoadError> {
        let get = class_loader.get_class(classpath);
        match get {
            None => self.generate_class(classpath, class_loader.clone()),
            Some(get) => Ok(get),
        }
    }

    pub fn create_thread(self: &Arc<JVM>, class_loader: Arc<dyn ClassLoader>) -> ThreadHandle {
        let thread = Arc::new(Mutex::new(Thread {
            jvm: self.clone(),
            class_loader,
            frame_store: Pin::new(Box::new(FrameStack::new())),
            killed: false,
        }));

        let mut threads = self.threads.write();
        threads.push(thread.clone());

        std::mem::forget(thread.lock());

        ThreadHandle {
            thread: thread.data_ptr(),
            guard: thread,
        }
    }

    fn register_refs(&self, loader: &dyn ClassLoader, class: &Class) {
        let stores = self.class_loaders.read();

        let store = &stores[loader.id()];
        let mut method_refs = store.method_refs.write();
        let mut refs = store.refs.write();

        for method in &class.methods {
            let ref_ = Arc::new(Ref {
                class: class.this_class.clone(),
                name_and_type: Arc::new(NameAndType {
                    name: method.name.clone(),
                    descriptor: Arc::new(method.descriptor.string.clone()),
                }),
            });

            refs.insert(Pin::new(ref_.clone()));

            let (max_stack, max_locals) =
                if let Some(Attribute::Code(code)) = method.attributes.get("Code") {
                    (code.max_stack as usize + 1, code.max_locals as usize)
                } else {
                    (0, 0)
                };

            let ref_clone = ref_.clone();

            if !method.access_flags.contains(AccessFlags::NATIVE) {
                #[cfg(not(target_arch = "wasm32"))]
                let method_handle = MethodHandle {
                    ptr: ThreadHandle::interpret,
                    context: ExecutionContext::Interpret(Box::new(move |args| {
                        RawFrame::new(
                            &ref_clone.clone(),
                            args.iter()
                                .copied()
                                .chain(std::iter::repeat(Operand { data: 0 }).take(max_locals))
                                .collect::<Vec<Operand>>()
                                .into_boxed_slice(),
                            vec![Operand { data: 0 }; max_stack].into_boxed_slice(),
                        )
                    })),
                    method_ref: ref_.clone(),
                };

                #[cfg(target_arch = "wasm32")]
                let method_handle = panic!("Linking on wasm32 has not yet been implemented");

                method_refs.insert(ref_.clone(), Arc::new(method_handle));
            } else {
                match native::link(&ref_) {
                    None => {}
                    Some(ptr) => {
                        method_refs.insert(
                            ref_.clone(),
                            Arc::new(MethodHandle {
                                ptr,
                                context: ExecutionContext::Native,
                                method_ref: ref_.clone(),
                            }),
                        );
                    }
                }
            }
        }
    }
}

pub fn routine_resolve_invokespecial(class: &Class, method_ref: &Arc<Ref>) {
    match class.get_method(&method_ref.name_and_type) {
        None => {}
        Some(_other_method) => {}
    }
}

pub fn routine_resolve_field<'a>(name_and_type: &'a NameAndType, class: &'a Class) -> Option<(&'a Field, &'a Class)> {
    match class.get_field(name_and_type) {
        None => {
            //TODO: superinterfaces?

            if let Some(superclass) = &class.super_class {
                let super_resolve = routine_resolve_field(name_and_type, superclass);

                if let Some(resolved) = super_resolve {
                    return Some(resolved);
                }
            }

            None
        }
        Some(field) => Some((field, class)),
    }
}