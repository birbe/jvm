#![feature(slice_ptr_get)]
#![feature(slice_ptr_len)]
#![feature(ptr_metadata)]
#![feature(pointer_byte_offsets)]
#![feature(alloc_layout_extra)]
#![feature(const_alloc_layout)]
#![feature(if_let_guard)]

extern crate core;

use crate::classfile::resolved::{AccessFlags, Class, ClassId, Field, FieldType, Method, NameAndType, Ref};
use crate::classfile::ClassFile;
use crate::thread::{FrameStack, Operand, Thread, ThreadHandle};
use bitflags::Flags;

use jvm_types::JParse;
use linker::ClassLoader;
use parking_lot::{Mutex, RwLock};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};

use std::io::{Cursor, Write};

use crate::env::{Compiler, Environment, Object};
use crate::execution::{ExecutionContext, MethodHandle};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;
use highway::{HighwayHash, HighwayHasher, Key};

macro_rules! console_log {
    ($($t:tt)*) => (web_sys::console::log_1(&format_args!($($t)*).to_string().into()))
}

pub mod classfile;
mod tests;

pub mod bytecode;
pub mod env;
pub mod execution;
pub mod linker;
pub mod thread;

pub type Byte = i8;
pub type Short = i16;
pub type Char = u16;
pub type Int = i32;
pub type Long = i64;
pub type Float = f32;
pub type Double = f64;

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
}

pub struct JVM {
    pub stdout: Mutex<Box<dyn Write>>,
    pub threads: RwLock<Vec<Arc<Mutex<Thread>>>>,
    pub environment: Box<dyn Environment>,
    pub class_objects: RwLock<HashMap<ClassId, Object>>,
    class_ids: AtomicU32,
    thread_ids: AtomicU32,
}

impl JVM {
    pub fn new(stdout: Mutex<Box<dyn Write>>, environment: Box<dyn Environment>) -> Arc<Self> {
        Arc::new(Self {
            stdout,
            threads: RwLock::new(vec![]),
            environment,
            class_objects: RwLock::new(HashMap::new()),
            class_ids: AtomicU32::new(0),
            thread_ids: AtomicU32::new(0),
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

    pub fn make_class_struct(&self, thread: &mut Thread, class_loader: &dyn ClassLoader, bytes: &[u8]) -> Result<Arc<Class>, ClassLoadError> {
        let classfile =
            ClassFile::from_bytes(Cursor::new(bytes)).map_err(|_| ClassLoadError::ParserError)?;

        let class = Arc::new(
            Class::init(
                &classfile,
                thread,
                class_loader,
                self.class_ids.fetch_add(1, Ordering::Relaxed),
            )
                .ok_or(ClassLoadError::MalformedClassDef)?,
        );

        Ok(class)
    }

    pub fn define_class(&self, thread: &mut Thread, class_loader: &dyn ClassLoader, bytes: &[u8]) -> Result<Option<Object>, ClassLoadError> {
        let class = self.make_class_struct(thread, class_loader, bytes)?;

        class_loader.register_class(class.clone());

        self.link_class(class_loader, class.clone());

        console_log!("Defining {}", class.this_class);

        match class_loader.get_class("java/lang/Class") {
            None => {
                Ok(None)
            },
            Some(java_lang_class) => {

                let class_object = self.environment.new_object(&java_lang_class);

                // let init = class_loader.get_method_by_name("java/lang/Class", "<init>", "(Ljava/lang/ClassLoader;)V");
                // thread.invoke(&init, Box::new([
                //     Operand {
                //         objectref: class_object.ptr
                //     },
                //     Operand {
                //         objectref: class_loader.get_class_loader_object_handle().ptr
                //     },
                // ]));
                //
                // let mut objects = self.class_objects.write();
                //
                // unsafe {
                //     objects.insert(
                //         class.get_id(),
                //         self.environment.object_from_operand(&Operand {
                //             objectref: class_object.ptr
                //         })
                //     );
                // }

                Ok(Some(class_object))
            }
        }

    }

    pub fn find_class(
        &self,
        classpath: &str,
        class_loader: &dyn ClassLoader,
        thread: &mut Thread
    ) -> Result<Arc<Class>, ClassLoadError> {
        let get = class_loader.get_class(classpath);
        match get {
            None => {
                class_loader.find_class(thread, classpath);
                Ok(class_loader.get_class(classpath).unwrap())
            },
            Some(get) => Ok(get),
        }
    }

    pub fn create_thread(self: &Arc<JVM>, class_loader: Arc<dyn ClassLoader>) -> ThreadHandle {
        let thread = Arc::new(Mutex::new(Thread {
            jvm: self.clone(),
            class_loader,
            frame_stack: Box::new(FrameStack::new()),
            killed: false,
            id: self.thread_ids.fetch_add(1, Ordering::Relaxed),
        }));

        let mut threads = self.threads.write();
        threads.push(thread.clone());

        std::mem::forget(thread.lock());

        unsafe { ThreadHandle::new(thread.data_ptr(), thread) }
    }

    fn link_class(&self, loader: &dyn ClassLoader, class: Arc<Class>) {
        self.environment.link_class(class.clone());

        for method in &class.methods {
            let ref_ = Arc::new(Ref {
                class: class.this_class.clone(),
                name_and_type: Arc::new(NameAndType {
                    name: method.name.clone(),
                    descriptor: Arc::new(method.descriptor.string.clone()),
                }),
            });

            let method_handle = self.environment
                    .create_method_handle(loader, ref_.clone(), method.clone(), class.clone());

            loader.link_ref(ref_, Arc::new(method_handle));
        }
    }
}

pub fn routine_resolve_invokespecial(class: &Class, method_ref: &Ref) {
    match class.get_method(&method_ref.name_and_type) {
        None => {}
        Some(_other_method) => {}
    }
}

pub fn routine_resolve_field<'a>(
    name_and_type: &'a NameAndType,
    class: &'a Class,
) -> Option<(&'a Field, &'a Class)> {
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

#[derive(Debug, Clone)]
pub enum ResolveMethodError {
    IncompatibleClassChangeError,
}

pub fn routine_resolve_method<'class>(
    thread: &Thread,
    ref_: &Ref,
    class: &'class Class,
) -> Result<&'class Method, ResolveMethodError> {
    if class.access_flags.contains(AccessFlags::INTERFACE) {
        return Err(ResolveMethodError::IncompatibleClassChangeError);
    }

    fn step2<'class>(ref_: &Ref, class: &'class Class) -> Option<&'class Method> {
        let methods: Vec<&Arc<Method>> = class
            .methods
            .iter()
            .filter(|m| m.name == ref_.name_and_type.name)
            .collect();

        if methods.len() == 1 && methods[0].is_signature_polymorphic(&class.this_class) {
            let resolved_method = methods[0];

            for arg in &resolved_method.descriptor.args {
                match arg {
                    FieldType::Array { type_, dimensions } => {
                        match &**type_ {
                            FieldType::Class(classpath) => {
                                todo!()
                            },
                            _ => {}
                        }
                    }
                    FieldType::Class(_) => {},
                    _ => {}
                }
            }

            return Some(&*resolved_method);
        }

        if let Some(method) = class.get_method(&ref_.name_and_type) {
            return Some(method);
        }

        class
            .super_class
            .as_ref()
            .and_then(|super_class| step2(ref_, super_class))
    }

    if let Some(method) = step2(ref_, class) {
        return Ok(method);
    }

    todo!()
}
