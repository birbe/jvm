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
use js_sys::Atomics::load;
use crate::linker::{BootstrapLoader, ClassLoaderThreadedContext};

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

pub struct ClassInstance {
    pub class: Arc<Class>,
    pub class_object: Object
}

pub trait ClassContext {

    fn get_class(&self, classpath: &str) -> Result<Arc<Class>, ClassLoadError>;

    fn id(&self) -> u32;

}

pub struct JVM {
    pub stdout: Mutex<Box<dyn Write>>,
    pub threads: RwLock<Vec<Arc<Mutex<Thread>>>>,
    pub environment: Box<dyn Environment>,
    pub loaded_classes: RwLock<HashMap<u64, ClassInstance>>,
    pub method_handles: RwLock<HashMap<u64, Arc<MethodHandle>>>,
    pub bootstrap_classloader: Arc<dyn BootstrapLoader>,
    pub system_classloader: ClassLoader,
    class_ids: AtomicU32,
    thread_ids: AtomicU32,
}

impl ClassContext for JVM {
    fn get_class(&self, classpath: &str) -> Result<Arc<Class>, ClassLoadError> {

        let mut loaded_classes = self.loaded_classes.write();

        let mut hasher = HighwayHasher::default();
        hasher.write_all(classpath.as_bytes()).unwrap();
        hasher.write_u8(0);

        let hash_key = hasher.finish();

        if loaded_classes.contains_key(&hash_key) {
            return Ok(loaded_classes.get(&hash_key).unwrap().class.clone());
        } else {
            let classfile = self.bootstrap_classloader.get_classfile(classpath).unwrap();
            let class = self.generate_class(&classfile, self)?;

            loaded_classes.insert(hash_key, ClassInstance {
                class: class.clone(),
                class_object: Object::NULL,
            });

            drop(loaded_classes);

            let class_class = self.get_class("java/lang/Class")?;
            let class_object = self.environment.new_object(&class_class);

            let mut loaded_classes = self.loaded_classes.write();
            let generated_class = loaded_classes.get_mut(&hash_key).unwrap();
            generated_class.class_object = class_object;

            Ok(class)
        }

    }

    fn id(&self) -> u32 {
        0
    }

}

impl JVM {
    pub fn new(stdout: Mutex<Box<dyn Write>>, environment: Box<dyn Environment>, bootstrap_classloader: Arc<dyn BootstrapLoader>) -> Arc<Self> {
        Arc::new(Self {
            stdout,
            threads: RwLock::new(vec![]),
            environment,
            loaded_classes: RwLock::new(HashMap::new()),
            method_handles: RwLock::new(HashMap::new()),
            bootstrap_classloader,
            system_classloader: ClassLoader {
                instance: Object::NULL,
                loader_classpath: "sun/misc/Loader".into(),
                id: 1,
            },
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

    pub fn generate_class(&self, class_file: &ClassFile, context: &dyn ClassContext) -> Result<Arc<Class>, ClassLoadError> {
        let class = Arc::new(
            Class::init(
                &class_file,
                context,
                self.class_ids.fetch_add(1, Ordering::Relaxed),
            )
                .ok_or(ClassLoadError::MalformedClassDef)?,
        );

        Ok(class)
    }

    pub fn define_class(&self, bytes: &[u8], thread: &mut Thread) -> Result<(), ClassLoadError> {
        let class_file = ClassFile::from_bytes(Cursor::new(bytes)).map_err(|err| {
            ClassLoadError::MalformedClassDef
        })?;

        let loader = thread.class_loader.clone();

        let context = ClassLoaderThreadedContext {
            loader: &loader,
            context: thread,
        };

        let class = self.generate_class(&class_file, &context)?;
        let java_lang_class = self.get_class("java/lang/Class")?;

        let mut hasher = HighwayHasher::default();
        hasher.write_all(class.this_class.as_bytes()).unwrap();
        hasher.write_u32(thread.class_loader.id);
        let hash_key = hasher.finish();

        let class_object = self.environment.new_object(&java_lang_class);
        let handle = self.get_method("java/lang/Class", "<init>", "(Ljava/lang/ClassLoader;)V", 0);

        thread.invoke(&handle, Box::new([
            Operand {
                objectref: thread.class_loader.instance.ptr
            }
        ]));

        let instance = ClassInstance {
            class,
            class_object,
        };

        let mut loaded_classes = self.loaded_classes.write();
        loaded_classes.insert(hash_key, instance);

        Ok(())
    }

    pub fn get_method(&self, classpath: &str, method_name: &str, method_descriptor: &str, class_loader: u32) -> Arc<MethodHandle> {
        let mut hasher = HighwayHasher::default();
        hasher.write_all(classpath.as_bytes()).unwrap();
        hasher.write_all(method_name.as_bytes()).unwrap();
        hasher.write_all(method_descriptor.as_bytes()).unwrap();
        hasher.write_u32(class_loader);
        let key = hasher.finish();

        let handles = self.method_handles.read();
        handles.get(&key).unwrap().clone()
    }

    pub fn retrieve_class(
        &self,
        classpath: &str,
        class_loader: &ClassLoader,
        thread: &mut Thread
    ) -> Result<Arc<Class>, ClassLoadError> {
        let loaded_classes = self.loaded_classes.read();

        let mut hasher = HighwayHasher::default();
        hasher.write_all(classpath.as_bytes()).unwrap();
        hasher.write_u32(thread.class_loader.id);
        let hash_key = hasher.finish();

        if self.bootstrap_classloader.has_class(classpath) {
            return self.get_class(classpath);
        } else if !loaded_classes.contains_key(&hash_key) {
            drop(loaded_classes);
            match self.system_classloader.find_class(thread, classpath) {
                None => {
                    thread.class_loader.find_class(thread, classpath);

                    let loaded_classes = self.loaded_classes.read();

                    let mut hasher = HighwayHasher::default();
                    hasher.write_all(classpath.as_bytes()).unwrap();
                    hasher.write_u32(self.system_classloader.id);
                    let hash_key = hasher.finish();

                    return Ok(loaded_classes.get(&hash_key).unwrap().class.clone())
                }
                Some(_) => {
                    return self.retrieve_class(classpath, thread);
                }
            }
        }

        Ok(loaded_classes.get(&hash_key).unwrap().class.clone())
    }

    pub fn create_thread(self: &Arc<JVM>, class_loader: Arc<ClassLoader>) -> ThreadHandle {
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

    fn link_class(&self, class: Arc<Class>, loader: Option<&ClassLoader>) {
        self.environment.link_class(class.clone());

        let mut methods = self.method_handles.write();

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

            let mut key = HighwayHasher::default();
            key.write_all(class.this_class.as_bytes()).unwrap();
            key.write_all(method.name.as_bytes()).unwrap();
            key.write_all(method.descriptor.string.as_bytes()).unwrap();
            key.write_u32(loader.map(|loader| loader.id).unwrap_or(0));

            methods.insert(key.finish(), Arc::new(method_handle));
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
