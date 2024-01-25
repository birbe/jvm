#![feature(slice_ptr_get)]
#![feature(slice_ptr_len)]
#![feature(ptr_metadata)]
#![feature(pointer_byte_offsets)]
#![feature(alloc_layout_extra)]
#![feature(const_alloc_layout)]
#![feature(if_let_guard)]

extern crate core;

use crate::classfile::resolved::{AccessFlags, Attribute, Class, Field, Method, NameAndType, Ref};
use crate::classfile::{ClassFile};
use crate::thread::{FrameStack, Operand, RawFrame, Thread, ThreadHandle};
use bitflags::Flags;

use heap::{Heap};
use jvm_types::JParse;
use linker::ClassLoader;
use parking_lot::{Mutex, RwLock};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};

use std::io::{Cursor, stdout, Write};



use std::pin::Pin;

use std::sync::Arc;
use wasm_bindgen::prelude::wasm_bindgen;
use crate::bytecode::JValue;
use crate::env::Compiler;
use crate::execution::{ExecutionContext, invoke_table_function, MethodHandle};
use crate::heap::Object;

pub mod classfile;
mod tests;

pub mod bytecode;
pub mod heap;
pub mod linker;
pub mod execution;
pub mod native;
pub mod thread;
pub mod env;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    pub fn log(s: &str);

}

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
    pub class_loaders: RwLock<Vec<Pin<Box<ClassLoaderStore>>>>
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
            }))])
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

        unsafe { ThreadHandle::new(thread.data_ptr(), thread) }
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


pub fn routine_resolve_invokespecial(class: &Class, method_ref: &Ref) {
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

pub enum ResolveMethodError {
    IncompatibleClassChangeError
}

pub fn routine_resolve_method<'class>(jvm: &JVM, ref_: &Ref, class: &'class Class) -> Result<&'class Method, ResolveMethodError> {
    if class.access_flags.contains(AccessFlags::INTERFACE) {
        return Err(ResolveMethodError::IncompatibleClassChangeError);
    }

    fn step2<'class>(ref_: &Ref, class: &'class Class) -> Option<&'class Method> {
        let methods: Vec<&Method> = class.methods.iter().filter(|m| m.name == ref_.name_and_type.name).collect();

        if methods.len() == 1 && methods[0].is_signature_polymorphic(&class.this_class) {
            //TODO resolve references
            return Some(methods[0]);
        }

        if let Some(method) = class.get_method(&ref_.name_and_type) {
            return Some(method);
        }

        class.super_class.as_ref().and_then(|super_class| {
            step2(ref_, super_class)
        })
    }

    if let Some(method) = step2(ref_, class) {
        return Ok(method);
    }

    todo!()
}

struct NativeClassLoader {
    pub classes: RwLock<HashMap<String, Arc<Class>>>,
}

impl Debug for NativeClassLoader {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "NativeClassLoader")
    }
}

impl NativeClassLoader {
    unsafe fn dropper(ptr: *const ()) {
        Arc::from_raw(ptr as *const NativeClassLoader);
    }
}

impl ClassLoader for NativeClassLoader {
    fn get_bytes(&self, classpath: &str) -> Option<Vec<u8>> {
        match classpath {
            "Main" => Some(include_bytes!("../test_classes/Main.class")[..].into()),
            "java/lang/Object" => Some(include_bytes!("../test_classes/java/lang/Object.class")[..].into()),
            "java/lang/String" => Some(include_bytes!("../test_classes/java/lang/String.class")[..].into()),
            _ => panic!("{classpath} not found")
        }
    }

    fn register_class(&self, classpath: &str, class: Arc<Class>) {
        let mut classes = self.classes.write();
        classes.insert(classpath.into(), class);
    }

    fn get_class(&self, classpath: &str) -> Option<Arc<Class>> {
        let classes = self.classes.read();
        classes.get(classpath).cloned()
    }

    fn id(&self) -> usize {
        0
    }
}


#[wasm_bindgen]
pub fn wasm_test() {
    console_error_panic_hook::set_once();

    let mock = NativeClassLoader {
        classes: Default::default(),
    };

    let bootstrapper = Arc::new(mock) as Arc<dyn ClassLoader>;
    let jvm = JVM::new(
        bootstrapper.clone(),
        Mutex::new(Box::new(stdout())),
    );

    let _main = jvm.find_class("Main", bootstrapper.clone());

    let mut thread = jvm.create_thread(bootstrapper.clone());
    let result = thread.call("Main", "main", "([Ljava/lang/String;)[Ljava/lang/String;", &[])
        .unwrap();
}