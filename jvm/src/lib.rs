#![feature(slice_ptr_get)]
#![feature(slice_ptr_len)]
#![feature(ptr_metadata)]
#![feature(pointer_byte_offsets)]
#![feature(alloc_layout_extra)]
#![feature(const_alloc_layout)]
#![feature(if_let_guard)]

extern crate core;

use crate::classfile::resolved::{AccessFlags, Attribute, Class, Field, Method, NameAndType, Ref};
use crate::classfile::ClassFile;
use crate::thread::{FrameStack, Operand, RawFrame, Thread, ThreadHandle};
use bitflags::Flags;

use heap::Heap;
use jvm_types::JParse;
use linker::ClassLoader;
use parking_lot::{Mutex, RwLock};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};

use std::io::{stdout, Cursor, Write};

use std::pin::Pin;

use crate::env::wasm::{RefSlots, WasmEnvironment, REFS};
use crate::env::{Compiler, Environment};
use crate::execution::{ExecutionContext, InterpreterContext, MethodHandle};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen_futures::spawn_local;

pub mod classfile;
mod tests;

pub mod bytecode;
pub mod env;
pub mod execution;
pub mod heap;
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
}

pub struct JVM {
    pub stdout: Mutex<Box<dyn Write>>,
    pub threads: RwLock<Vec<Arc<Mutex<Thread>>>>,
    pub heap: Heap,
    pub environment: Box<dyn Environment>,
    class_ids: AtomicU32,
}

impl JVM {
    pub fn new(stdout: Mutex<Box<dyn Write>>, environment: Box<dyn Environment>) -> Arc<Self> {
        Arc::new(Self {
            stdout,
            threads: RwLock::new(vec![]),
            heap: Heap::new(),
            environment,
            class_ids: AtomicU32::new(0),
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
            Class::init(
                &classfile,
                &self,
                class_loader.clone(),
                self.class_ids.fetch_add(1, Ordering::Relaxed),
            )
            .ok_or(ClassLoadError::MalformedClassDef)?,
        );

        class_loader.register_class(classpath, class.clone());

        self.link_class(&*class_loader, class.clone());

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

            let method_handle = if !method.access_flags.contains(AccessFlags::NATIVE) {
                unsafe {
                    MethodHandle::new(
                        ThreadHandle::interpret,
                        ExecutionContext::Interpret(InterpreterContext::new(
                            ref_.clone(),
                            class.clone(),
                        )),
                        method.clone(),
                    )
                }
            } else {
                match native::link(&ref_) {
                    None => return,
                    Some(ptr) => unsafe {
                        MethodHandle::new(ptr, ExecutionContext::Native, method.clone())
                    },
                }
            };

            self.environment
                .register_method_handle(loader, ref_, method_handle);
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

pub enum ResolveMethodError {
    IncompatibleClassChangeError,
}

pub fn routine_resolve_method<'class>(
    jvm: &JVM,
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
            //TODO resolve references
            return Some(methods[0]);
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
            "java/lang/Object" => {
                Some(include_bytes!("../test_classes/java/lang/Object.class")[..].into())
            }
            "java/lang/String" => {
                Some(include_bytes!("../test_classes/java/lang/String.class")[..].into())
            }
            "javax/swing/BorderFactory" => {
                Some(include_bytes!("../test_classes/javax/swing/BorderFactory.class")[..].into())
            }
            "javax/swing/JColorChooser" => {
                Some(include_bytes!("../test_classes/javax/swing/JColorChooser.class")[..].into())
            }
            "javax/swing/JComponent" => {
                Some(include_bytes!("../test_classes/javax/swing/JComponent.class")[..].into())
            }
            "java/awt/Container" => {
                Some(include_bytes!("../test_classes/java/awt/Container.class")[..].into())
            }
            "java/awt/Component" => {
                Some(include_bytes!("../test_classes/java/awt/Component.class")[..].into())
            }
            "java/awt/image/ImageObserver" => Some(
                include_bytes!("../test_classes/java/awt/image/ImageObserver.class")[..].into(),
            ),
            "java/awt/MenuContainer" => {
                Some(include_bytes!("../test_classes/java/awt/MenuContainer.class")[..].into())
            }
            "java/io/Serializable" => {
                Some(include_bytes!("../test_classes/java/awt/MenuContainer.class")[..].into())
            }

            _ => panic!("{classpath} not found"),
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

    let descriptor = js_sys::Object::new();

    js_sys::Reflect::set(&descriptor, &"element".into(), &"anyref".into()).unwrap();
    js_sys::Reflect::set(&descriptor, &"initial".into(), &"0".into()).unwrap();

    let table = js_sys::WebAssembly::Table::new(&descriptor).unwrap();

    let jvm = JVM::new(
        Mutex::new(Box::new(stdout())),
        Box::new(WasmEnvironment::new(table)),
    );

    let _main = jvm.find_class("javax/swing/JColorChooser", bootstrapper.clone());
    //
    // let mut thread = jvm.create_thread(bootstrapper.clone());
    // let result = thread.call("Main", "main", "([Ljava/lang/String;)[Ljava/lang/String;", &[])
    //     .unwrap();
}
