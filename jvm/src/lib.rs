#![feature(slice_ptr_get)]
#![feature(ptr_metadata)]
#![feature(alloc_layout_extra)]
#![feature(if_let_guard)]
#![feature(asm_experimental_arch)]
#![feature(let_chains)]
extern crate core;

use crate::classfile::resolved::{AccessFlags, Class, ClassDeserializationError, ConstantPool, Method, MethodDescriptor, NameAndType, Ref};
use crate::classfile::ClassFile;
use crate::thread::{FrameStack, Operand, Thread, ThreadHandle};

use jvm_types::JParse;
use parking_lot::{Mutex, RwLock};
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};

use std::io::Write;
use std::iter;
use crate::env::{Compiler, Environment, Object};
use crate::execution::MethodHandle;
use crate::linker::{BootstrapLoader, ClassLoader};
use highway::{HighwayHash, HighwayHasher};
use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};
use std::sync::Arc;
use once_cell::sync::OnceCell;
use crate::bytecode::ArrayType;

#[cfg(target_arch = "wasm32")]
macro_rules! console_log {
    ($($t:tt)*) => (web_sys::console::log_1(&format_args!($($t)*).to_string().into()))
}

#[cfg(not(target_arch = "wasm32"))]
macro_rules! console_log {
    ($($t:tt)*) => (println!($t))
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

#[derive(Debug)]
pub enum ClassLoadError {
    ClassDefNotFound(String),
    MalformedClassDef(String, ClassDeserializationError),
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

    fn get_class(&self, thread: &mut Thread, classpath: &str) -> Result<Arc<Class>, ClassLoadError>;

    fn id(&self) -> u32;

}

pub struct JVM {
    pub stdout: Mutex<Box<dyn Write>>,
    pub threads: RwLock<Vec<Arc<Mutex<Thread>>>>,
    pub environment: Box<dyn Environment>,
    pub loaded_classes: RwLock<HashMap<u64, Arc<Class>>>,
    pub array_classes: RwLock<HashMap<u64, Object>>,
    pub method_handles: RwLock<HashMap<u64, Arc<MethodHandle>>>,
    pub bootstrap_classloader: Arc<dyn BootstrapLoader>,
    class_ids: AtomicU32,
    thread_ids: AtomicU64,
}

impl JVM {
    pub fn new(stdout: Mutex<Box<dyn Write>>, environment: Box<dyn Environment>, bootstrap_classloader: Arc<dyn BootstrapLoader>) -> Arc<Self> {
        let jvm = Arc::new(Self {
            stdout,
            threads: RwLock::new(vec![]),
            environment,
            loaded_classes: RwLock::new(HashMap::new()),
            array_classes: RwLock::new(HashMap::new()),
            method_handles: RwLock::new(HashMap::new()),
            bootstrap_classloader,
            class_ids: AtomicU32::new(0),
            thread_ids: AtomicU64::new(0),
        });


        let mut temp_thread = jvm.create_thread(jvm.bootstrap_classloader.clone());
        let java_lang_class = jvm.bootstrap_classloader.find_class(&mut temp_thread, "java/lang/Class").unwrap();

        let classes = jvm.loaded_classes.read().clone();

        jvm.generate_class_object(java_lang_class, &*jvm.bootstrap_classloader, &mut temp_thread).unwrap();

        for (_, class) in classes.iter().filter(|(_, class)| &*class.this_class != "java/lang/Class") {
            jvm.generate_class_object(class.clone(), &*jvm.bootstrap_classloader, &mut temp_thread).unwrap();
        }

        let primitives: HashMap<ArrayType, Arc<Class>> = [
            (ArrayType::Byte, "B"),
            (ArrayType::Char, "C"),
            (ArrayType::Double, "D"),
            (ArrayType::Float, "F"),
            (ArrayType::Int, "I"),
            (ArrayType::Long, "J"),
            (ArrayType::Short, "S"),
            (ArrayType::Bool, "Z")
        ].into_iter().map(|(array_type, primitive)| {
            let primitive = jvm.generate_primitive_class(&mut temp_thread, primitive);
            jvm.link_class(primitive.clone(), &*jvm.bootstrap_classloader);
            jvm.generate_class_object(primitive.clone(), &*jvm.bootstrap_classloader, &mut temp_thread).unwrap();
            let primitive_array = jvm.find_array_class(&mut temp_thread, primitive, 1);

            (array_type, primitive_array)
        }).collect();

        jvm.environment.store_primitive_classes(primitives);

        jvm.init_thread_object(&mut temp_thread);

        jvm.bootstrap_classloader.find_class(&mut temp_thread, "java/lang/System").unwrap();

        let ref_class = jvm.bootstrap_classloader.find_class(&mut temp_thread, "java/lang/ref/Reference").unwrap();
        ThreadHandle::init_static(&ref_class, &mut temp_thread);

        // temp_thread.call("java/lang/System", "initPhase1", "()V", Box::new([])).unwrap();
        
        jvm
    }

    fn init_thread_object(&self, thread: &mut Thread) {
        let java_lang_thread = self.bootstrap_classloader.find_class(thread, "java/lang/Thread").unwrap();
        let thread_obj = self.environment.new_object(&java_lang_thread);

        thread.thread_obj.set(self.environment.clone_reference(&thread_obj)).unwrap();

        let java_lang_thread_group = self.bootstrap_classloader.find_class(thread, "java/lang/ThreadGroup").unwrap();
        let thread_group = self.environment.new_object(&java_lang_thread_group);

        let init_thread_group = self.get_method("java/lang/ThreadGroup", "<init>", "()V", 0);

        drop(thread.invoke(&init_thread_group, Box::new([self.environment.clone_reference(&thread_group).consume()])));

        let init_thread = self.get_method(
            "java/lang/Thread",
            "<init>",
            "(Ljava/lang/ThreadGroup;Ljava/lang/String;ILjava/lang/Runnable;JLjava/security/AccessControlContext;)V",
            0
        );
        
        let thread_name = self.new_string("", thread);
        
        drop(thread.invoke(&init_thread, Box::new([
            thread_obj.consume(),
            thread_group.consume(),
            thread_name.consume(),
            0b100i32.into(),
            Object::NULL.consume(),
            256.into(),
            Object::NULL.consume(),
        ])));

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
            true
        } else {
            Self::is_subclass(&child.super_class.as_ref().unwrap(), parent)
        }
    }

    pub fn generate_class(&self, classpath: &str, class_file: &ClassFile, class_loader: Arc<dyn ClassLoader>, thread: &mut Thread) -> Result<Arc<Class>, ClassLoadError> {
        let class = Arc::new(
            Class::init(
                &class_file,
                thread,
                class_loader.clone(),
                self.class_ids.fetch_add(1, Ordering::Relaxed),
            )
                .map_err(|e| ClassLoadError::MalformedClassDef(classpath.to_string(), e))?,
        );

        let mut loaded_classes = self.loaded_classes.write();

        let mut hasher = HighwayHasher::default();
        hasher.write_all(class.this_class.as_bytes()).unwrap();
        hasher.write_u32(class_loader.get_id());
        let hash_key = hasher.finish();

        if let Some(_) = loaded_classes.insert(hash_key, class.clone()) {
            panic!("Duplicate class loaded for {}", class.this_class);
        }

        drop(loaded_classes);

        self.link_class(class.clone(), &*class_loader);

        Ok(class)
    }
    
    pub fn is_class_linked(&self, classpath: &str, loader: u32) -> Option<Arc<Class>> {
        let mut hasher = HighwayHasher::default();
        hasher.write_all(classpath.as_bytes()).unwrap();
        hasher.write_u32(loader);
        let hash_key = hasher.finish();
        
        self.loaded_classes.read().get(&hash_key).cloned()
    }

    ///This method creates an instance of java.lang.Class<?> for a given Class
    pub fn generate_class_object(&self, class: Arc<Class>, class_loader: &dyn ClassLoader, thread: &mut Thread) -> Result<(), ClassLoadError> {
        if self.is_class_linked("java/lang/Class", 0).is_none() { return Ok(()); }
        
        let java_lang_class = self.bootstrap_classloader.get_class(thread, "java/lang/Class")?;

        // ThreadHandle::init_static(&java_lang_class, thread);
        let class_object = self.environment.new_object(&java_lang_class);

        let handle = self.get_method("java/lang/Class", "<init>", "(Ljava/lang/ClassLoader;Ljava/lang/Class;)V", 0);
        
        let class_loader_object = class_loader.get_class_loader_object_handle();

        let op = self.environment.clone_reference(&class_object).consume();

        thread.invoke(&handle, Box::new([
            op,
            self.environment.clone_reference(&class_loader_object).consume(),
            Object::NULL.consume()
        ]));

        unsafe {
            self.environment.set_object_field(&class_object, &java_lang_class, "__jvm_internal_id", "I", Operand::new_data(&*class as *const Class as u32));
        }

        if let Err(err) = class.class_instance.set(class_object) {
            panic!(":(");
        }

        Ok(())
    }

    pub fn find_array_class(&self, thread: &mut Thread, component: Arc<Class>, depth: u8) -> Arc<Class> {
        if depth == 0 {
            return component;
        }

        let classpath = format!("{}{}", iter::repeat_n("[", depth as usize).collect::<String>(), component.internal_identifier);

        let internal = {
            match self.is_class_linked(&classpath, component.class_loader.get_id()) {
                Some(class) => return class,
                None => self.generate_internal_array_class(thread, &classpath, component.clone(), depth)
            }
        };

        internal
    }

    fn generate_primitive_class(&self, thread: &mut Thread, classpath: &str) -> Arc<Class> {
        let this_class = Arc::new(classpath.to_string());

        let class = Arc::new(Class {
            constant_pool: ConstantPool {
                constants: Default::default(),
            },
            class_loader: self.bootstrap_classloader.clone(),
            access_flags: AccessFlags::PUBLIC,
            this_class: this_class.clone(),
            super_class: Some(self.bootstrap_classloader.find_class(thread, "java/lang/Object").unwrap()),
            interfaces: vec![],
            fields: vec![],
            methods: vec![],
            attributes: vec![],
            class_instance: OnceCell::new(),
            statics: Box::into_raw(Box::new([])),
            internal_id: self.class_ids.fetch_add(1, Ordering::Relaxed),
            static_init_state: AtomicU32::new(2),
            is_array: false,
            is_primitive: true,
            internal_identifier: classpath.to_string(),
        });

        {
            let mut hasher = HighwayHasher::default();
            hasher.write_all(classpath.as_bytes()).unwrap();
            //bootstrap loader
            hasher.write_u32(0);
            let hash_key = hasher.finish();

            self.loaded_classes.write().insert(hash_key, class.clone());
        }

        class
    }

    fn generate_internal_array_class(&self, thread: &mut Thread, classpath: &str, component: Arc<Class>, depth: u8) -> Arc<Class> {
        let inner_component = if depth == 0 {
            component.clone()
        } else {
            self.find_array_class(thread, component.clone(), depth - 1)
        };

        let this_class = Arc::new(classpath.to_string());

        let class = Arc::new(Class {
            constant_pool: ConstantPool {
                constants: Default::default(),
            },
            class_loader: component.class_loader.clone(),
            access_flags: AccessFlags::PUBLIC,
            this_class: this_class.clone(),
            internal_identifier: (*this_class).clone(),
            super_class: Some(self.bootstrap_classloader.find_class(thread, "java/lang/Object").unwrap()),
            interfaces: vec![],
            fields: vec![],
            methods: vec![
                Arc::new(
                    Method {
                        access_flags: AccessFlags::PUBLIC | AccessFlags::NATIVE,
                        class: this_class,
                        descriptor: MethodDescriptor::try_from(&*format!("(){}", classpath)).unwrap(),
                        name: Arc::new("clone".into()),
                        attributes: HashMap::new(),
                        identifier: Arc::new(format!("clone(){}", classpath))
                    }
                )
            ],
            attributes: vec![],
            class_instance: OnceCell::new(),
            statics: Box::into_raw(Box::new([])),
            internal_id: self.class_ids.fetch_add(1, Ordering::Relaxed),
            static_init_state: AtomicU32::new(2),
            is_array: true,
            is_primitive: false,
        });

        {
            let mut hasher = HighwayHasher::default();
            hasher.write_all(classpath.as_bytes()).unwrap();
            hasher.write_u32(component.class_loader.get_id());
            let hash_key = hasher.finish();

            self.loaded_classes.write().insert(hash_key, class.clone());
        }

        let java_lang_class = self.bootstrap_classloader.find_class(thread, "java/lang/Class").unwrap();
        let class_object= self.environment.new_object(&java_lang_class);

        let handle = self.get_method("java/lang/Class", "<init>", "(Ljava/lang/ClassLoader;Ljava/lang/Class;)V", 0);

        thread.invoke(&handle, Box::new([
            self.environment.clone_reference(&class_object).consume(),
            self.environment.clone_reference(component.class_loader.get_class_loader_object_handle()).consume(),
            self.environment.clone_reference(inner_component.class_instance.get().unwrap()).consume(),
        ]));

        unsafe {
            self.environment.set_object_field(&class_object, &java_lang_class, "__jvm_internal_id", "I", Operand::new_data(&*class as *const Class as u32));
        }

        class.class_instance.set(class_object).unwrap();

        class
    }

    pub fn get_method(&self, classpath: &str, method_name: &str, method_descriptor: &str, class_loader: u32) -> Arc<MethodHandle> {
        let mut hasher = HighwayHasher::default();
        hasher.write_all(classpath.as_bytes()).unwrap();
        hasher.write_all(method_name.as_bytes()).unwrap();
        hasher.write_all(method_descriptor.as_bytes()).unwrap();
        hasher.write_u32(class_loader);
        let key = hasher.finish();

        let handles = self.method_handles.read();
        match handles.get(&key) {
            None => panic!("classpath {classpath} method name {method_name} descriptor {method_descriptor} loader {class_loader}"),
            Some(handle) => handle.clone()
        }
    }

    ///Return an already loaded Class, and if its not loaded, attempt to load it using the ClassLoader
    pub fn retrieve_class(
        &self,
        classpath: &str,
        class_loader: &dyn ClassLoader,
        thread: &mut Thread
    ) -> Result<Arc<Class>, ClassLoadError> {
        let loaded_classes = self.loaded_classes.read();

        let mut hasher = HighwayHasher::default();
        hasher.write_all(classpath.as_bytes()).unwrap();
        hasher.write_u32(thread.class_loader.get_id());
        let hash_key = hasher.finish();

        match loaded_classes.get(&hash_key) {
            None => {}
            Some(class) => return Ok(class.clone())
        }

        drop(loaded_classes);

        if self.bootstrap_classloader.has_class(classpath) {
            return self.bootstrap_classloader.find_class(thread, classpath);
        }

        class_loader.find_class(thread, classpath)
    }

    pub fn create_thread(self: &Arc<JVM>, class_loader: Arc<dyn ClassLoader>) -> ThreadHandle {
        let thread = Thread {
            jvm: self.clone(),
            class_loader,
            frame_stack: Box::new(FrameStack::new()),
            killed: false,
            id: self.thread_ids.fetch_add(1, Ordering::Relaxed) as u32,
            instructions_executed: AtomicU64::new(0),
            thread_obj: OnceCell::new(),
        };

        if let Some(thread_class) = self.is_class_linked("java/lang/Thread", 0) {
            let thread_obj = self.environment.new_object(&thread_class);
            // let characteristics = 0b100; //native thread, no parent
            thread.thread_obj.set(thread_obj).unwrap();
        }

        let mutex = Arc::new(Mutex::new(thread));

        let mut threads = self.threads.write();
        threads.push(mutex.clone());

        std::mem::forget(mutex.lock());

        unsafe { ThreadHandle::new(mutex.data_ptr(), mutex) }
    }

    fn link_class(&self, class: Arc<Class>, loader: &dyn ClassLoader) {
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
                    .create_method_handle(ref_.clone(), method.clone(), class.clone(), loader);

            let mut key = HighwayHasher::default();
            key.write_all(class.this_class.as_bytes()).unwrap();
            key.write_all(method.name.as_bytes()).unwrap();
            key.write_all(method.descriptor.string.as_bytes()).unwrap();
            key.write_u32(loader.get_id());

            methods.insert(key.finish(), Arc::new(method_handle));
        }
    }

    fn new_string(&self, contents: &str, thread: &mut Thread) -> Object {
        let string_class = self.bootstrap_classloader.find_class(thread,"java/lang/String").unwrap();

        ThreadHandle::init_static(&string_class, thread);

        let string_object = self.environment.new_object(&string_class);

        let char_array = self.environment.new_array(ArrayType::Byte, contents.as_bytes().len() as i32);

        for (index, byte) in contents.as_bytes().iter().enumerate() {
            unsafe { self.environment.set_array_element(ArrayType::Byte, &char_array, index as i32, Operand::new_data(*byte as u32)) };
        }

        unsafe { self.environment.set_object_field(&string_object, &string_class, "value", "[B", char_array.consume()); }

        string_object
    }

    pub fn get_string(&self, string_object: &Object) -> String {
        let object_class = self.environment.get_object_class(string_object);
        assert_eq!(&*object_class.this_class, "java/lang/String");

        let value = unsafe { self.environment.get_object_field(string_object, object_class, "value", "[B") };
        let byte_array = unsafe { self.environment.object_from_operand(value) };

        let bytes_len = self.environment.get_array_length(&byte_array);
        let bytes: Vec<u8> = (0..bytes_len).map(|index| {
            unsafe { self.environment.get_array_element(ArrayType::Byte, &byte_array, index).data() as u8 }
        }).collect();

        String::from_utf8(bytes).unwrap()
    }


}

#[derive(Debug, Clone)]
pub enum ResolveMethodError {
    IncompatibleClassChangeError,
}

