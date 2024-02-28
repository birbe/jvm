use std::collections::HashMap;
use crate::classfile::resolved::{Class, Ref};

use std::fmt::{Debug, Formatter};
use std::hash::Hasher;
use std::sync::Arc;
use highway::{HighwayHasher, Key};
use js_sys::Atomics::load;
use parking_lot::{Mutex, RwLock};
use crate::env::Object;
use crate::execution::MethodHandle;
use crate::thread::{Operand, Thread};
use highway::HighwayHash;
use crate::{ClassContext, ClassLoadError};
use crate::classfile::ClassFile;

pub trait BootstrapLoader: Send + Debug {

    fn get_classfile(&self, classpath: &str) -> Option<ClassFile>;

    fn has_class(&self, classpath: &str) -> bool;

}

pub struct ClassLoaderThreadedContext<'a> {
    pub loader: &'a ClassLoader,
    pub context: &'a mut Thread
}

impl ClassContext for ClassLoaderThreadedContext {
    fn get_class(&self, classpath: &str) -> Result<Arc<Class>, ClassLoadError> {
        self.context.jvm.retrieve_class(classpath, self.context)
    }

    fn id(&self) -> u32 {
        self.loader.id
    }
}

#[derive(Debug)]
pub struct ClassLoader {
    pub instance: Object,
    pub loader_classpath: String,
    pub id: u32
}

unsafe impl Send for ClassLoader {}

impl ClassLoader {
    pub fn find_class(&self, thread: &mut Thread, classpath: &str) -> Option<Object> {
        let method_handle = thread.class_loader.get_method_by_name(&self.loader_classpath, "findClass", "(Ljava/lang/String;)Ljava/lang/Class;");
        let classpath_string: Object = thread.jvm.environment.new_string(classpath, thread);

        let jvm = thread.jvm.clone();

        unsafe {
            Some(jvm.environment.object_from_operand(&thread.invoke(&method_handle, Box::new([Operand {
                objectref: classpath_string.ptr
            }])).unwrap()))
        }
    }

    pub fn get_class_loader_object_handle(&self) -> &Object {
        &self.instance
    }

}
