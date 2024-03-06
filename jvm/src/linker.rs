use std::cell::RefCell;
use std::collections::HashMap;
use crate::classfile::resolved::{Class, Ref};

use std::fmt::{Debug, Formatter};
use std::hash::Hasher;
use std::sync::Arc;
use highway::{HighwayHasher, Key};
use parking_lot::{Mutex, RwLock};
use crate::env::Object;
use crate::execution::MethodHandle;
use crate::thread::{Operand, Thread};
use highway::HighwayHash;
use crate::{ClassContext, ClassLoadError};
use crate::classfile::ClassFile;

pub trait BootstrapLoader: Send + Debug + ClassLoader {

    fn get_classfile(&self, classpath: &str) -> Option<ClassFile>;

    fn has_class(&self, classpath: &str) -> bool;

}

pub struct ClassLoaderThreadedContext<'a> {
    pub loader: &'a dyn ClassLoader,
    pub context: RefCell<&'a mut Thread>
}

impl<'a> ClassContext for ClassLoaderThreadedContext<'a> {
    fn get_class(&self, classpath: &str) -> Result<Arc<Class>, ClassLoadError> {
        let mut context = self.context.borrow_mut();
        let jvm = context.jvm.clone();

        jvm.retrieve_class(classpath, self.loader, *context)
    }

    fn id(&self) -> u32 {
        self.loader.get_id()
    }
}

#[derive(Debug)]
pub struct ClassLoaderObject {
    pub instance: Object,
    pub loader_classpath: String,
    pub find_class: Arc<MethodHandle>,
    pub id: u32
}

unsafe impl Send for ClassLoaderObject {}

pub trait ClassLoader {

    fn find_class(&self, thread: &mut Thread, classpath: &str) -> Option<Object>;

    fn get_class_loader_object_handle(&self) -> &Object;

    fn get_id(&self) -> u32;

}

impl ClassLoaderObject {
    pub fn find_class(&self, thread: &mut Thread, classpath: &str) -> Option<Object> {
        let jvm = thread.jvm.clone();
        let classpath_string: Object = jvm.environment.new_string(classpath, thread);

        let jvm = thread.jvm.clone();

        unsafe {
            Some(jvm.environment.object_from_operand(&thread.invoke(&self.find_class, Box::new([Operand {
                objectref: classpath_string.ptr
            }])).unwrap()))
        }
    }

    pub fn get_class_loader_object_handle(&self) -> &Object {
        &self.instance
    }

}
