use std::cell::RefCell;
use std::collections::HashMap;
use crate::classfile::resolved::{Class, Ref};

use std::fmt::{Debug, Formatter};
use std::hash::Hasher;
use std::io::Write;
use std::sync::Arc;
use highway::{HighwayHasher, Key};
use parking_lot::{Mutex, RwLock};
use crate::env::Object;
use crate::execution::MethodHandle;
use crate::thread::{Operand, Thread};
use highway::HighwayHash;
use crate::{ClassContext, ClassInstance, ClassLoadError, JVM};
use crate::classfile::ClassFile;

pub trait BootstrapLoader: ClassLoader {

    fn get_classfile(&self, classpath: &str) -> Option<ClassFile>;

    fn has_class(&self, classpath: &str) -> bool;

    ///This method checks the JVM method area to see if the class has been defined already;
    /// if not, then it will retrieve the bytes defining the classfile through some implementation-dependent manner
    /// and call JVM::generate_class. This method will not generate a corresponding Class<?> instance
    fn get_class(&self, thread: &mut Thread, classpath: &str) -> Result<Arc<Class>, ClassLoadError>;

}

pub struct ClassLoaderThreadedContext<'a> {
    pub loader: &'a dyn ClassLoader,
    pub context: RefCell<&'a mut Thread>
}

// impl<'a> ClassContext for ClassLoaderThreadedContext<'a> {
//     fn get_class(&self, _: &JVM, classpath: &str) -> Result<Arc<Class>, ClassLoadError> {
//         let mut context = self.context.borrow_mut();
//         let jvm = context.jvm.clone();
//
//         jvm.retrieve_class(classpath, self.loader, *context)
//     }
//
//     fn id(&self) -> u32 {
//         self.loader.get_id()
//     }
// }

#[derive(Debug)]
pub struct ClassLoaderObject {
    pub instance: Object,
    pub loader_classpath: String,
    pub find_class: Arc<MethodHandle>,
    pub id: u32
}

unsafe impl Send for ClassLoaderObject {}

pub trait ClassLoader: Debug {

    fn find_class(&self, thread: &mut Thread, classpath: &str) -> Option<Arc<Class>>;

    fn get_class_loader_object_handle(&self) -> &Object;

    fn get_id(&self) -> u32;

}

impl ClassLoader for Arc<ClassLoaderObject> {
    fn find_class(&self, thread: &mut Thread, classpath: &str) -> Option<Arc<Class>> {
        let jvm = thread.jvm.clone();
        let classpath_string: Object = jvm.environment.new_string(classpath, thread);

        {
            let loaded_classes = jvm.loaded_classes.read();

            let mut hasher = HighwayHasher::default();
            hasher.write_all(classpath.as_bytes()).unwrap();
            hasher.write_u32(self.get_id());
            let hash_key = hasher.finish();
            
            match loaded_classes.get(&hash_key) {
                None => {}
                Some(class) => return Some(class.clone())
            }
        }

        //defineClass should be called by the ClassLoader when invoked
        let operands = Box::new([
            Operand { objectref: self.instance.ptr },
            Operand { objectref: classpath_string.ptr }
        ]);
        
        let object = unsafe {
            jvm.environment.object_from_operand(&thread.invoke(&self.find_class, operands).unwrap())
        };
        let class = jvm.retrieve_class(classpath, self, thread).ok()?;
        
        Some(class)
    }

    fn get_class_loader_object_handle(&self) -> &Object {
        &self.instance
    }

    fn get_id(&self) -> u32 {
        self.id
    }
}
