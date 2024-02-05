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

pub trait ClassLoader: Send + Debug {

    fn find_class(&self, thread: &mut Thread, classpath: &str) -> Option<Object>;

    fn register_class(&self, class: Arc<Class>);

    fn get_class(&self, classpath: &str) -> Option<Arc<Class>>;

    fn link_ref(&self, ref_: Arc<Ref>, method_handle: Arc<MethodHandle>);

    fn get_method_handle(&self, ref_: &Ref) -> Arc<MethodHandle>;

    fn get_method_by_name(&self, classpath: &str, name: &str, descriptor: &str) -> Arc<MethodHandle>;

    fn id(&self) -> usize;

    fn get_class_loader_object_handle(&self) -> &Object;

}

#[derive(Debug)]
pub struct JavaClassLoader {
    pub instance: Object,
    pub loader_classpath: String,
    pub classes: Mutex<HashMap<String, Arc<Class>>>,
    pub handles: RwLock<HashMap<u64, Arc<MethodHandle>>>
}

unsafe impl Send for JavaClassLoader {}

impl ClassLoader for JavaClassLoader {
    fn find_class(&self, thread: &mut Thread, classpath: &str) -> Option<Object> {
        let method_handle = thread.class_loader.get_method_by_name(&self.loader_classpath, "findClass", "(Ljava/lang/String;)Ljava/lang/Class;");
        let classpath_string: Object = todo!();

        let jvm = thread.jvm.clone();

        unsafe {
            Some(jvm.environment.object_from_operand(&thread.invoke(&method_handle, Box::new([Operand {
                objectref: classpath_string.ptr
            }])).unwrap()))
        }
    }

    fn register_class(&self, class: Arc<Class>) {
        let mut classes = self.classes.lock();
        classes.insert((*class.this_class).clone(), class);
    }

    fn get_class(&self, classpath: &str) -> Option<Arc<Class>> {
        let classes = self.classes.lock();
        classes.get(classpath).cloned()
    }

    fn link_ref(&self, ref_: Arc<Ref>, method_handle: Arc<MethodHandle>) {
        let mut hasher = HighwayHasher::new(Key([0; 4]));
        hasher.append(ref_.class.as_bytes());
        hasher.append(ref_.name_and_type.name.as_bytes());
        hasher.append(ref_.name_and_type.descriptor.as_bytes());
        let key = hasher.finish();

        console_log!("java classloader linking {ref_:?}");

        self.handles.write().insert(key, method_handle);
    }

    fn get_method_handle(&self, ref_: &Ref) -> Arc<MethodHandle> {
        let handles = self.handles.read();

        let mut hasher = HighwayHasher::new(Key([0; 4]));
        hasher.append(ref_.class.as_bytes());
        hasher.append(ref_.name_and_type.name.as_bytes());
        hasher.append(ref_.name_and_type.descriptor.as_bytes());
        let key = hasher.finish();

        handles.get(&key).unwrap().clone()
    }

    fn get_method_by_name(&self, classpath: &str, name: &str, descriptor: &str) -> Arc<MethodHandle> {
        let handles = self.handles.read();

        let mut hasher = HighwayHasher::new(Key([0; 4]));
        hasher.append(classpath.as_bytes());
        hasher.append(name.as_bytes());
        hasher.append(descriptor.as_bytes());
        let key = hasher.finish();

        handles.get(&key).unwrap().clone()
    }

    fn id(&self) -> usize {
        1
    }

    fn get_class_loader_object_handle(&self) -> &Object {
        &self.instance
    }
}
