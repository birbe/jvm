use crate::classfile::resolved::{Class, Ref};

use std::fmt::Debug;
use std::sync::Arc;
use crate::execution::MethodHandle;

pub trait ClassLoader: Send + Sync + Debug {
    fn get_bytes(&self, classpath: &str) -> Option<Vec<u8>>;

    fn register_class(&self, classpath: &str, class: Arc<Class>);

    fn get_class(&self, classpath: &str) -> Option<Arc<Class>>;

    fn link_ref(&self, ref_: Arc<Ref>, method_handle: Arc<MethodHandle>);

    fn get_method_handle(&self, ref_: &Ref) -> Arc<MethodHandle>;

    fn get_method_by_name(&self, classpath: &str, name: &str, descriptor: &str) -> Arc<MethodHandle>;

    fn id(&self) -> usize;
}
