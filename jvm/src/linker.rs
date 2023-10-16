use crate::classfile::resolved::Class;

use std::fmt::Debug;
use std::sync::Arc;

pub trait ClassLoader: Send + Sync + Debug {
    fn get_bytes(&self, classpath: &str) -> Option<Vec<u8>>;

    fn register_class(&self, classpath: &str, class: Arc<Class>);

    fn get_class(&self, classpath: &str) -> Option<Arc<Class>>;

    fn id(&self) -> usize;
}
