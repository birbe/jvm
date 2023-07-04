use std::fmt::Debug;
use std::sync::Arc;
use crate::classfile::resolved::Class;
use crate::{ClassLoadError, JVM};

pub trait ClassLoader: Send + Sync + Debug {

    fn get_class(&self, classpath: &str) -> Option<Arc<Class>>;

    fn find_class(&self, classpath: &str, jvm: &JVM) -> Result<Arc<Class>, ClassLoadError>;

    fn generate_class(&self, classpath: &str, jvm: &JVM) -> Result<Arc<Class>, ClassLoadError>;

    fn id(&self) -> usize;

}
