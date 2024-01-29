use std::sync::Arc;
use crate::classfile::resolved::{Class, Ref};
use crate::execution::MethodHandle;
use crate::heap::Object;
use crate::linker::ClassLoader;
use crate::thread::{Operand, Thread, ThreadHandle};

pub mod wasm;
pub mod interpreter;
pub mod aot;
pub mod native;

pub trait Environment {

    fn link_class(&self, class: Arc<Class>);

    fn invoke_handle(&self, thread: &mut Thread, method_handle: &MethodHandle, args: Box<[Operand]>) -> u64;

    fn register_method_handle(&self, class_loader: &dyn ClassLoader, method: Arc<Ref>, handle: MethodHandle);

    fn get_object_field(&self, object: Object, class: &Class, field: &Ref) -> Operand;

}

pub trait Compiler {

}