use std::sync::Arc;
use crate::classfile::resolved::Class;
use crate::JVM;
use crate::thread::Thread;

pub mod wasm;
pub mod interpreter;
pub mod aot;

pub trait Environment {

    type Compiler;

    fn compiler(&self) -> &Self::Compiler;

    fn visit_class(class: Arc<Class>);

    fn step(thread: &mut Thread);

}

pub trait Compiler {

}