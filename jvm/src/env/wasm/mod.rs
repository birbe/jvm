use std::sync::Arc;
use crate::classfile::resolved::Class;
use crate::env::{Compiler, Environment};
use crate::thread::Thread;

pub mod compile;
mod scc;
pub mod wasm;
pub mod cfg;

struct WasmEnvironment {
}

impl Environment for WasmEnvironment {
    type Compiler = WasmCompiler;

    fn compiler(&self) -> &Self::Compiler {
        todo!()
    }

    fn visit_class(class: Arc<Class>) {
        todo!()
    }

    fn step(thread: &mut Thread) {
        todo!()
    }
}

struct WasmCompiler;

impl Compiler for WasmCompiler {

}