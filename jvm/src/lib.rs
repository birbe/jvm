use crate::bytecode::Bytecode;
use crate::classfile::resolved::attribute::Instruction;
use crate::classfile::resolved::{Attribute, Class};
use crate::execution::MethodHandleStore;
use crate::thread::{MethodIdentifier, Thread};
use parking_lot::{Mutex, RwLock};
use std::collections::HashMap;
use std::io::Write;
use std::marker::PhantomData;
use std::pin::Pin;
use std::sync::Arc;

mod classfile;
mod tests;

mod bytecode;
pub mod execution;
mod jit;
pub mod thread;

#[derive(Clone)]
pub struct JVM<'jvm> {
    pub internal: Arc<JVMInternal<'jvm>>,
}

pub struct Heap {
    pub data: Pin<Box<[u8]>>,
}

pub struct JVMInternal<'jvm> {
    pub phantom: &'jvm PhantomData<()>,
    pub stdout: Mutex<Box<dyn Write>>,
    pub threads: RwLock<Vec<Thread>>,
    pub method_handles: MethodHandleStore<'jvm>,
    pub classes: RwLock<HashMap<String, Class>>,
    pub heap: Heap,
}

impl<'jvm> JVMInternal<'jvm> {}

fn get_method_bytecode(jvm: &JVMInternal, identifier: &MethodIdentifier) -> Vec<Instruction> {
    let classes = jvm.classes.read();
    let class = classes.get(identifier.class.to_str().unwrap()).unwrap();

    let method = class
        .methods
        .iter()
        .find(|method| &*method.name == identifier.method.to_str().unwrap())
        .unwrap();
    let code = method.attributes.get("Code").unwrap();

    if let Attribute::Code(code) = code {
        code.instructions.clone()
    } else {
        unreachable!()
    }
}

#[repr(C)]
//Some of these functions are only designated for usage in Rust. If this is the case, they're marked as such
pub struct JVMPtrs<'jvm> {
    jvm: *mut JVMInternal<'jvm>,
    //Rust only
    get_method_bytecode: fn(*mut JVMInternal, MethodIdentifier) -> (Vec<Bytecode>, Vec<u8>),
}
