use crate::classfile::resolved::{Class, Method, Ref};
use crate::thread::{FrameStack, Operand, RawFrame, Thread};


use std::fmt::{Debug, Formatter};

use std::sync::{Arc};

pub type ABIHandlePtr = unsafe extern "C" fn(&mut FrameStack, thread: &mut Thread) -> Operand;

#[link(wasm_import_module = "JVM")]
extern "C" {

    pub fn invoke_table_function(function_id: u32, frame_stack: &mut FrameStack, thread: &mut Thread);

}

pub struct InterpreterContext {
    //RawFrame contains a pointer to the Ref and the Class, these keep the pointers alive
    ref_: Arc<Ref>,
    class: Arc<Class>
}

impl InterpreterContext {

    pub fn new(ref_: Arc<Ref>, class: Arc<Class>) -> Self {
        Self {
            ref_,
            class
        }
    }

}

pub enum ExecutionContext {
    //Interpret requires that any caller pushes the appropriate frame onto the stack before calling
    Interpret(InterpreterContext),
    Compiled,
    Native,
}

impl Debug for ExecutionContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ExecutionContext::{}",
            match self {
                ExecutionContext::Interpret(_) => "Interpret",
                ExecutionContext::Compiled => "JIT",
                ExecutionContext::Native => "Native",
            }
        )
    }
}

pub struct MethodHandle {
    pub ptr: ABIHandlePtr,
    pub context: ExecutionContext,
    pub method_ref: Arc<Ref>,
}

impl MethodHandle {
    pub fn invoke(
        &self,
        args: &[Operand],
        frame_store: &mut FrameStack,
        thread: &mut Thread,
    ) -> Operand {
        let mut locals = Vec::from(args);

        match &self.context {
            ExecutionContext::Interpret(context) => todo!(),
            ExecutionContext::Compiled => {}
            ExecutionContext::Native => (*frame_store).push(RawFrame {
                method_ref: &*self.method_ref,
                class: std::ptr::null(),
                program_counter: 0,
                locals_length: args.len(),
                locals: locals.as_mut_ptr(),
                stack_index: 0,
                stack_length: 0,
                stack: std::ptr::null_mut(),
            }),
        }

        std::mem::forget(locals);

        let out = unsafe { (self.ptr)(frame_store, thread) };

        match &self.context {
            ExecutionContext::Interpret(_) => {}
            ExecutionContext::Compiled => {}
            ExecutionContext::Native => { frame_store.pop(); },
        }

        out
    }
}
