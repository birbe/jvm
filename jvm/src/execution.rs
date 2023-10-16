use crate::classfile::resolved::Ref;
use crate::thread::{FrameStack, RawFrame, Thread};


use std::fmt::{Debug, Formatter};

use std::ptr;
use std::sync::{Arc};

pub type ABIHandlePtr = unsafe extern "C" fn(*mut FrameStack, thread: *mut Thread) -> u64;

pub enum ExecutionContext {
    //Interpret requires that any caller pushes the appropriate frame onto the stack before calling
    Interpret(Box<dyn Fn(&[u64]) -> RawFrame>),
    JIT,
    Native,
}

impl Debug for ExecutionContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ExecutionContext::{}",
            match self {
                ExecutionContext::Interpret(_) => "Interpret",
                ExecutionContext::JIT => "JIT",
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
    pub unsafe fn invoke(
        &self,
        args: &[u64],
        frame_store: *mut FrameStack,
        thread: *mut Thread,
    ) -> u64 {
        let mut locals = Vec::from(args);

        match &self.context {
            ExecutionContext::Interpret(frame) => (*frame_store).push(frame(args)),
            ExecutionContext::JIT => {}
            ExecutionContext::Native => (*frame_store).push(RawFrame {
                method_ref: ptr::null(),
                program_counter: 0,
                locals_length: args.len(),
                locals: locals.as_mut_ptr(),
                stack_index: 0,
                stack_length: 0,
                stack: ptr::null_mut(),
            }),
        }

        let out = unsafe { (self.ptr)(frame_store, thread) };

        match &self.context {
            ExecutionContext::Interpret(_) => {}
            ExecutionContext::JIT => {}
            ExecutionContext::Native => unsafe { (*frame_store).pop() },
        }

        out
    }
}
