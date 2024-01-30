use crate::classfile::resolved::{Class, Method, Ref};
use crate::thread::{FrameStack, Operand, RawFrame, Thread};

use std::fmt::{Debug, Formatter};

use std::sync::Arc;

pub type ABIHandlePtr = unsafe extern "C" fn(thread: &mut Thread) -> Operand;

pub struct InterpreterContext {
    //RawFrame contains a pointer to the Ref and the Class, these keep the pointers alive
    ref_: Arc<Ref>,
    class: Arc<Class>,
}

impl InterpreterContext {
    pub fn new(ref_: Arc<Ref>, class: Arc<Class>) -> Self {
        Self { ref_, class }
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
    pub(crate) ptr: ABIHandlePtr,
    pub(crate) context: ExecutionContext,
    pub(crate) class: Arc<Class>,
    pub(crate) method: Arc<Method>,
}

impl Debug for MethodHandle {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MethodHandle").finish()
    }
}

impl MethodHandle {
    pub unsafe fn new(ptr: ABIHandlePtr, context: ExecutionContext, method: Arc<Method>, class: Arc<Class>) -> Self {
        Self {
            ptr,
            context,
            class,
            method,
        }
    }
}
