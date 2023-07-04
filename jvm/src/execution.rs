use crate::thread::{RawFrame, FrameStore, Thread};
use std::collections::{HashMap, HashSet};
use std::ffi::CString;
use std::pin::Pin;
use std::sync::{Arc, Mutex};
use crate::classfile::resolved::Ref;

pub type ABIHandlePtr = unsafe extern "C" fn(*mut FrameStore, thread: *mut Thread) -> i64;

pub enum ExecutionContext {
    //Interpret requires that any caller pushes the appropriate frame onto the stack before calling
    Interpret(Box<dyn Fn(&[i32]) -> RawFrame>),
    JIT,
    Native
}

pub struct MethodHandle {
    pub ptr: ABIHandlePtr,
    pub context: ExecutionContext,
    pub method_ref: Arc<Ref>
}

impl MethodHandle {
    pub unsafe fn invoke(&self, args: &[i32], frame_store: *mut FrameStore, thread: *mut Thread) -> i64 {
        match &self.context {
            ExecutionContext::Interpret(frame) => (*frame_store).push(
                frame(args)
            ),
            ExecutionContext::JIT => {}
            ExecutionContext::Native => {}
        }

        unsafe {
            (self.ptr)(
                frame_store,
                thread
            )
        }
    }
}