use crate::thread::{RawFrame, FrameStore, Thread};
use std::collections::{HashMap, HashSet};
use std::ffi::CString;
use std::pin::Pin;
use std::sync::{Arc, Mutex};
use crate::classfile::resolved::Ref;

pub type ABIHandlePtr = unsafe extern "C" fn(*mut FrameStore, thread: *mut Thread) -> i64;

pub enum ExecutionContext {
    //Interpret requires that any caller pushes the appropriate frame onto the stack before calling
    Interpret(RawFrame),
    JIT,
}

pub struct MethodHandle {
    pub ptr: ABIHandlePtr,
    pub context: ExecutionContext,
    pub method_ref: Arc<Ref>,
    pub stack_size: usize
}

impl MethodHandle {
    pub fn invoke(&self, args: &[i32], frame_store: &mut FrameStore, thread: &mut Thread) -> i64 {
        match &self.context {
            ExecutionContext::Interpret(frame) => frame_store.push(
                RawFrame::new(
                    &self.method_ref,
                    Vec::from(args).into_boxed_slice(),
                    vec![0; self.stack_size].into_boxed_slice()
                )
            ),
            ExecutionContext::JIT => {}
        }

        unsafe {
            (self.ptr)(
                frame_store as *mut FrameStore,
                thread as *mut Thread
            )
        }
    }
}