use crate::thread::{Frame, FrameStore};
use std::collections::{HashMap, HashSet};
use std::ffi::CString;
use std::pin::Pin;
use std::sync::Mutex;

pub type ABIHandlePtr = dyn Fn(u64, *const i32, *mut Frame) -> i64;

pub enum ExecutionContext<'jvm> {
    //Interpret requires that any caller pushes the appropriate frame onto the stack before calling
    Interpret(Frame<'jvm>),
    JIT,
}

pub struct MethodHandle<'jvm> {
    pub ptr: unsafe fn(u64, *const i32, *mut Frame) -> i64,
    pub context: ExecutionContext<'jvm>,
}

impl<'jvm> MethodHandle<'jvm> {
    pub unsafe fn execute(&self, args: &[i32], frame_store: &mut FrameStore<'jvm>) -> i64 {
        match self.context {
            ExecutionContext::Interpret(frame) => frame_store.push(Frame {
                method_identifier: frame.method_identifier,
                program_counter: 0,
            }),
            ExecutionContext::JIT => {}
        }

        (self.ptr)(
            args.len() as u64,
            args.as_ptr(),
            frame_store.frames.as_mut_ptr() as *mut Frame,
        )
    }
}

pub struct MethodHandleStore<'jvm> {
    pub methods: Mutex<HashMap<Frame<'jvm>, MethodHandle<'jvm>>>,
    pub cstring_store: HashSet<Pin<CString>>,
}

impl<'jvm> MethodHandleStore<'jvm> {
    
    pub fn new() -> Self {
        Self {
            methods: Mutex::new(Default::default()),
            cstring_store: Default::default(),
        }
    }
    
}