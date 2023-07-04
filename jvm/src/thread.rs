use crate::JVM;

use std::ffi::{CString};
use std::mem::MaybeUninit;
use std::ops::{Deref, DerefMut};
use std::pin::Pin;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicI32};
use parking_lot::{Mutex, MutexGuard};
use crate::classfile::resolved::{Attribute, Class, Method, NameAndType, Ref};
use crate::classfile::resolved::attribute::Code;
use crate::linker::ClassLoader;

pub enum ThreadError {
    UnresolvedClassDefinition,
}

#[repr(C)]
#[derive(Clone)]
pub struct RawFrame {
    pub method_ref: *const Ref,
    pub program_counter: u32,
    pub locals_length: usize,
    pub locals: *mut i32,
    pub stack_index: usize,
    pub stack_length: usize,
    pub stack: *mut i32
}

impl RawFrame {

    pub fn new(method_identifier: &Ref, locals: Box<[i32]>, stack: Box<[i32]>) -> Self {
        let locals = Box::into_raw(locals);
        let stack = Box::into_raw(stack);

        Self {
            method_ref: method_identifier as *const Ref,
            program_counter: 0,
            locals_length: locals.len(),
            locals: locals.as_mut_ptr(),
            stack_index: 0,
            stack_length: 0,
            stack: stack.as_mut_ptr(),
        }
    }

    pub unsafe fn as_frame(&mut self) -> Frame {
        Frame {
            method_ref: &*self.method_ref,
            program_counter: &mut self.program_counter,
            locals: std::slice::from_raw_parts_mut(self.locals, self.locals_length),
            stack_index: &mut self.stack_index,
            stack: std::slice::from_raw_parts_mut(self.stack, self.stack_length),
        }
    }

}

impl Drop for RawFrame {
    fn drop(&mut self) {
        unsafe {
            drop(Box::from_raw(std::slice::from_raw_parts_mut(self.stack, self.stack_length)));
            drop(Box::from_raw(std::slice::from_raw_parts_mut(self.locals, self.locals_length)));
        }
    }
}

#[repr(C)]
pub struct Frame<'a> {
    pub method_ref: &'a Ref,
    pub program_counter: &'a mut u32,
    pub locals: &'a mut [i32],
    pub stack_index: &'a mut usize,
    pub stack: &'a mut [i32]
}

pub struct FrameStore {
    pub frames: Pin<Box<[MaybeUninit<RawFrame>; 1024]>>,
    pub frame_index: usize,
}

impl<'jvm> FrameStore {
    pub fn new() -> Self {
        const UNINIT: MaybeUninit<RawFrame> = MaybeUninit::uninit();

        FrameStore {
            frames: Pin::new(Box::new([UNINIT; 1024])),
            frame_index: 0,
        }
    }

    pub fn push(&mut self, frame: RawFrame) {
        self.frame_index += 1;
        self.frames[self.frame_index as usize] = MaybeUninit::new(frame);
    }

    pub fn pop(&mut self) {
        assert!(self.frame_index > 0);
        self.frames[self.frame_index as usize] = MaybeUninit::uninit();
        self.frame_index -= 1;
    }
}

pub enum ThreadStepResult {
    Ok,
    Error(ThreadError),
    Result(i64),
}

#[repr(C)]
pub struct Thread {
    pub jvm: Arc<JVM>,
    pub class_loader: Arc<dyn ClassLoader>,
    pub frame_store: Pin<Box<FrameStore>>,
    pub killed: bool
}

#[derive(Debug)]
pub enum JavaBarrierError {
    ClassNotFound,
    MethodNotFound
}

pub struct ThreadHandle {
    pub thread: *mut Thread,
    pub guard: Arc<Mutex<Thread>>
}

impl Deref for ThreadHandle {
    type Target = Thread;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.thread }
    }
}

impl DerefMut for ThreadHandle {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.thread }
    }
}

impl Drop for ThreadHandle {
    fn drop(&mut self) {
        unsafe {
            self.guard.force_unlock();
        }
    }
}

impl ThreadHandle {

    pub fn call(&mut self, classpath: &str, method_name: &str, method_descriptor: &str, args: &[i32]) -> Result<Option<i64>, JavaBarrierError> {
        let frame_store = unsafe {
            std::mem::transmute::<_, *mut FrameStore>(&mut self.frame_store)
        };

        let class = self.class_loader.get_class(classpath).ok_or(JavaBarrierError::ClassNotFound)?;
        let method = class.methods.iter().find(|method| *method.name == method_name && method_descriptor == method_descriptor).ok_or(JavaBarrierError::MethodNotFound)?;

        let id = self.class_loader.id();
        let class_loader_stores = self.jvm.class_loaders.read();
        let store = &class_loader_stores[id];
        let refs = store.method_refs.read();

        let method_ref = Ref {
            class: Arc::new(classpath.to_string()),
            name_and_type: Arc::new(NameAndType { name: Arc::new(method_name.to_string()), descriptor: Arc::new(method_descriptor.to_string()) }),
        };

        let handle = refs.get(&method_ref).unwrap();
        Ok(Some(unsafe { handle.invoke(args, frame_store, self.thread) }))
    }

    //Conforms to the ABI
    pub unsafe fn interpret_trampoline(
        frame_store: *mut FrameStore,
        thread: *mut Thread
    ) -> i64 {
        Self::interpret(
            //Safety: RawFrame has the same in-memory representation as Frame. As we have a *mut Thread, that implies
            //that the JVM is also still alive due to the Arc<JVM>. This also implies that the *const Ref and thus the &'a Ref is still valid.
            std::mem::transmute(&mut (*frame_store).frames[(*frame_store).frame_index]),
            &mut (*frame_store).frames.as_mut()[(*frame_store).frame_index..1024],
            &mut *thread
        )
        .unwrap_or(0)
    }

    pub fn interpret(
        frame: &mut Frame,
        frames: &mut [MaybeUninit<RawFrame>],
        thread: &mut Thread
    ) -> Option<i64> {
        let class = thread.class_loader.get_class(&frame.method_ref.class).unwrap();
        let method = class.get_method(&frame.method_ref.name_and_type).unwrap();

        let code = if let Some(Attribute::Code(code)) = method.attributes.get("Code") {
            code
        } else {
            panic!()
        };

        let return_value = loop {
            match Self::step(&class, method, code, frame, frames) {
                ThreadStepResult::Ok => {}
                ThreadStepResult::Error(_) => panic!(),
                ThreadStepResult::Result(result) => {
                    break result;
                }
            }
        };

        Some(return_value)
    }

    fn step(
        class: &Class,
        method: &Method,
        code: &Code,
        frame: &mut Frame,
        _frames: &mut [MaybeUninit<RawFrame>],
    ) -> ThreadStepResult {
        let pc = *frame.program_counter;
        let mut pc_inc = 0;



        *frame.program_counter = ((*frame.program_counter as i32) + pc_inc) as u32;

        ThreadStepResult::Ok
    }
}
