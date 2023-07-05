use crate::JVM;

use std::ffi::{CString};
use std::mem::MaybeUninit;
use std::ops::{Deref, DerefMut};
use std::pin::Pin;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicI32};
use parking_lot::{Mutex, MutexGuard};
use crate::bytecode::{Bytecode, JValue};
use crate::classfile::resolved::{Attribute, Class, Constant, FieldType, Method, NameAndType, Ref, ReturnType};
use crate::classfile::resolved::attribute::Code;
use crate::linker::ClassLoader;

#[derive(Debug)]
pub enum ThreadError {
    UnresolvedClassDefinition,
}

#[repr(C)]
pub struct RawFrame {
    pub method_ref: *const Ref,
    pub program_counter: u32,
    pub locals_length: usize,
    pub locals: *mut u64,
    pub stack_index: usize,
    pub stack_length: usize,
    pub stack: *mut u64
}

impl RawFrame {

    pub fn new(method_identifier: &Ref, locals: Box<[u64]>, stack: Box<[u64]>) -> Self {
        let locals = Box::into_raw(locals);
        let stack = Box::into_raw(stack);

        Self {
            method_ref: method_identifier as *const Ref,
            program_counter: 0,
            locals_length: locals.len(),
            locals: locals.as_mut_ptr(),
            stack_index: 0,
            stack_length: stack.len(),
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
    pub locals: &'a mut [u64],
    pub stack_index: &'a mut usize,
    pub stack: &'a mut [u64]
}

///Stack of frames
pub struct FrameStore {
    pub frames: Pin<Box<[MaybeUninit<RawFrame>; 1024]>>,
    pub frame_index: isize,
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
        assert!(self.frame_index > -1);
        self.frames[self.frame_index as usize] = MaybeUninit::uninit();
        self.frame_index -= 1;
    }

    pub fn get_first(&mut self) -> *mut RawFrame {
        assert!(self.frame_index >= 0);
        unsafe { self.frames[self.frame_index as usize].as_mut_ptr() }
    }

}

pub enum ThreadStepResult {
    Ok(*mut RawFrame),
    Error(ThreadError),
    Return(Option<JValue>),
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

    #[must_use]
    pub fn call(&mut self, classpath: &str, method_name: &str, method_descriptor: &str, args: &[JValue]) -> Result<Option<JValue>, JavaBarrierError> {
        let frame_store = unsafe {
            self.frame_store.as_mut().get_mut() as *mut FrameStore
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

        let handle = refs.get(&method_ref).ok_or(JavaBarrierError::MethodNotFound)?;

        let u64_args: Vec<u64> = args.iter().map(|arg| arg.as_u64()).collect();

        let return_value = unsafe { handle.invoke(&u64_args, frame_store, self.thread) };

        let jvalue = match &method.descriptor.return_type {
            ReturnType::FieldType(field_type) => Some(match field_type {
                FieldType::Array { .. } => JValue::Reference(return_value as *mut ()),
                FieldType::Byte => JValue::Byte(return_value as i8),
                FieldType::Char => todo!(),
                FieldType::Double => JValue::Double(unsafe { std::mem::transmute::<_, f64>(return_value) }),
                FieldType::Float => JValue::Float(unsafe { std::mem::transmute::<_, f32>(return_value as u32) }),
                FieldType::Int => JValue::Int(return_value as i32),
                FieldType::Long => JValue::Long(return_value as i64),
                FieldType::Short => JValue::Short(return_value as i16),
                FieldType::Boolean => JValue::Int(return_value as i32),
                FieldType::Class(_) => JValue::Reference(return_value as *mut ())
            }),
            ReturnType::Void => None
        };

        Ok(jvalue)
    }

    //Conforms to the ABI
    pub unsafe extern "C" fn interpret_trampoline(
        frame_store: *mut FrameStore,
        thread: *mut Thread
    ) -> u64 {
        Self::interpret(
            (*(*frame_store).get_first()).as_frame(),
            &mut *thread,
            frame_store
        ).unwrap_or(JValue::Long(0)).as_u64()
    }

    pub fn interpret(
        mut frame: Frame,
        thread: &mut Thread,
        frame_store: *mut FrameStore
    ) -> Option<JValue> {
        let return_value = loop {
            match Self::step(
                //Safety: RawFrame has the same in-memory representation as Frame. As we have a *mut Thread, that implies
                //that the JVM is also still alive due to the Arc<JVM>. This also implies that the *const Ref and thus the &'a Ref is still valid.
                frame,
                frame_store,
                thread
            ) {
                ThreadStepResult::Ok(frame_out) => {
                    frame = unsafe { (*frame_out).as_frame() };
                }
                ThreadStepResult::Error(error) => panic!("{:?}", error),
                ThreadStepResult::Return(result) => {
                    unsafe {
                        if (*frame_store).frame_index > 0 {
                            (*frame_store).pop();

                            let lower_frame = (*(*frame_store).get_first()).as_frame();

                            match result {
                                None => {}
                                Some(value) => {}
                            }

                            frame = lower_frame;
                        } else {
                            return result;
                        }
                    }

                    break result;
                }
            }
        };

        return_value
    }

    fn step(
        mut frame: Frame,
        frame_store: *mut FrameStore,
        thread: &mut Thread
    ) -> ThreadStepResult {
        //TODO: Super slow to do this at *every* instruction
        let mut class = thread.class_loader.get_class(&frame.method_ref.class).unwrap();
        let mut method = class.get_method(&frame.method_ref.name_and_type).unwrap();

        let mut code = if let Some(Attribute::Code(code)) = method.attributes.get("Code") {
            code
        } else {
            panic!()
        };

        let pc = *frame.program_counter;
        let mut pc_inc = 1;

        let bytecode = &code.instructions[pc as usize].bytecode;

        match bytecode {
            Bytecode::Invokestatic(constant_index) => {
                let constant = class.constant_pool.constants.get(constant_index).unwrap();
                let method_ref = constant.as_ref().unwrap();
                todo!()
            },
            Bytecode::Return => {
                return ThreadStepResult::Return(None);
            },
            Bytecode::Ireturn => {
                let int = frame.stack[*frame.stack_index-1] as i32;
                return ThreadStepResult::Return(Some(JValue::Int(int)));
            }
            Bytecode::Bipush(value) => {
                frame.stack[*frame.stack_index] = *value as i32 as u64;
                *frame.stack_index += 1;
            }
            Bytecode::Aload_n(index)
            | Bytecode::Aload(index) => {
                frame.stack[*frame.stack_index] = frame.locals[*index as usize];
                *frame.stack_index += 1;
            }
            Bytecode::Areturn => {
                let ptr = frame.stack[*frame.stack_index-1];
                return ThreadStepResult::Return(Some(JValue::Reference(ptr as *mut ())));
            }
            _ => unimplemented!("Bytecode {:?} unimplemented in interpreter", bytecode)
        }

        *frame.program_counter = ((*frame.program_counter as i32) + pc_inc) as u32;

        drop(frame);

        ThreadStepResult::Ok(unsafe { (*frame_store).get_first() })
    }
}
