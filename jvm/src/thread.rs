use crate::JVM;

use std::ffi::CString;
use std::mem::MaybeUninit;
use std::ops::{Deref, DerefMut};
use std::pin::Pin;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicI32};
use parking_lot::{Mutex, MutexGuard};
use crate::bytecode::{Bytecode, JValue};
use crate::classfile::resolved::{Attribute, Class, Constant, FieldType, Method, MethodDescriptor, NameAndType, Ref, ReturnType};
use crate::classfile::resolved::attribute::Code;
use crate::heap::{Object, RawObject};
use crate::linker::ClassLoader;

#[derive(Debug)]
pub enum RuntimeException {
    NullPointer
}

#[derive(Debug)]
pub enum ThreadError {
    UnresolvedClassDefinition,
    RuntimeException(RuntimeException)
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
            stack_length: &mut self.stack_index,
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
    pub stack_length: &'a mut usize,
    pub stack: &'a mut [u64]
}

///Stack of frames
#[derive(Debug)]
pub struct FrameStack {
    pub frames: Pin<Box<[MaybeUninit<RawFrame>; 1024]>>,
    pub frame_index: isize,
}

impl<'jvm> FrameStack {
    pub fn new() -> Self {
        const UNINIT: MaybeUninit<RawFrame> = MaybeUninit::uninit();

        FrameStack {
            frames: Pin::new(Box::new([UNINIT; 1024])),
            frame_index: -1
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
    pub frame_store: Pin<Box<FrameStack>>,
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
            self.frame_store.as_mut().get_mut() as *mut FrameStack
        };

        let class = self.class_loader.get_class(classpath).ok_or(JavaBarrierError::ClassNotFound)?;
        let method = class.methods.iter().find(|method| *method.name == method_name && method_descriptor == method_descriptor).ok_or(JavaBarrierError::MethodNotFound)?;

        let handle = {
            let id = self.class_loader.id();
            let class_loader_stores = self.jvm.class_loaders.read();
            let store = &class_loader_stores[id];
            let refs = store.method_refs.read();

            let method_ref = Ref {
                class: Arc::new(classpath.to_string()),
                name_and_type: Arc::new(NameAndType { name: Arc::new(method_name.to_string()), descriptor: Arc::new(method_descriptor.to_string()) }),
            };

            refs.get(&method_ref).ok_or(JavaBarrierError::MethodNotFound)?.clone()
        };

        let u64_args: Vec<u64> = args.iter().map(|arg| arg.as_u64()).collect();

        let return_value = unsafe { handle.invoke(&u64_args, frame_store, self.thread) };

        let jvalue = match &method.descriptor.return_type {
            ReturnType::FieldType(field_type) => Some(match field_type {
                FieldType::Array { .. } => JValue::Reference(unsafe { Object::from_raw(return_value as usize as *mut RawObject<()>) }),
                FieldType::Byte => JValue::Byte(return_value as i8),
                FieldType::Char => todo!(),
                FieldType::Double => JValue::Double(unsafe { std::mem::transmute::<_, f64>(return_value) }),
                FieldType::Float => JValue::Float(unsafe { std::mem::transmute::<_, f32>(return_value as u32) }),
                FieldType::Int => JValue::Int(return_value as i32),
                FieldType::Long => JValue::Long(return_value as i64),
                FieldType::Short => JValue::Short(return_value as i16),
                FieldType::Boolean => JValue::Int(return_value as i32),
                FieldType::Class(_) => JValue::Reference(unsafe { Object::from_raw(return_value as usize as *mut RawObject<()>) })
            }),
            ReturnType::Void => None
        };

        Ok(jvalue)
    }

    //Conforms to the ABI
    pub unsafe extern "C" fn interpret_trampoline(
        frame_store: *mut FrameStack,
        thread: *mut Thread
    ) -> u64 {
        Self::interpret(
            (*(*frame_store).get_first()).as_frame(),
            &mut *thread,
            thread,
            frame_store
        ).unwrap_or(JValue::Long(0)).as_u64()
    }

    pub fn interpret(
        mut frame: Frame,
        thread: &mut Thread,
        thread_raw: *mut Thread,
        frame_store: *mut FrameStack
    ) -> Option<JValue> {
        let return_value = loop {
            match Self::step(
                //Safety: RawFrame has the same in-memory representation as Frame. As we have a *mut Thread, that implies
                //that the JVM is also still alive due to the Arc<JVM>. This also implies that the *const Ref and thus the &'a Ref is still valid.
                frame,
                frame_store,
                thread,
                thread_raw
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

                            match &result {
                                None => {}
                                Some(value) => {
                                    lower_frame.stack[*lower_frame.stack_length] = value.as_u64();
                                    *lower_frame.stack_length += 1;
                                }
                            }

                            frame = lower_frame;
                        } else {
                            return result;
                        }
                    }
                }
            }
        };

        return_value
    }

    fn step(
        mut frame: Frame,
        frame_store: *mut FrameStack,
        thread: &mut Thread,
        thread_raw: *mut Thread
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

        let instruction = &code.instructions[pc as usize];
        let bytes_index = instruction.bytes_index;
        let bytecode = &instruction.bytecode;

        match bytecode {
            Bytecode::Invokestatic(constant_index) => {
                if let Some(Constant::MethodRef(ref_)) = class.constant_pool.constants.get(constant_index) {
                    let method_handle = {
                        let loaders = thread.jvm.class_loaders.read();
                        let loader_store = &loaders[thread.class_loader.id()];
                        let refs = loader_store.method_refs.read();

                        refs.get(ref_).unwrap().clone()
                    };

                    let method_descriptor = MethodDescriptor::try_from(&**ref_.name_and_type.descriptor).unwrap();

                    let args = &frame.stack[*frame.stack_length-method_descriptor.args.len()..];

                    unsafe { method_handle.invoke(args, frame_store, thread_raw); };

                    *frame.stack_length -= method_descriptor.args.len();
                }
            },
            Bytecode::Return => {
                return ThreadStepResult::Return(None);
            },
            Bytecode::Ireturn => {
                let int = frame.stack[*frame.stack_length -1] as i32;
                return ThreadStepResult::Return(Some(JValue::Int(int)));
            }
            Bytecode::Bipush(value) => {
                frame.stack[*frame.stack_length] = *value as i32 as u64;
                *frame.stack_length += 1;
            }
            Bytecode::Aload_n(index)
            | Bytecode::Aload(index) => {
                frame.stack[*frame.stack_length] = frame.locals[*index as usize];
                *frame.stack_length += 1;
            }
            Bytecode::Areturn => {
                let ptr = frame.stack[*frame.stack_length -1];
                return ThreadStepResult::Return(Some(JValue::Reference(unsafe { Object::from_raw(ptr as *mut RawObject<()>) })));
            }
            Bytecode::Iconst_n_m1(value) => {
                frame.stack[*frame.stack_length] = *value as i32 as u64;
                *frame.stack_length += 1;
            }
            Bytecode::If_icmpge(offset) => {
                let a = frame.stack[*frame.stack_length - 2];
                let b = frame.stack[*frame.stack_length - 1];
                *frame.stack_length -= 2;

                if a >= b {
                    let target_offset = bytes_index as isize + *offset as isize;
                    let idx = code.instructions.iter().find(|instr| instr.bytes_index == target_offset as u32)
                        .unwrap();
                    pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
                }
            }
            Bytecode::Istore(index)
            | Bytecode::Istore_n(index)
            | Bytecode::Astore(index)
            | Bytecode::Astore_n(index) => {
                frame.locals[*index as usize] = frame.stack[*frame.stack_length-1];
                *frame.stack_length -= 1;
            }
            Bytecode::Iload(index)
            | Bytecode::Iload_n(index) => {
                frame.stack[*frame.stack_length] = frame.locals[*index as usize];
                *frame.stack_length += 1;
            }
            Bytecode::Sipush(short) => {
                frame.stack[*frame.stack_length] = *short as i32 as u64;
                *frame.stack_length += 1;
            }
            Bytecode::Iinc(index, inc) => {
                frame.locals[*index as usize] = (frame.locals[*index as usize] as i64 + *inc as i64) as u64;
            }
            Bytecode::Goto(offset) => {
                let target_offset = bytes_index as isize + *offset as isize;
                let idx = code.instructions.iter().find(|instr| instr.bytes_index == target_offset as u32)
                    .unwrap();
                pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
            }
            Bytecode::Ldc(constant_index) => {
                let constant = class.constant_pool.constants.get(&(*constant_index as u16)).unwrap();

                match constant {
                    Constant::Integer(int) => {
                        frame.stack[*frame.stack_length] = *int as u64;
                        *frame.stack_length += 1;
                    }
                    Constant::Float(float) => {
                        frame.stack[*frame.stack_length] = unsafe { std::mem::transmute::<f32, u32>(*float) as u64 };
                        *frame.stack_length += 1;
                    }
                    Constant::String(string) => {
                        let string_object = thread.jvm.heap.get_string(&string, &thread.jvm);

                        frame.stack[*frame.stack_length] = string_object.value.get_raw() as usize as u64;
                        *frame.stack_length += 1;
                    }
                    _ => unimplemented!()
                }
            }
            Bytecode::Aastore => {
                let value = unsafe { Object {
                    ptr: frame.stack[*frame.stack_length-1],
                } };
                let index = frame.stack[*frame.stack_length-2];
                let array_ref = unsafe { Object {
                    ptr: frame.stack[*frame.stack_length-3],
                } };

                if array_ref == Object::NULL {
                    return ThreadStepResult::Error(ThreadError::RuntimeException(RuntimeException::NullPointer));
                }

                *frame.stack_length -= 3;

                let array = unsafe { array_ref.cast_array::<Object>() };
                unsafe { (*array).body.body[index as usize] = value; }
            }
            Bytecode::Anewarray(constant_index) => {
                let count = frame.stack[*frame.stack_length-1] as i32;
                let constant = class.constant_pool.constants.get(constant_index).unwrap();
                let classpath = constant.as_string().unwrap();

                let array_class = thread.jvm.find_class(&classpath, thread.class_loader.clone()).unwrap();
                let object = unsafe { Object::from_raw(thread.jvm.heap.allocate_raw_object_array(&array_class, count)) };

                frame.stack[*frame.stack_length] = object.ptr;
                *frame.stack_length += 1;
            }
            _ => unimplemented!("Bytecode {:?} unimplemented in interpreter", bytecode)
        }

        *frame.program_counter = ((*frame.program_counter as i32) + pc_inc) as u32;

        drop(frame);

        ThreadStepResult::Ok(unsafe { (*frame_store).get_first() })
    }
}
