use crate::{JVM, routine_resolve_field};



use crate::bytecode::{Bytecode, JValue};

use crate::classfile::resolved::{AccessFlags, Attribute, Class, Constant, Field, FieldType, Method, MethodDescriptor, NameAndType, Ref, ReferenceKind, ReturnType};
use crate::heap::{Int, Object, RawObject};
use crate::linker::ClassLoader;
use bitflags::Flags;
use colored::Colorize;
use parking_lot::{Mutex};

use std::mem::{MaybeUninit};
use std::ops::{Deref, DerefMut};
use std::pin::Pin;

use std::sync::atomic::{Ordering};
use std::sync::Arc;
use arrayvec::ArrayVec;

#[derive(Debug)]
pub enum RuntimeException {
    NullPointer,
}

#[derive(Debug)]
pub enum ThreadError {
    UnresolvedClassDefinition,
    RuntimeException(RuntimeException),
}

#[repr(C)]
pub struct RawFrame {
    pub(crate) method_ref: *const Ref,
    pub(crate) program_counter: u32,
    pub(crate) locals_length: usize,
    pub(crate) locals: *mut u64,
    pub(crate) stack_index: usize,
    pub(crate) stack_length: usize,
    pub(crate) stack: *mut u64,
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

    pub fn as_frame(&mut self) -> Frame {
        unsafe {
            Frame {
                method_ref: &*self.method_ref,
                program_counter: &mut self.program_counter,
                locals: std::slice::from_raw_parts_mut(self.locals, self.locals_length),
                stack_length: &mut self.stack_index,
                stack: std::slice::from_raw_parts_mut(self.stack, self.stack_length),
            }
        }
    }
}

impl Drop for RawFrame {
    fn drop(&mut self) {
        unsafe {
            drop(Box::from_raw(std::slice::from_raw_parts_mut(
                self.stack,
                self.stack_length,
            )));
            drop(Box::from_raw(std::slice::from_raw_parts_mut(
                self.locals,
                self.locals_length,
            )));
        }
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct Frame<'a> {
    pub method_ref: &'a Ref,
    pub program_counter: &'a mut u32,
    pub locals: &'a mut [u64],
    pub stack_length: &'a mut usize,
    pub stack: &'a mut [u64],
}

impl<'a> Frame<'a> {

    pub fn pop<const N: usize>(&mut self) -> [u64; N] {
        assert!(*self.stack_length >= N);

        *self.stack_length -= N;
        (&self.stack[*self.stack_length-N..*self.stack_length]).try_into().unwrap()
    }

    pub fn push<const N: usize>(&mut self, operands: [u64; N]) {
        let len = *self.stack_length;
        (&mut self.stack[len..len+N]).copy_from_slice(&operands);
        *self.stack_length += N;
    }

}

///Stack of frames
#[derive(Debug)]
pub struct FrameStack {
    pub frames: Pin<Box<[MaybeUninit<RawFrame>; 1024]>>,
    pub frame_index: isize,
}

pub struct FrameStackView<'a> {
    pub frames: &'a mut [RawFrame]
}

impl<'jvm> FrameStack {
    pub fn new() -> Self {
        const UNINIT: MaybeUninit<RawFrame> = MaybeUninit::uninit();

        FrameStack {
            frames: Pin::new(Box::new([UNINIT; 1024])),
            frame_index: -1,
        }
    }

    pub fn push(&mut self, frame: RawFrame) {
        self.frame_index += 1;
        self.frames[self.frame_index as usize] = MaybeUninit::new(frame);
    }

    pub fn pop(&mut self) -> RawFrame {
        assert!(self.frame_index > -1);
        let frame = std::mem::replace(&mut self.frames[self.frame_index as usize], MaybeUninit::uninit());
        self.frame_index -= 1;

        unsafe { frame.assume_init() }
    }

    pub fn get_first(&mut self) -> *mut RawFrame {
        assert!(self.frame_index >= 0);
        unsafe { self.frames[self.frame_index as usize].as_mut_ptr() }
    }
}

pub enum ThreadStepResult {
    Ok(RawFrame),
    Error(ThreadError),
    Return(Option<JValue>),
}

#[repr(C)]
pub struct Thread {
    pub jvm: Arc<JVM>,
    pub class_loader: Arc<dyn ClassLoader>,
    pub frame_store: Pin<Box<FrameStack>>,
    pub killed: bool,
}

#[derive(Debug)]
pub enum JavaBarrierError {
    ClassNotFound,
    MethodNotFound,
}

pub struct ThreadHandle {
    pub thread: *mut Thread,
    pub guard: Arc<Mutex<Thread>>,
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
    pub fn call(
        &mut self,
        classpath: &str,
        method_name: &str,
        method_descriptor: &str,
        args: &[JValue],
    ) -> Result<Option<JValue>, JavaBarrierError> {
        let class = self
            .class_loader
            .get_class(classpath)
            .ok_or(JavaBarrierError::ClassNotFound)?;
        let method = class
            .methods
            .iter()
            .find(|method| *method.name == method_name && method_descriptor == method_descriptor)
            .ok_or(JavaBarrierError::MethodNotFound)?;

        let handle = {
            let id = self.class_loader.id();
            let class_loader_stores = self.jvm.class_loaders.read();
            let store = &class_loader_stores[id];
            let refs = store.method_refs.read();

            let method_ref = Ref {
                class: Arc::new(classpath.to_string()),
                name_and_type: Arc::new(NameAndType {
                    name: Arc::new(method_name.to_string()),
                    descriptor: Arc::new(method_descriptor.to_string()),
                }),
            };

            refs.get(&method_ref)
                .ok_or(JavaBarrierError::MethodNotFound)?
                .clone()
        };

        let u64_args: Vec<u64> = args.iter().map(|arg| arg.as_u64()).collect();

        let thread_ptr = self.thread;
        let return_value = handle.invoke(&u64_args, &mut self.frame_store, unsafe { &mut *thread_ptr });

        let jvalue = match &method.descriptor.return_type {
            ReturnType::FieldType(field_type) => Some(match field_type {
                FieldType::Array { .. } => JValue::Reference(unsafe {
                    Object::from_raw(return_value as usize as *mut RawObject<()>)
                }),
                FieldType::Byte => JValue::Byte(return_value as i8),
                FieldType::Char => todo!(),
                FieldType::Double => {
                    JValue::Double(f64::from_ne_bytes(return_value.to_ne_bytes()))
                }
                FieldType::Float => {
                    JValue::Float(f32::from_ne_bytes((return_value as u32).to_ne_bytes()))
                }
                FieldType::Int => JValue::Int(return_value as i32),
                FieldType::Long => JValue::Long(return_value as i64),
                FieldType::Short => JValue::Short(return_value as i16),
                FieldType::Boolean => JValue::Int(return_value as i32),
                FieldType::Class(_) => JValue::Reference(unsafe {
                    Object::from_raw(return_value as usize as *mut RawObject<()>)
                }),
            }),
            ReturnType::Void => None,
        };

        Ok(jvalue)
    }

    pub extern "C" fn interpret(
        frame_stack: &mut FrameStack,
        thread: &mut Thread,
    ) -> u64 {

        let mut raw_frame = frame_stack.pop();

        loop {
            match Self::step(raw_frame, frame_stack, thread) {
                ThreadStepResult::Ok(frame) => raw_frame = frame,
                ThreadStepResult::Error(error) => panic!("{:?}", error),
                ThreadStepResult::Return(value) => {
                    return value.unwrap_or(JValue::Long(0)).as_u64()
                }
            }
        }
    }

    fn init_static(class: &Class, frame_stack: &mut FrameStack, thread: &mut Thread) {
        match &class.super_class {
            None => {}
            Some(superclass) => Self::init_static(superclass, frame_stack, thread),
        }

        if !class.methods.iter().any(|field| *field.name == "<clinit>") {
            return;
        }

        let status = class.static_init_state.load(Ordering::Acquire);

        //Class statics have already been initialized and <clinit> should not be called
        if status == 2 {
            return;
        }

        //Attempt to set the class statics into the "currently initializing" status
        match class
            .static_init_state
            .compare_exchange(0, 1, Ordering::SeqCst, Ordering::SeqCst)
        {
            //This thread has to call <clinit>
            Ok(_) => {
                let clinit_ref = Ref {
                    class: class.this_class.clone(),
                    name_and_type: Arc::new(NameAndType {
                        name: Arc::new("<clinit>".to_string()),
                        descriptor: Arc::new("()V".to_string()),
                    }),
                };

                let method_handle = {
                    let loaders = thread.jvm.class_loaders.read();
                    let loader_store = &loaders[thread.class_loader.id()];

                    let refs = loader_store.method_refs.read();

                    refs.get(&clinit_ref).unwrap().clone()
                };

                unsafe {
                    method_handle.invoke(&[], frame_stack, thread);
                };

                class.static_init_state.store(2, Ordering::Release);
            }
            //Another thread is initializing, so wait until that's not the case
            Err(_) => while class.static_init_state.load(Ordering::Acquire) == 1 {},
        }
    }

    fn step(
        mut raw_frame: RawFrame,
        frame_stack: &mut FrameStack,
        thread: &mut Thread
    ) -> ThreadStepResult {
        let mut frame = raw_frame.as_frame();

        //TODO: Super slow to do this at *every* instruction
        let class = thread
            .class_loader
            .get_class(&frame.method_ref.class)
            .unwrap();
        let method = class.get_method(&frame.method_ref.name_and_type).unwrap();

        let code = if let Some(Attribute::Code(code)) = method.attributes.get("Code") {
            code
        } else {
            panic!()
        };

        let pc = *frame.program_counter;
        let mut pc_inc = 1;

        let instruction = &code.instructions[pc as usize];
        let bytes_index = instruction.bytes_index;
        let bytecode = &instruction.bytecode;

        println!(
            "{:<25} | {:<10} | {:<50} | {:<10}",
            format!("{bytecode:?}").yellow(),
            frame.program_counter,
            method.name.green(),
            class.this_class.blue()
        );

        match bytecode {
            Bytecode::Invokestatic(constant_index) => {
                let ref_ = class
                    .constant_pool
                    .constants
                    .get(constant_index)
                    .unwrap()
                    .as_ref()
                    .unwrap();

                let method_handle = {
                    let loaders = thread.jvm.class_loaders.read();
                    let loader_store = &loaders[thread.class_loader.id()];
                    let refs = loader_store.method_refs.read();

                    refs.get(ref_).expect(&format!("{:?}", ref_)).clone()
                };

                let method_descriptor =
                    MethodDescriptor::try_from(&**ref_.name_and_type.descriptor).unwrap();

                let args = &frame.stack[*frame.stack_length - method_descriptor.args.len()..];

                let result = unsafe { method_handle.invoke(args, frame_stack, thread) };

                *frame.stack_length -= method_descriptor.args.len();

                if method_descriptor.return_type != ReturnType::Void {
                    frame.stack[*frame.stack_length] = result;
                    *frame.stack_length += 1;
                }
            }
            Bytecode::Invokespecial(constant_index) => {
                let method_ref = class
                    .constant_pool
                    .constants
                    .get(constant_index)
                    .unwrap()
                    .as_ref()
                    .unwrap();

                let classpath = &method_ref.class;
                let target_class = thread
                    .jvm
                    .find_class(classpath, thread.class_loader.clone())
                    .unwrap();

                let method = target_class.get_method(&method_ref.name_and_type).unwrap();

                let c = if {
                    !method.is_init(classpath, thread.class_loader.clone(), &thread.jvm)
                        && !target_class.access_flags.contains(AccessFlags::INTERFACE)
                        && JVM::is_subclass(&class, &target_class)
                        && target_class.access_flags.contains(AccessFlags::SUPER)
                } {
                    &**class.super_class.as_ref().unwrap()
                } else {
                    &target_class
                };

                Self::init_static(c, frame_stack, thread);

                let method = match c.get_method(&method_ref.name_and_type) {
                    None => todo!(),
                    Some(method) => method,
                };

                if method.access_flags.contains(AccessFlags::SYNCHRONIZED)
                    | method.access_flags.contains(AccessFlags::NATIVE)
                {
                    todo!()
                }

                let method_descriptor =
                    MethodDescriptor::try_from(&**method_ref.name_and_type.descriptor).unwrap();

                let args = &frame.stack[*frame.stack_length - method_descriptor.args.len() - 1..];
                *frame.stack_length -= method_descriptor.args.len() + 1;

                if method_descriptor.return_type != ReturnType::Void {
                    panic!()
                }

                let method_handle = {
                    let loaders = thread.jvm.class_loaders.read();
                    let loader_store = &loaders[thread.class_loader.id()];
                    let refs = loader_store.method_refs.read();

                    refs.get(method_ref).unwrap().clone()
                };

                unsafe { method_handle.invoke(args, frame_stack, thread) };
            }
            Bytecode::Invokevirtual(constant_index) => {
                let method_ref = class
                    .constant_pool
                    .constants
                    .get(constant_index)
                    .unwrap()
                    .as_ref()
                    .unwrap();

                let classpath = &**method_ref.class;
                let target_class = thread
                    .jvm
                    .find_class(classpath, thread.class_loader.clone())
                    .unwrap();

                let method = target_class.get_method(&method_ref.name_and_type).unwrap();

                if {
                    classpath != "java/lang/invoke/MethodHandle"
                        && classpath != "java/lang/invoke/VarHandle"
                        && !(method.descriptor.args.len() == 1
                            && method.descriptor.args[0]
                                == FieldType::Array {
                                    type_: Box::new(FieldType::Class("java/lang/Object".into())),
                                    dimensions: 1,
                                })
                        && !(method.access_flags.contains(AccessFlags::VARARGS)
                            && method.access_flags.contains(AccessFlags::NATIVE))
                } {
                    let _resolved_method = if method.access_flags.contains(AccessFlags::PRIVATE) {
                        method
                    } else {
                        todo!()
                    };

                    let method_handle = {
                        let loaders = thread.jvm.class_loaders.read();
                        let loader_store = &loaders[thread.class_loader.id()];
                        let refs = loader_store.method_refs.read();

                        refs.get(method_ref).unwrap().clone()
                    };

                    let method_descriptor =
                        MethodDescriptor::try_from(&**method_ref.name_and_type.descriptor).unwrap();

                    if method_descriptor.return_type != ReturnType::Void {
                        panic!()
                    }

                    let args =
                        &frame.stack[*frame.stack_length - method_descriptor.args.len() - 1..];

                    method_handle.invoke(args, frame_stack, thread);

                    *frame.stack_length -= method_descriptor.args.len() + 1;
                } else {
                    todo!()
                }
            }
            Bytecode::Invokedynamic(constant_index) => {
                let dynamic = class
                    .constant_pool
                    .constants
                    .get(constant_index)
                    .unwrap()
                    .as_dynamic()
                    .unwrap();

                let bootstrap = &class
                    .attributes
                    .iter()
                    .find(|attr| matches!(attr, Attribute::BootstrapMethods(_)))
                    .unwrap();

                let bootstrap_methods = match bootstrap {
                    Attribute::BootstrapMethods(bootstrap_methods) => bootstrap_methods,
                    _ => unreachable!(),
                };

                let bootstrap_method = &bootstrap_methods.bootstrap_methods[dynamic.bootstrap_method_attr as usize];
                let mh = &bootstrap_method.method_ref;
                let r = &mh.reference;

                match mh.reference_kind {
                    ReferenceKind::GetField
                    | ReferenceKind::GetStatic
                    | ReferenceKind::PutField
                    | ReferenceKind::PutStatic => {
                        //Field resolution
                        todo!()
                    }
                    ReferenceKind::InvokeVirtual
                    | ReferenceKind::InvokeStatic
                    | ReferenceKind::InvokeSpecial
                    | ReferenceKind::NewInvokeSpecial => {

                    }
                    ReferenceKind::InvokeInterface => {
                        //Interface method resolution
                        todo!()
                    }
                }
            }
            Bytecode::Return => {
                return ThreadStepResult::Return(None);
            }
            Bytecode::Ireturn => {
                let [int] = frame.pop();

                return ThreadStepResult::Return(Some(JValue::Int(int as Int)));
            }
            Bytecode::Bipush(value) => {
                frame.push([*value as i32 as u64])
            },
            Bytecode::Aload_n(index) | Bytecode::Aload(index) => {
                frame.push([frame.locals[*index as usize]]);
            }
            Bytecode::Areturn => {
                let [ptr] = frame.pop();

                return ThreadStepResult::Return(Some(JValue::Reference(unsafe {
                    Object::from_raw(ptr as *mut RawObject<()>)
                })));
            }
            Bytecode::Putfield(constant_index) => {
                let constant = class.constant_pool.constants.get(constant_index).unwrap();

                let field_ref = constant.as_ref().unwrap();

                let classpath = &field_ref.class;
                let target_class = thread
                    .jvm
                    .find_class(classpath, thread.class_loader.clone())
                    .unwrap();

                let (field, field_class) = routine_resolve_field(&field_ref.name_and_type, &target_class).unwrap();

                if field_class.this_class != class.this_class {
                    Self::init_static(&field_class, frame_stack, thread);
                }

                let [ptr, value] = frame.pop();

                let object = unsafe { Object::from_raw(ptr as *mut RawObject<()>) };

                unsafe { thread.jvm.heap.set_class_field(&object, field, value as i64); }
            }
            Bytecode::Getfield(constant_index) => {
                let constant = class.constant_pool.constants.get(constant_index).unwrap();

                let field_ref = constant.as_ref().unwrap();

                let classpath = &field_ref.class;
                let target_class = thread
                    .jvm
                    .find_class(classpath, thread.class_loader.clone())
                    .unwrap();

                let (field, field_class) = routine_resolve_field(&field_ref.name_and_type, &target_class).unwrap();

                if field_class.this_class != class.this_class {
                    Self::init_static(&field_class, frame_stack, thread);
                }

                let ptr = frame.stack[*frame.stack_length - 1];

                let object = unsafe { Object::from_raw(ptr as *mut RawObject<()>) };

                let value = unsafe { thread.jvm.heap.get_class_field::<i64>(&object, field) as u64 };
                frame.stack[*frame.stack_length - 1] = value;
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
                    let idx = code
                        .instructions
                        .iter()
                        .find(|instr| instr.bytes_index == target_offset as u32)
                        .unwrap();
                    pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
                }
            }
            Bytecode::Istore(index)
            | Bytecode::Istore_n(index)
            | Bytecode::Astore(index)
            | Bytecode::Astore_n(index) => {
                frame.locals[*index as usize] = frame.stack[*frame.stack_length - 1];
                *frame.stack_length -= 1;
            }
            Bytecode::Iload(index) | Bytecode::Iload_n(index) => {
                frame.stack[*frame.stack_length] = frame.locals[*index as usize];
                *frame.stack_length += 1;
            }
            Bytecode::Sipush(short) => {
                frame.stack[*frame.stack_length] = *short as i32 as u64;
                *frame.stack_length += 1;
            }
            Bytecode::Iinc(index, inc) => {
                frame.locals[*index as usize] =
                    (frame.locals[*index as usize] as i64 + *inc as i64) as u64;
            }
            Bytecode::Goto(offset) => {
                let target_offset = bytes_index as isize + *offset as isize;
                let idx = code
                    .instructions
                    .iter()
                    .find(|instr| instr.bytes_index == target_offset as u32)
                    .unwrap();
                pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
            }
            Bytecode::Ldc(constant_index) => {
                let constant = class
                    .constant_pool
                    .constants
                    .get(&(*constant_index as u16))
                    .unwrap();

                match constant {
                    Constant::Integer(int) => {
                        frame.stack[*frame.stack_length] = *int as u64;
                        *frame.stack_length += 1;
                    }
                    Constant::Float(float) => {
                        frame.stack[*frame.stack_length] =
                            u32::from_ne_bytes(float.to_ne_bytes()) as u64;
                        *frame.stack_length += 1;
                    }
                    Constant::String(string) => {
                        let string_object = thread.jvm.heap.get_string(&string, &thread.jvm);

                        Self::init_static(&string_object.class, frame_stack, thread);

                        frame.stack[*frame.stack_length] =
                            string_object.value.get_raw() as usize as u64;
                        *frame.stack_length += 1;
                    }
                    _ => unimplemented!(),
                }
            }
            Bytecode::Aastore => {
                let value = unsafe {
                    Object {
                        ptr: frame.stack[*frame.stack_length - 1],
                    }
                };
                let index = frame.stack[*frame.stack_length - 2];
                let array_ref = unsafe {
                    Object {
                        ptr: frame.stack[*frame.stack_length - 3],
                    }
                };

                if array_ref == Object::NULL {
                    return ThreadStepResult::Error(ThreadError::RuntimeException(
                        RuntimeException::NullPointer,
                    ));
                }

                *frame.stack_length -= 3;

                let array = unsafe { array_ref.cast_array::<Object>() };
                unsafe {
                    (*array).body.body[index as usize] = value;
                }
            }
            Bytecode::Aaload => {
                let index = frame.stack[*frame.stack_length - 1];
                let array_ref = unsafe {
                    Object {
                        ptr: frame.stack[*frame.stack_length - 2],
                    }
                };
                println!("{}", array_ref.ptr);

                if array_ref == Object::NULL {
                    return ThreadStepResult::Error(ThreadError::RuntimeException(
                        RuntimeException::NullPointer,
                    ));
                }

                *frame.stack_length -= 1;

                let array = unsafe { array_ref.cast_array::<Object>() };

                unsafe {
                    println!("{}", (*array).body.length);
                    frame.stack[*frame.stack_length - 1] = (*array).body.body[index as usize].ptr;
                }
            }
            Bytecode::Anewarray(constant_index) => {
                let count = frame.stack[*frame.stack_length - 1] as i32;
                let constant = class.constant_pool.constants.get(constant_index).unwrap();
                let classpath = constant.as_string().unwrap();

                let array_class = thread
                    .jvm
                    .find_class(&classpath, thread.class_loader.clone())
                    .unwrap();
                let object = unsafe {
                    Object::from_raw(
                        thread
                            .jvm
                            .heap
                            .allocate_raw_object_array(&array_class, count),
                    )
                };

                frame.stack[*frame.stack_length] = object.ptr;
                *frame.stack_length += 1;
            }
            Bytecode::Putstatic(constant_index) => {
                let constant = class.constant_pool.constants.get(constant_index).unwrap();

                let field_ref = constant.as_ref().unwrap();

                let classpath = &field_ref.class;
                let target_class = thread
                    .jvm
                    .find_class(classpath, thread.class_loader.clone())
                    .unwrap();

                let (field, field_class) = routine_resolve_field(&field_ref.name_and_type, &target_class).unwrap();

                if field_class.this_class != class.this_class {
                    Self::init_static(&field_class, frame_stack, thread);
                }

                let value = frame.stack[*frame.stack_length - 1];
                *frame.stack_length -= 1;
                let index = field_class
                    .fields
                    .iter()
                    .filter(|field| field.access_flags.contains(AccessFlags::STATIC))
                    .position(|field| field.name == field_ref.name_and_type.name)
                    .unwrap();

                unsafe {
                    *(field_class.statics as *mut u64).add(index) = value;
                }
            }
            Bytecode::Getstatic(constant_index) => {
                let constant = class.constant_pool.constants.get(constant_index).unwrap();

                let field_ref = constant.as_ref().unwrap();
                let classpath = &field_ref.class;
                let target_class = thread
                    .jvm
                    .find_class(classpath, thread.class_loader.clone())
                    .unwrap();

                if target_class.this_class != class.this_class {
                    Self::init_static(&target_class, frame_stack, thread);
                }

                let index = target_class
                    .fields
                    .iter()
                    .filter(|field| field.access_flags.contains(AccessFlags::STATIC))
                    .position(|field| field.name == field_ref.name_and_type.name)
                    .unwrap();

                unsafe {
                    frame.stack[*frame.stack_length] =
                        *(target_class.statics as *mut u64).add(index);
                    *frame.stack_length += 1;
                }
            }
            Bytecode::New(constant_index) => {
                let constant = class.constant_pool.constants.get(constant_index).unwrap();

                if let Constant::Class(classpath) = constant {
                    let target_class = thread
                        .jvm
                        .find_class(classpath, thread.class_loader.clone())
                        .unwrap();

                    if target_class.this_class != class.this_class {
                        Self::init_static(&target_class, frame_stack, thread);
                    }

                    let object = thread.jvm.heap.allocate_class(&target_class);

                    frame.stack[*frame.stack_length] = object.ptr;
                    *frame.stack_length += 1;
                } else {
                    panic!()
                }
            }
            Bytecode::Dup => {
                frame.stack[*frame.stack_length] = frame.stack[*frame.stack_length - 1];
                *frame.stack_length += 1;
            }
            Bytecode::Aconst_null => {
                frame.stack[*frame.stack_length] = Object::NULL.ptr;
                *frame.stack_length += 1;
            }
            _ => unimplemented!("Bytecode {:?} unimplemented in interpreter", bytecode),
        }

        *frame.program_counter = ((*frame.program_counter as i32) + pc_inc) as u32;

        drop(frame);

        ThreadStepResult::Ok(raw_frame)
    }
}
