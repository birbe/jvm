use std::fmt::{Debug, Formatter};
use crate::bytecode::{Bytecode, JValue};
use crate::{JVM, routine_resolve_field};

use crate::classfile::resolved::{
    AccessFlags, Attribute, Class, Constant, FieldType, Method, MethodDescriptor, NameAndType, Ref,
    ReferenceKind, ReturnType,
};
use crate::env::native::heap::{AsJavaPrimitive, Byte, Int, RawObject};
use crate::linker::ClassLoader;
use bitflags::Flags;
use parking_lot::Mutex;

use std::ops::{Deref, DerefMut};
use std::pin::Pin;

use std::sync::atomic::Ordering;
use std::sync::Arc;
use crate::env::Object;
use crate::env::wasm::dealloc_ref;
use crate::execution::MethodHandle;

macro_rules! console_log {
    ($($t:tt)*) => (web_sys::console::log_1(&format_args!($($t)*).to_string().into()))
}

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
#[derive(Debug)]
pub struct RawFrame {
    pub(crate) locals_length: usize,
    pub(crate) locals: *mut Operand,
    pub(crate) method: *const Method,
    pub(crate) class: *const Class,
    pub(crate) program_counter: u32,
    pub(crate) stack_index: usize,
    pub(crate) stack_length: usize,
    pub(crate) stack: *mut Operand,
}

impl RawFrame {
    pub const UNINIT: Self = RawFrame {
        method: std::ptr::null(),
        class: std::ptr::null(),
        program_counter: 0,
        locals_length: 0,
        locals: std::ptr::null_mut(),
        stack_index: 0,
        stack_length: 0,
        stack: std::ptr::null_mut(),
    };

    pub fn new(
        method: &Method,
        class: &Class,
        locals: Box<[Operand]>,
        stack: Box<[Operand]>,
    ) -> Self {
        let locals = Box::into_raw(locals);
        let stack = Box::into_raw(stack);

        Self {
            method: method as *const Method,
            class: class as *const Class,
            program_counter: 0,
            locals_length: locals.len(),
            locals: locals.as_mut_ptr(),
            stack_index: 0,
            stack_length: stack.len(),
            stack: stack.as_mut_ptr(),
        }
    }

    pub fn as_frame<'a>(&'a mut self) -> Frame {
        unsafe {
            Frame {
                method: &*self.method,
                class: &*self.class,
                program_counter: &mut self.program_counter,
                locals: std::slice::from_raw_parts_mut::<'a>(self.locals, self.locals_length),
                stack_length: &mut self.stack_index,
                stack: std::slice::from_raw_parts_mut::<'a>(self.stack, self.stack_length),
            }
        }
    }
}

impl Drop for RawFrame {
    fn drop(&mut self) {
        unsafe {
            if !self.stack.is_null() {
                drop(Box::from_raw(std::slice::from_raw_parts_mut(
                    self.stack,
                    self.stack_length,
                )));
            }
            if !self.locals.is_null() {
                drop(Box::from_raw(std::slice::from_raw_parts_mut(
                    self.locals,
                    self.locals_length,
                )));
            }
        }
    }
}

#[derive(Copy, Clone)]
pub union Operand {
    pub objectref: *mut (),
    pub data: u64,
}

#[repr(C)]
pub struct Frame<'a> {
    pub method: &'a Method,
    pub class: &'a Class,
    pub program_counter: &'a mut u32,
    pub locals: &'a mut [Operand],
    pub stack_length: &'a mut usize,
    pub stack: &'a mut [Operand],
}

impl<'a> Frame<'a> {
    pub fn pop<const N: usize>(&mut self) -> [Operand; N] {
        assert!(*self.stack_length >= N);

        let val: [Operand; N] = (&self.stack[*self.stack_length - N..*self.stack_length])
            .try_into()
            .unwrap();
        *self.stack_length -= N;
        val
    }

    pub fn pop_dynamic(&mut self, n: usize) -> Vec<Operand> {
        assert!(*self.stack_length >= n);

        let val = Vec::from(&self.stack[*self.stack_length - n..*self.stack_length]);
        *self.stack_length -= n;
        val
    }

    pub fn push<const N: usize>(&mut self, operands: [Operand; N]) {
        let len = *self.stack_length;
        (&mut self.stack[len..len + N]).copy_from_slice(&operands);
        *self.stack_length += N;
    }
}

impl<'a> Debug for Frame<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let locals: Vec<u64> = self.locals.iter().map(|op| unsafe { op.data }).collect();
        let stack: Vec<u64> = self.stack[..*self.stack_length].iter().map(|op| unsafe { op.data }).collect();

        f.debug_struct("Frame")
            .field("method", &self.method.get_identifier())
            .field("class", &self.class.this_class)
            .field("program_counter", self.program_counter)
            .field("locals", &locals)
            .field("stack", &stack)
            .finish()
    }
}

///Stack of frames
#[derive(Debug)]
#[repr(C)]
pub struct FrameStack {
    pub frames: Pin<Box<[RawFrame; 1024]>>,
    pub frame_index: isize,
}

pub struct FrameStackView<'a> {
    pub frames: &'a mut [RawFrame],
}

impl<'jvm> FrameStack {
    pub fn new() -> Self {
        FrameStack {
            frames: Pin::new(Box::new([RawFrame::UNINIT; 1024])),
            frame_index: -1,
        }
    }

    pub fn push(&mut self, frame: RawFrame) {
        self.frame_index += 1;
        self.frames[self.frame_index as usize] = frame;
    }

    pub fn pop(&mut self) -> RawFrame {
        assert!(self.frame_index > -1);
        let frame = std::mem::replace(
            &mut self.frames[self.frame_index as usize],
            RawFrame::UNINIT,
        );
        self.frame_index -= 1;

        frame
    }

    pub fn get_top(&mut self) -> *mut RawFrame {
        &mut self.frames[self.frame_index as usize] as *mut RawFrame
    }
}

pub enum ThreadStepResult {
    Ok(RawFrame),
    Error(ThreadError),
    Return(Option<JValue>),
}

#[repr(C)]
pub struct Thread {
    pub frame_stack: Box<FrameStack>,
    pub jvm: Arc<JVM>,
    pub class_loader: Arc<dyn ClassLoader>,
    pub killed: bool,
}

impl Thread {

    fn invoke(&mut self, method_handle: &MethodHandle, args: Box<[Operand]>) -> Option<Operand> {
        let jvm = self.jvm.clone();
        jvm.environment.invoke_handle(self, method_handle, args)
    }

}

#[derive(Debug)]
pub enum JavaBarrierError {
    ClassNotFound,
    MethodNotFound,
}

pub struct ThreadHandle {
    thread: *mut Thread,
    guard: Arc<Mutex<Thread>>,
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
    pub unsafe fn new(thread: *mut Thread, guard: Arc<Mutex<Thread>>) -> Self {
        Self { thread, guard }
    }

    pub fn call(
        &mut self,
        classpath: &str,
        method_name: &str,
        method_descriptor: &str,
        args: &[JValue],
    ) -> Result<Option<JValue>, JavaBarrierError> {
        let ref_ = Ref {
            class: Arc::new(classpath.into()),
            name_and_type: Arc::new(NameAndType { name: Arc::new(method_name.into()), descriptor: Arc::new(method_descriptor.into()) }),
        };

        let method_handle = self.class_loader.get_method_handle(&ref_);
        let method = &*method_handle.method;

        let operands: Vec<Operand> = args.iter().map(|arg| arg.as_operand()).collect();

        let operand = self.invoke(&method_handle, operands.into_boxed_slice());

        let jvalue = match (&method.descriptor.return_type, operand) {
            (ReturnType::FieldType(field_type), Some(return_value)) => Some(match field_type {
                FieldType::Array { .. } => {
                    JValue::Reference(unsafe {
                        self.jvm.environment.object_from_operand(&return_value)
                    })
                },
                FieldType::Byte => JValue::Byte(unsafe { return_value.data } as Byte),
                FieldType::Char => todo!(),
                FieldType::Double => {
                    JValue::Double(f64::from_ne_bytes(unsafe { return_value.data }.to_ne_bytes()))
                }
                FieldType::Float => {
                    JValue::Float(f32::from_ne_bytes((unsafe { return_value.data } as u32).to_ne_bytes()))
                }
                FieldType::Int => JValue::Int(unsafe { return_value.data } as i32),
                FieldType::Long => JValue::Long(unsafe { return_value.data } as i64),
                FieldType::Short => JValue::Short(unsafe { return_value.data } as i16),
                FieldType::Boolean => JValue::Int(unsafe { return_value.data } as i32),
                FieldType::Class(_) => JValue::Reference(unsafe {
                    self.jvm.environment.object_from_operand(&return_value)
                }),
            }),
            (_, _) => None,
        };

        Ok(jvalue)
    }

    #[no_mangle]
    pub extern "C" fn interpret(thread: &mut Thread) -> Operand {
        let mut raw_frame = thread.frame_stack.pop();

        loop {
            match Self::step(raw_frame, thread) {
                ThreadStepResult::Ok(frame) => raw_frame = frame,
                ThreadStepResult::Error(error) => panic!("{:?}", error),
                ThreadStepResult::Return(value) => {
                    return value.unwrap_or(JValue::Long(0)).as_operand()
                }
            }
        }
    }

    fn init_static(class: &Class, thread: &mut Thread) {
        match &class.super_class {
            None => {}
            Some(superclass) => Self::init_static(superclass, thread),
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
                let method_handle = thread.class_loader.get_method_by_name(&class.this_class, "<clinit>", "()V");

                thread.invoke(&method_handle, Box::new([]));

                class.static_init_state.store(2, Ordering::Release);
            }
            //Another thread is initializing, so wait until that's not the case
            Err(_) => while class.static_init_state.load(Ordering::Acquire) == 1 {},
        }
    }

    fn step(
        mut raw_frame: RawFrame,
        thread: &mut Thread,
    ) -> ThreadStepResult {
        let mut frame = raw_frame.as_frame();

        let class = frame.class;
        let method = frame.method;

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

        console_log!(
            "{:<25} | {:<10} | {:<50} | {:<10}",
            format!("{bytecode:?}"),
            frame.program_counter,
            method.name,
            class.this_class
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

                let method_handle = thread.class_loader.get_method_handle(ref_);
                let method_descriptor = &method_handle.method.descriptor;

                let args = &frame.stack[*frame.stack_length - method_descriptor.args.len()..];

                let operand = thread.invoke(&method_handle, Vec::from(args).into_boxed_slice());

                *frame.stack_length -= method_descriptor.args.len();

                if let Some(return_value) = operand {
                    frame.push([return_value]);
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

                Self::init_static(c, thread);

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

                panic!("invokespecial");

                let method_handle = thread.class_loader.get_method_handle(method_ref);

                thread.invoke(&method_handle, Vec::from(args).into_boxed_slice());
            }
            Bytecode::Invokevirtual(constant_index) => {
                let [objectref] = frame.pop();
                let objectref = unsafe { thread.jvm.environment.object_from_operand(&objectref) };

                let object_class = thread.jvm.environment.get_object_class(&objectref);

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

                let m_invoke = if !method.is_signature_polymorphic(classpath) {
                    println!("not signature polymorphic");

                    if method.access_flags.contains(AccessFlags::PRIVATE) {
                        method
                    } else {
                        let mut search = &class.super_class;

                        loop {
                            match search {
                                None => break todo!(),
                                Some(next_class) => {
                                    if let Some(overrider) =
                                        next_class.find_overriding_method(method)
                                    {
                                        break overrider;
                                    } else {
                                        search = &next_class.super_class;
                                    }
                                }
                            }
                        }
                    }
                } else {
                    todo!()
                };

                let nargs = frame.pop_dynamic(m_invoke.descriptor.args.len());

                if m_invoke.access_flags.contains(AccessFlags::SYNCHRONIZED) {
                    todo!()
                }

                todo!()
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

                let bootstrap_method =
                    &bootstrap_methods.bootstrap_methods[dynamic.bootstrap_method_attr as usize];
                let mh = &bootstrap_method.method_ref;
                let _r = &mh.reference;

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
                    | ReferenceKind::NewInvokeSpecial => {}
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

                return ThreadStepResult::Return(Some(JValue::Int(unsafe { int.data } as Int)));
            }
            Bytecode::Bipush(value) => frame.push([Operand {
                data: *value as u64,
            }]),
            Bytecode::Aload_n(index) | Bytecode::Aload(index) => {
                frame.push([frame.locals[*index as usize]]);
            }
            Bytecode::Areturn => {
                let [ptr] = frame.pop();

                return ThreadStepResult::Return(Some(JValue::Reference(unsafe {
                    thread.jvm.environment.object_from_operand(&ptr)
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

                let (field, field_class) =
                    routine_resolve_field(&field_ref.name_and_type, &target_class).unwrap();

                if field_class.this_class != class.this_class {
                    Self::init_static(&field_class, thread);
                }

                let [object, value] = frame.pop();

                let object = unsafe { thread.jvm.environment.object_from_operand(&object) };

                thread.jvm.environment.set_object_field(object, &target_class, field_ref, value);
            }
            Bytecode::Getfield(constant_index) => {
                let constant = class.constant_pool.constants.get(constant_index).unwrap();

                todo!()

                // thread.jvm.environment.get_object_field()

                // let field_ref = constant.as_ref().unwrap();
                //
                // let classpath = &field_ref.class;
                // let target_class = thread
                //     .jvm
                //     .find_class(classpath, thread.class_loader.clone())
                //     .unwrap();
                //
                // let (field, field_class) = routine_resolve_field(&field_ref.name_and_type, &target_class).unwrap();
                //
                // if field_class.this_class != class.this_class {
                //     Self::init_static(&field_class, frame_stack, thread);
                // }
                //
                // let [ptr] = frame.pop();
                //
                // let object = unsafe { Object::from_raw(ptr.objectref as *mut RawObject<()>) };
                //
                // let value = unsafe { thread.jvm.heap.get_class_field::<Operand>(&object, field) };
                // frame.push([value]);
            }
            Bytecode::Iconst_n_m1(value) => {
                frame.push([Operand {
                    data: *value as u64,
                }]);
            }
            Bytecode::If_icmpge(offset) => {
                let a = unsafe { frame.stack[*frame.stack_length - 2].data } as i32;
                let b = unsafe { frame.stack[*frame.stack_length - 1].data } as i32;
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
                frame.locals[*index as usize] = frame.pop::<1>()[0];
            }
            Bytecode::Iload(index) | Bytecode::Iload_n(index) => {
                frame.push([frame.locals[*index as usize]]);
            }
            Bytecode::Iadd => {
                let [a, b] = frame.pop();
                frame.push([
                    unsafe {
                        Operand {
                            data: ((a.data as i32) + (b.data as i32)) as u64
                        }
                    }
                ]);
            }
            Bytecode::Sipush(short) => {
                frame.push([Operand {
                    data: *short as u64,
                }]);
            }
            Bytecode::Iinc(index, inc) => {
                let int = unsafe { frame.locals[*index as usize].data } as i32;
                frame.locals[*index as usize] = Operand {
                    data: (int + (*inc as i32)) as u64,
                };
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
                        frame.push([Operand { data: *int as u64 }]);
                    }
                    Constant::Float(float) => {
                        frame.push([Operand {
                            data: u32::from_ne_bytes(float.to_ne_bytes()) as u64,
                        }]);
                    }
                    Constant::String(string) => {
                        todo!()

                        // Self::init_static(&string_object.class, thread);
                        //
                        // frame.push([Operand {
                        //     objectref: string_object.value.ptr,
                        // }]);
                    }
                    _ => unimplemented!(),
                }
            }
            Bytecode::Aastore => {
                let [arrayref, index, value] = frame.pop();

                if unsafe { arrayref.objectref }.is_null() {
                    return ThreadStepResult::Error(ThreadError::RuntimeException(
                        RuntimeException::NullPointer,
                    ));
                }

                let array = unsafe { thread.jvm.environment.object_from_operand(&arrayref) };
                let index = unsafe { index.data as i32 };

                thread.jvm.environment.set_object_array_element(array, index, value);
            }
            Bytecode::Aaload => {
                let [arrayref, index] = frame.pop();

                if unsafe { arrayref.objectref }.is_null() {
                    return ThreadStepResult::Error(ThreadError::RuntimeException(
                        RuntimeException::NullPointer,
                    ));
                }

                let index = unsafe { index.data as i32 };

                let array_object = unsafe { thread.jvm.environment.object_from_operand(&arrayref) };
                let element = thread.jvm.environment.get_array_element(array_object, index);

                unsafe {
                    frame.push([element]);
                }
            }
            Bytecode::Anewarray(constant_index) => {
                let [count] = frame.pop();
                let count = unsafe { count.data as i32 };

                let constant = class.constant_pool.constants.get(constant_index).unwrap();
                let classpath = constant.as_string().unwrap();

                let array_class = thread
                    .jvm
                    .find_class(&classpath, thread.class_loader.clone())
                    .unwrap();

                let object = thread.jvm.environment.allocate_object_array(&array_class, count);

                frame.push([Operand {
                    objectref: object.ptr,
                }]);

                std::mem::forget(object);
            }
            Bytecode::Putstatic(constant_index) => {
                let constant = class.constant_pool.constants.get(constant_index).unwrap();

                let field_ref = constant.as_ref().unwrap();

                let classpath = &field_ref.class;
                let target_class = thread
                    .jvm
                    .find_class(classpath, thread.class_loader.clone())
                    .unwrap();

                let (_field, field_class) =
                    routine_resolve_field(&field_ref.name_and_type, &target_class).unwrap();

                if field_class.this_class != class.this_class {
                    Self::init_static(&field_class, thread);
                }

                let [value] = frame.pop();

                let index = field_class
                    .fields
                    .iter()
                    .filter(|field| field.access_flags.contains(AccessFlags::STATIC))
                    .position(|field| field.name == field_ref.name_and_type.name)
                    .unwrap();

                unsafe {
                    *(field_class.statics as *mut Operand).add(index) = value;
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
                    Self::init_static(&target_class, thread);
                }

                let index = target_class
                    .fields
                    .iter()
                    .filter(|field| field.access_flags.contains(AccessFlags::STATIC))
                    .position(|field| field.name == field_ref.name_and_type.name)
                    .unwrap();

                unsafe {
                    frame.push([*(target_class.statics as *mut Operand).add(index)]);
                }
            }
            Bytecode::New(constant_index) => {
                let constant = class.constant_pool.constants.get(constant_index).unwrap();

                if let Constant::Class { path, .. } = constant {
                    let target_class = thread
                        .jvm
                        .find_class(path, thread.class_loader.clone())
                        .unwrap();

                    if target_class.this_class != class.this_class {
                        Self::init_static(&target_class, thread);
                    }

                    let object = thread.jvm.environment.new_object(&target_class);

                    frame.push([Operand {
                        objectref: object.ptr,
                    }]);

                    std::mem::forget(object);
                } else {
                    panic!()
                }
            }
            Bytecode::Dup => {
                frame.stack[*frame.stack_length] = frame.stack[*frame.stack_length - 1];
                *frame.stack_length += 1;
            }
            Bytecode::Aconst_null => {
                frame.push([Operand {
                    objectref: std::ptr::null_mut(),
                }]);
            }
            _ => unimplemented!("Bytecode {:?} unimplemented in interpreter", bytecode),
        }

        *frame.program_counter = ((*frame.program_counter as i32) + pc_inc) as u32;

        drop(frame);

        ThreadStepResult::Ok(raw_frame)
    }
}
