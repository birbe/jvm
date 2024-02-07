use std::arch::wasm32::unreachable;
use std::fmt::{Debug, Formatter};
use crate::bytecode::{Bytecode, JValue};
use crate::{Byte, Int, JVM, Long, routine_resolve_field, routine_resolve_method};

use crate::classfile::resolved::{
    AccessFlags, Attribute, Class, Constant, FieldType, Method, MethodDescriptor, NameAndType, Ref,
    ReferenceKind, ReturnType,
};
use crate::linker::ClassLoader;
use bitflags::Flags;
use parking_lot::Mutex;

use std::ops::{Deref, DerefMut};
use std::pin::Pin;

use std::sync::atomic::Ordering;
use std::sync::Arc;
use crate::env::Object;
use crate::execution::MethodHandle;

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
#[repr(C)]
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

    pub fn push_dynamic(&mut self, operands: impl IntoIterator<Item = Operand>) {
        for operand in operands {
            self.push([operand]);
        }
    }

}

impl<'a> Debug for Frame<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Frame")
            .field("method", &self.method.get_identifier())
            .field("class", &self.class.this_class)
            .field("program_counter", self.program_counter)
            // .field("locals", &)
            // .field("stack", &stack)
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
    Ok,
    Error(ThreadError),
    Return(Option<JValue>),
}

#[repr(C)]
pub struct Thread {
    pub frame_stack: Box<FrameStack>,
    pub jvm: Arc<JVM>,
    pub class_loader: Arc<dyn ClassLoader>,
    pub id: u32,
    pub killed: bool,
}

impl Thread {

    pub(crate) fn invoke(&mut self, method_handle: &MethodHandle, args: Box<[Operand]>) -> Option<Operand> {
        let jvm = self.jvm.clone();
        jvm.environment.invoke_handle(self, method_handle, args)
    }

    pub fn get_id(&self) -> u32 {
        self.id
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
        let method_handle = self.class_loader.get_method_by_name(classpath, method_name, method_descriptor);
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
        // for index in 0..thread.frame_stack.frame_index {
        //     let frame = &mut thread.frame_stack.frames[index as usize];
        //     let frame = frame.as_frame();
        //     console_log!("{}", frame.class.this_class);
        // }

        let raw_frame = thread.frame_stack.get_top();
        let raw_frame = unsafe { &mut *raw_frame };

        loop {
            match Self::step(raw_frame, thread) {
                ThreadStepResult::Ok => {},
                ThreadStepResult::Error(error) => panic!("{:?}", error),
                ThreadStepResult::Return(value) => {
                    thread.frame_stack.pop();
                    return value.unwrap_or(JValue::Long(0)).as_operand()
                }
            }
        }
    }

    pub fn init_static(class: &Class, thread: &mut Thread) {
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
            .compare_exchange(0, thread.get_id() + 10, Ordering::SeqCst, Ordering::SeqCst)
        {
            //This thread has to call <clinit>
            Ok(_) => {
                console_log!("Initializing {}", class.this_class);

                let method_handle = thread.class_loader.get_method_by_name(&class.this_class, "<clinit>", "()V");

                thread.invoke(&method_handle, Box::new([]));

                class.static_init_state.store(2, Ordering::Release);
            }
            //Another thread is initializing, so wait until that's not the case
            Err(_) => while {
                let state = class.static_init_state.load(Ordering::Acquire);
                if state != 2 && state != (thread.get_id() + 10) { true } else { false }
            } {},
        }
    }

    fn step(
        mut raw_frame: &mut RawFrame,
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

        let locals: Vec<u32> = frame.locals.iter().map(|local| unsafe { local.objectref as u32 }).collect();
        let stack: Vec<u32> = (&frame.stack[..*frame.stack_length]).iter().map(|local| unsafe { local.objectref as u32 }).collect();

        if &*class.this_class != "java/lang/Class" {
            let mut stdout = thread.jvm.stdout.lock();
            write!(&mut stdout, "{}",
                   format!(
                       "{:<20} | {:<30} | {:<15} | {:<6} | {:?} | {:?}",
                       format!("{:?}", bytecode),
                       class.this_class,
                       method.name,
                       pc,
                       locals,
                       stack
                   )
            ).unwrap();
        }

        let jvm = thread.jvm.clone();
        let class_loader = thread.class_loader.clone();

        match bytecode {
            Bytecode::Invokestatic(constant_index) => {
                let ref_ = class
                    .constant_pool
                    .constants
                    .get(constant_index)
                    .unwrap()
                    .as_ref()
                    .unwrap();

                let class = jvm.find_class(&ref_.class, &*class_loader, thread).unwrap();

                if frame.class.get_id() != class.get_id() {
                    Self::init_static(&class, thread);
                }

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
                let target_class = jvm
                    .find_class(classpath, &*class_loader, thread)
                    .unwrap();

                let method = target_class.get_method(&method_ref.name_and_type).unwrap();

                let c = if {
                    !method.is_init(classpath, &*class_loader, thread)
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

                let args = frame.pop_dynamic(method_descriptor.args.len() + 1);

                if method_descriptor.return_type != ReturnType::Void {
                    panic!()
                }

                let method_handle = thread.class_loader.get_method_handle(method_ref);

                thread.invoke(&method_handle, Vec::from(args).into_boxed_slice());
            }
            Bytecode::Pop => {
                frame.pop::<1>();
            }
            Bytecode::Pop2 => {
                frame.pop::<2>();
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

                let method_ref_class = jvm
                    .find_class(classpath, &*class_loader, thread)
                    .unwrap();

                let resolved_method = routine_resolve_method(&thread, &method_ref, &method_ref_class).unwrap();

                let mut nargs = frame.pop_dynamic(resolved_method.descriptor.args.len());

                let [objectref] = frame.pop();

                nargs.insert(0, objectref);

                let objectref = unsafe { thread.jvm.environment.object_from_operand(&objectref) };

                let object_class = thread.jvm.environment.get_object_class(&objectref);

                if !resolved_method.is_signature_polymorphic(classpath) {
                    if resolved_method.access_flags.contains(AccessFlags::PRIVATE) {
                        if resolved_method.access_flags.contains(AccessFlags::SYNCHRONIZED) {
                            todo!()
                        }

                        let method_handle = thread.class_loader.get_method_by_name(
                            &object_class.this_class,
                            &resolved_method.name,
                            &resolved_method.descriptor.string
                        );

                        let out = thread.invoke(&method_handle, nargs.into_boxed_slice());

                        frame.push_dynamic(out);
                    } else {
                        let parents = object_class.parents();

                        let (resolved_class, resolved_method) = [object_class].into_iter().chain(parents.iter().map(|parent| &**parent)).find_map(|parent| {
                            parent.find_overriding_method(resolved_method, &method_ref_class).map(|method| (parent, method))
                        }).expect("Maximally specific resolution not yet implemented");

                        let method_handle = thread.class_loader.get_method_by_name(
                            &resolved_class.this_class,
                            &resolved_method.name,
                            &resolved_method.descriptor.string
                        );

                        let out = thread.invoke(&method_handle, nargs.into_boxed_slice());

                        frame.push_dynamic(out);
                    }
                } else {
                    todo!()
                };
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
            Bytecode::Lreturn => {
                let [long] = frame.pop();

                return ThreadStepResult::Return(Some(JValue::Long(unsafe { long.data } as Long)));
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
                let target_class = jvm
                    .find_class(classpath, &*class_loader, thread)
                    .unwrap();

                let (field, field_class) =
                    routine_resolve_field(&field_ref.name_and_type, &target_class).unwrap();

                if field_class.this_class != class.this_class {
                    Self::init_static(&field_class, thread);
                }

                let [object, value] = frame.pop();
                let object = unsafe { thread.jvm.environment.object_from_operand(&object) };

                unsafe {
                    thread.jvm.environment.set_object_field(&object, &target_class, &field_ref.name_and_type.name, &field_ref.name_and_type.descriptor, value);
                }
            }
            Bytecode::Getfield(constant_index) => {
                let constant = class.constant_pool.constants.get(constant_index).unwrap();

                let field_ref = constant.as_ref().unwrap();

                let classpath = &field_ref.class;
                let target_class = jvm
                    .find_class(classpath, &*class_loader, thread)
                    .unwrap();

                let (field, field_class) = routine_resolve_field(&field_ref.name_and_type, &target_class).unwrap();

                // if field_class.this_class != class.this_class {
                //     Self::init_static(&field_class, frame, thread);
                // }

                let [ptr] = frame.pop();

                let object = unsafe { thread.jvm.environment.object_from_operand(&ptr) };
                let operand = unsafe { thread.jvm.environment.get_object_field(&object, &field_class, &field_ref.name_and_type.name, &field_ref.name_and_type.descriptor) };

                frame.push([operand]);
            }
            Bytecode::Iconst_n_m1(value) => {
                frame.push([Operand {
                    data: *value as u64,
                }]);
            }
            Bytecode::Lconst_n(value) => {
                frame.push([Operand {
                    data: *value as u64,
                }]);
            }
            Bytecode::Fconst_n(index) => {
                frame.push([Operand {
                    data: u32::from_ne_bytes(match index {
                        0 => 0.0f32,
                        1 => 1.0f32,
                        2 => 2.0f32,
                        _ => unreachable!()
                    }.to_be_bytes()) as u64,
                }]);
            }
            Bytecode::Fcmpg | Bytecode::Fcmpl => {
                let [a, b] = frame.pop();

                let a = unsafe { f32::from_ne_bytes((a.data as u32).to_ne_bytes()) };
                let b = unsafe { f32::from_ne_bytes((b.data as u32).to_ne_bytes()) };

                let nan_out = if matches!(bytecode, Bytecode::Fcmpg) { 1 } else { -1i32 as u64 };

                frame.push([Operand {
                    data: if a > b { 1 } else if a == b { 0 } else if a < b { -1i32 as u64 } else { nan_out }
                }]);
            }
            Bytecode::If_icmpge(offset) => {
                let [a, b] = frame.pop();

                let a = unsafe { a.data } as i32;
                let b = unsafe { b.data } as i32;

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
            Bytecode::Ifge(offset) => {
                let [value] = frame.pop();

                let value = unsafe { value.data } as i32;

                if value >= 0 {
                    let target_offset = bytes_index as isize + *offset as isize;
                    let idx = code
                        .instructions
                        .iter()
                        .find(|instr| instr.bytes_index == target_offset as u32)
                        .unwrap();
                    pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
                }
            }
            Bytecode::Ifle(offset) => {
                let [value] = frame.pop();

                let value = unsafe { value.data } as i32;

                if value <= 0 {
                    let target_offset = bytes_index as isize + *offset as isize;
                    let idx = code
                        .instructions
                        .iter()
                        .find(|instr| instr.bytes_index == target_offset as u32)
                        .unwrap();
                    pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
                }
            }
            Bytecode::Ifnonnull(offset) => {
                let [reference] = frame.pop::<1>();

                if unsafe { !reference.objectref.is_null() } {
                    let target_offset = bytes_index as isize + *offset as isize;
                    let idx = code
                        .instructions
                        .iter()
                        .find(|instr| instr.bytes_index == target_offset as u32)
                        .unwrap();
                    pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
                }
            }
            Bytecode::Ifnull(offset) => {
                let [reference] = frame.pop::<1>();

                if unsafe { reference.objectref.is_null() } {
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
            | Bytecode::Astore_n(index)
            | Bytecode::Lstore_n(index) => {
                frame.locals[*index as usize] = frame.pop::<1>()[0];
            }
            Bytecode::Lload(index) | Bytecode::Lload_n(index) | Bytecode::Iload(index) | Bytecode::Iload_n(index) | Bytecode::Fload_n(index) | Bytecode::Fload(index) => {
                frame.push([frame.locals[*index as usize]]);
            }
            Bytecode::I2l => {
                let [int] = frame.pop::<1>();
                frame.push([
                    Operand {
                        data: unsafe { int.data as i64 as u64 }
                    }
                ]);
            }
            Bytecode::Ladd => {
                let [a, b] = frame.pop();
                frame.push([
                    unsafe {
                        Operand {
                            data: ((a.data as i64) + (b.data as i64)) as u64
                        }
                    }
                ]);
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
            Bytecode::Isub => {
                let [a, b] = frame.pop();
                frame.push([
                    unsafe {
                        Operand {
                            data: ((a.data as i32) - (b.data as i32)) as u64
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
                        let string = jvm.environment.new_string(&string, &*class_loader, thread);

                        frame.push([
                            Operand {
                                objectref: string.ptr
                            }
                        ]);
                    }
                    Constant::Class {
                        path, depth
                    } => {
                        let class = class_loader.get_class(path).unwrap();
                        let objects = jvm.class_objects.read();
                        let object = objects.get(&class.get_id()).unwrap();

                        frame.push([
                            Operand {
                                objectref: object.ptr
                            }
                        ]);
                    }
                    _ => unimplemented!("Ldc {constant:?} unimplemented"),
                }
            }
            Bytecode::Ldc_w(constant_index) => {
                let constant = class
                    .constant_pool
                    .constants
                    .get(&(*constant_index))
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
                        let string_class = jvm.find_class("java/lang/String", &*class_loader, thread).unwrap();

                        if &*class.this_class != "java/lang/String" {
                            Self::init_static(&string_class, thread);
                        }

                        let string_object = jvm.environment.new_object(&string_class);

                        frame.push([Operand {
                            objectref: string_object.ptr,
                        }]);

                        std::mem::forget(string_object);
                    }
                    Constant::Class {
                        path, depth
                    } => {
                        let class = class_loader.get_class(path).unwrap();
                        let objects = jvm.class_objects.read();
                        let object = objects.get(&class.get_id()).unwrap();

                        frame.push([
                            Operand {
                                objectref: object.ptr
                            }
                        ]);
                    }
                    _ => unimplemented!("Ldc_w {constant:?} unimplemented"),
                }
            }
            Bytecode::Iastore => {
                let [arrayref, index, value] = frame.pop();

                if unsafe { arrayref.objectref }.is_null() {
                    return ThreadStepResult::Error(ThreadError::RuntimeException(
                        RuntimeException::NullPointer,
                    ));
                }

                let array = unsafe { thread.jvm.environment.object_from_operand(&arrayref) };
                let index = unsafe { index.data as i32 };

                thread.jvm.environment.set_array_element(10, &array, index, value);
            }
            Bytecode::Castore => {
                let [arrayref, index, value] = frame.pop();

                if unsafe { arrayref.objectref }.is_null() {
                    return ThreadStepResult::Error(ThreadError::RuntimeException(
                        RuntimeException::NullPointer,
                    ));
                }

                let array = unsafe { thread.jvm.environment.object_from_operand(&arrayref) };
                let index = unsafe { index.data as i32 };

                thread.jvm.environment.set_array_element(5, &array, index, value);
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

                thread.jvm.environment.set_object_array_element(&array, index, value);
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
                let element = thread.jvm.environment.get_array_element(&array_object, index);

                unsafe {
                    frame.push([element]);
                }
            }
            Bytecode::Newarray(type_) => {
                let [count] = frame.pop();
                let count = unsafe { count.data as i32 };

                let object = jvm.environment.new_array(*type_ as i32, count);

                frame.push([Operand {
                    objectref: object.ptr,
                }]);

                std::mem::forget(object);
            }
            Bytecode::Checkcast(constant_index) => {
                let [objectref] = frame.pop();

                let constant = class.constant_pool.constants.get(constant_index).unwrap();

                let classpath = if let Constant::Class { path, ..} = constant {
                    path
                } else {
                    panic!()
                };

                // let t_class = class_loader.get_class();

                if unsafe { objectref.objectref } != std::ptr::null_mut() {
                    let object = unsafe { jvm.environment.object_from_operand(&objectref) };
                    let class = jvm.environment.get_object_class(&object);
                }

                frame.push([objectref]);
            }
            Bytecode::Arraylength => {
                let [objectref] = frame.pop();
                let object = unsafe { jvm.environment.object_from_operand(&objectref) };

                frame.push([
                    Operand {
                        data: jvm.environment.get_array_length(&object) as u64
                    }
                ]);
            }
            Bytecode::Anewarray(constant_index) => {
                let [count] = frame.pop();
                let count = unsafe { count.data as i32 };

                let constant = class.constant_pool.constants.get(constant_index).unwrap();
                if let Constant::Class { path, .. } = constant {
                    let array_class = jvm
                        .find_class(&path, &*class_loader, thread)
                        .unwrap();

                    let object = thread.jvm.environment.new_object_array(&array_class, count);

                    frame.push([Operand {
                        objectref: object.ptr,
                    }]);

                    std::mem::forget(object);
                } else {
                    panic!()
                };
            }
            Bytecode::Putstatic(constant_index) => {
                let constant = class.constant_pool.constants.get(constant_index).unwrap();

                let field_ref = constant.as_ref().unwrap();

                let classpath = &field_ref.class;
                let target_class = jvm
                    .find_class(classpath, &*class_loader, thread)
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
                let target_class = jvm
                    .find_class(classpath, &*class_loader, thread)
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

                    let target_class = jvm
                        .find_class(path, &*class_loader, thread)
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
            Bytecode::New(constant_index) => {
                let constant = class.constant_pool.constants.get(constant_index).unwrap();

                if let Constant::Class { path, .. } = constant {

                    let target_class = jvm
                        .find_class(path, &*class_loader, thread)
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
                let [op] = frame.pop::<1>();
                frame.push([op, op]);
            }
            Bytecode::Aconst_null => {
                frame.push([Operand {
                    objectref: 0 as *mut (),
                }]);
            }
            Bytecode::Lookupswitch(default, entries) => {
                let [value] = frame.pop::<1>();
                let int = unsafe { value.data as i32 };

                let offset = entries.iter().find(|entry| entry.key == int).map(|entry| entry.offset).unwrap_or(*default);
                let target_offset = bytes_index as isize + offset as isize;
                let idx = code
                    .instructions
                    .iter()
                    .find(|instr| instr.bytes_index == target_offset as u32)
                    .unwrap();
                pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
            }
            _ => unimplemented!("Bytecode {:?} unimplemented in interpreter", bytecode),
        }

        *frame.program_counter = frame.program_counter.checked_add_signed(pc_inc).unwrap();

        drop(frame);

        ThreadStepResult::Ok
    }
}
