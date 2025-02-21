use std::fmt::{Debug, Formatter};
use std::mem::MaybeUninit;
use crate::bytecode::{ArrayType, Bytecode, JValue};
use crate::{Byte, Float, Int, Long, JVM};

use crate::classfile::resolved::{
    AccessFlags, Attribute, Class, Constant, FieldType, Method, MethodDescriptor,
    ReferenceKind, ReturnType,
};
use crate::linker::{routine_resolve_field, routine_resolve_method, routine_select_method, ClassLoader};
use bitflags::Flags;
use parking_lot::Mutex;

use std::ops::{BitAnd, BitOr, BitXor, Deref, DerefMut, Neg};
use std::pin::Pin;

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use once_cell::sync::OnceCell;
use crate::env::Object;
use crate::execution::MethodHandle;

#[derive(Debug)]
pub enum RuntimeException {
    NullPointer,
    IncompatibleClassChangeError,
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

    pub fn as_frame(&mut self) -> Frame {
        unsafe {
            Frame {
                method: &*self.method,
                class: &*self.class,
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

#[repr(u8)]
#[repr(align(8))]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum OperandTag {
    Ref = 0,
    Data = 1,
    Data2 = 2
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct OperandRef {
    pub ref_: *mut (),
    pub tag: OperandTag
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct OperandData {
    pub data: u64,
    pub tag: OperandTag
}

#[repr(C)]
pub union Operand {
    pub objectref: OperandRef,
    pub data: OperandData
}

impl Drop for Operand {

    fn drop(&mut self) {
        if self.tag() == OperandTag::Ref {
            // panic!("Cannot drop objectref operand");
        }
    }

}

impl Debug for Operand {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.tag() {
            OperandTag::Ref => write!(f, "@{}", self.ref_() as u32),
            OperandTag::Data | OperandTag::Data2 => write!(f, "{}", self.data())
        }
    }
}

impl Operand {

    pub fn tag(&self) -> OperandTag {
        unsafe { self.data.tag }
    }

    pub fn data(&self) -> u64 {
        assert!(matches!(self.tag(), OperandTag::Data | OperandTag::Data2));
        unsafe { self.data.data }
    }
    
    pub fn new_data(data: u32) -> Self {
        Self {
            data: OperandData {
                tag: OperandTag::Data,
                data: data as u64
            }
        }
    }

    pub fn new_data2(data: u64) -> Self {
        Self {
            data: OperandData {
                tag: OperandTag::Data2,
                data
            }
        }
    }

    pub fn copy_data(&self) -> Self {
        assert_ne!(unsafe { self.data.tag}, OperandTag::Ref);

        unsafe { std::ptr::read(self as *const Self) }
    }
    
    pub fn ref_(&self) -> *mut () {
        unsafe { self.objectref.ref_ }
    }

    pub fn new_ref(ref_: *mut ()) -> Self {
        Self {
            objectref: OperandRef {
                tag: OperandTag::Ref,
                ref_
            }
        }
    }
    
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

        let mut val: MaybeUninit<[Operand; N]> = MaybeUninit::uninit();
        let slice = &self.stack[*self.stack_length - N..*self.stack_length];

        unsafe { std::ptr::copy_nonoverlapping(slice.as_ptr(), &mut (val.assume_init_mut()[0]) as *mut Operand, N) };

        *self.stack_length -= N;
        unsafe { val.assume_init() }
    }

    pub fn pop_dynamic(&mut self, n: usize) -> Vec<Operand> {
        assert!(*self.stack_length >= n);

        let mut val = Vec::with_capacity(n);

        let slice = &self.stack[*self.stack_length - n..*self.stack_length];

        unsafe { std::ptr::copy_nonoverlapping(slice.as_ptr(), val.as_mut_ptr(), n) };
        unsafe { val.set_len(n) };

        *self.stack_length -= n;
        val
    }

    pub fn push<const N: usize>(&mut self, operands: [Operand; N]) {
        let len = *self.stack_length;

        for ((index, operand), slot) in operands.into_iter().enumerate().zip(&mut self.stack[len..len + N]) {
            *slot = operand;
        }

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

        // if self.frame_index as usize >= self.frames.len() {
        //     for frame in &self.frames[..] {
        //         console_log!("{} {}", unsafe { &(*frame.class).this_class }, unsafe {
        //             &(*frame.method).name
        //         });
        //     }
        // }

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
    pub instructions_executed: AtomicU64,
    pub thread_obj: OnceCell<Object>
}

impl Thread {

    #[must_use]
    pub(crate) fn invoke(&mut self, method_handle: &MethodHandle, args: Box<[Operand]>) -> Option<Operand> {
        let jvm = self.jvm.clone();

        let real_operand_count = method_handle.method.descriptor.args.len() + ((!method_handle.method.access_flags.contains(AccessFlags::STATIC)) as usize);
        assert_eq!(args.len(), real_operand_count, "Incorrect argument count calling {}", method_handle.method.name);
        
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
        args: Box<[JValue]>,
    ) -> Result<Option<JValue>, JavaBarrierError> {
        let method_handle = self.jvm.get_method(classpath, method_name, method_descriptor, self.get_id());
        let method = &*method_handle.method;

        let operands: Vec<Operand> = args.into_vec().into_iter().map(|arg| arg.to_operand()).collect();

        let operand = self.invoke(&method_handle, operands.into_boxed_slice());

        let jvalue = match (&method.descriptor.return_type, operand) {
            (ReturnType::FieldType(field_type), Some(return_value)) => Some(match field_type {
                FieldType::Byte => JValue::Byte(unsafe { return_value.data.data } as Byte),
                FieldType::Char => todo!(),
                FieldType::Double => {
                    JValue::Double(f64::from_ne_bytes(unsafe { return_value.data.data }.to_ne_bytes()))
                }
                FieldType::Float => {
                    JValue::Float(f32::from_ne_bytes((unsafe { return_value.data.data } as u32).to_ne_bytes()))
                }
                FieldType::Int => JValue::Int(unsafe { return_value.data.data } as i32),
                FieldType::Long => JValue::Long(unsafe { return_value.data.data } as i64),
                FieldType::Short => JValue::Short(unsafe { return_value.data.data } as i16),
                FieldType::Boolean => JValue::Int(unsafe { return_value.data.data } as i32),
                FieldType::Class(_) | FieldType::Array { .. } => JValue::Reference(unsafe {
                    self.jvm.environment.object_from_operand(return_value)
                }),
            }),
            (_, _) => None,
        };

        Ok(jvalue)
    }

    #[no_mangle]
    pub extern "C" fn interpret(thread: &mut Thread) -> Box<Operand> {
        let raw_frame = thread.frame_stack.get_top();
        let raw_frame = unsafe { &mut *raw_frame };

        loop {
            match Self::step(raw_frame, thread) {
                ThreadStepResult::Ok => {},
                ThreadStepResult::Error(error) => panic!("{:?}", error),
                ThreadStepResult::Return(value) => {

                    thread.frame_stack.pop();

                    return Box::new(value.unwrap_or(JValue::Long(0)).to_operand());
                }
            }
        }
    }

    pub fn init_static(class: &Class, thread: &mut Thread) {
        if !class.methods.iter().any(|field| *field.name == "<clinit>") {
            return;
        }

        if let Some(superclass) = &class.super_class {
            Self::init_static(superclass, thread);
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
                let method_handle = thread.jvm.get_method(&class.this_class, "<clinit>", "()V", class.class_loader.get_id());

                drop(thread.invoke(&method_handle, Box::new([])));

                class.static_init_state.store(2, Ordering::Release);
            }
            //Another thread is initializing, so wait until that's not the case
            Err(_) => while {
                let state = class.static_init_state.load(Ordering::Acquire);

                //state != 2; another thread just initialized statics, we can continue
                //state == thread.get_id() + 10; this thread is already initializing the class in another frame
                state != 2 && state != (thread.get_id() + 10)
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

        let locals: Vec<String> = frame.locals.iter().map(|local| {
            match local.tag() {
                OperandTag::Ref => format!("@{}", local.ref_() as usize as u64),
                OperandTag::Data | OperandTag::Data2 => format!("{}", local.data() as i32)
            }
        }).collect();
        let stack: Vec<String> = (&frame.stack[..*frame.stack_length]).iter().map(|local| {
            match local.tag() {
                OperandTag::Ref => format!("@{}", local.ref_() as usize as u64),
                OperandTag::Data | OperandTag::Data2 => format!("{}", local.data() as i32)
            }
        }).collect();

        if &*class.this_class != "java/lang/Class" && &*method.name != "<init>"
        {
            let instr_count = thread.instructions_executed.load(Ordering::Acquire);

            // if instr_count > 69000 {
            //     let mut stdout = thread.jvm.stdout.lock();
            //     write!(&mut stdout, "{}",
            //            format!(
            //                "{:<20} | {:<30} | {:<15} | {:<6} | [{}] | [{}] | {}",
            //                format!("{:?}", bytecode),
            //                class.this_class,
            //                method.identifier,
            //                pc,
            //                locals.join(", "),
            //                stack.join(", "),
            //                instr_count,
            //            )
            //     ).unwrap();
            // }
        }

        let jvm = thread.jvm.clone();

        match bytecode {
            Bytecode::Invokestatic(constant_index) => {
                let ref_ = class
                    .constant_pool
                    .constants
                    .get(constant_index)
                    .unwrap()
                    .as_ref()
                    .unwrap();
                
                let class = jvm.retrieve_class(&ref_.class, &*class.class_loader, thread).unwrap();

                if frame.class.get_id() != class.get_id() {
                    Self::init_static(&class, thread);
                }

                let method_handle = jvm.get_method(&class.this_class, &ref_.name_and_type.name, &ref_.name_and_type.descriptor, class.class_loader.get_id());

                let method_descriptor = &method_handle.method.descriptor;

                let args = frame.pop_dynamic(method_descriptor.args.len());
                let operand = thread.invoke(&method_handle, args.into_boxed_slice());

                if let Some(return_value) = operand {
                    frame.push([return_value]);
                }
            }

            Bytecode::Invokespecial(constant_index) => {
                let ref_ = class
                    .constant_pool
                    .constants
                    .get(constant_index)
                    .unwrap()
                    .as_ref()
                    .unwrap();

                let classpath = &ref_.class;
                let target_class = jvm
                    .retrieve_class(classpath, &*class.class_loader, thread)
                    .unwrap();

                let method = target_class.get_method(&ref_.name_and_type).unwrap();

                let c = if {
                    !method.is_init(classpath, thread)
                        && !target_class.access_flags.contains(AccessFlags::INTERFACE)
                        && JVM::is_subclass(&class, &target_class)
                        && target_class.access_flags.contains(AccessFlags::SUPER)
                } {
                    &**class.super_class.as_ref().unwrap()
                } else {
                    &target_class
                };

                Self::init_static(c, thread);

                let method = match c.get_method(&ref_.name_and_type) {
                    None => todo!(),
                    Some(method) => method,
                };

                if method.access_flags.contains(AccessFlags::SYNCHRONIZED)
                    | method.access_flags.contains(AccessFlags::NATIVE)
                {
                    //only one thread atm
                }

                let method_descriptor =
                    MethodDescriptor::try_from(&**ref_.name_and_type.descriptor).unwrap();

                let args = frame.pop_dynamic(method_descriptor.args.len() + 1);

                let method_handle = jvm.get_method(&c.this_class, &ref_.name_and_type.name, &ref_.name_and_type.descriptor, class.class_loader.get_id());

                let return_value = thread.invoke(&method_handle, Vec::from(args).into_boxed_slice());

                if let Some(operand) = return_value {
                    frame.push([operand]);
                }
            }
            Bytecode::Pop => {
                frame.pop::<1>();
            }
            Bytecode::Pop2 => {
                let [a] = frame.pop();

                if matches!(a.tag(), OperandTag::Data | OperandTag::Ref) {
                    let [b] = frame.pop();

                    if matches!(
                        (a.tag(), b.tag()),
                        (OperandTag::Ref | OperandTag::Data, OperandTag::Ref | OperandTag::Data)
                    ) {
                        if matches!(a.tag(), OperandTag::Ref) {
                            drop(unsafe { thread.jvm.environment.object_from_operand(a) });
                        };

                        if matches!(b.tag(), OperandTag::Ref) {
                            drop(unsafe { thread.jvm.environment.object_from_operand(b) });
                        }
                    } else {
                        panic!("Pop2 popped category 1 value, but then a category 2 value")
                    }
                }
            }
            Bytecode::Invokeinterface(constant_index, count) => {
                let interface_method = class
                    .constant_pool
                    .constants
                    .get(constant_index)
                    .unwrap()
                    .as_ref()
                    .unwrap();

                let descriptor = MethodDescriptor::try_from(&interface_method.name_and_type.descriptor[..]).unwrap();

                let mut nargs = frame.pop_dynamic(descriptor.args.len());
                let [objectref] = frame.pop();

                let object = unsafe { jvm.environment.object_ref_from_operand(&objectref) };
                let object_class = jvm.environment.get_object_class(&object);

                nargs.insert(0, objectref);

                let interface_class = class.class_loader.find_class(thread, &interface_method.class).unwrap();

                if !interface_class.access_flags.contains(AccessFlags::INTERFACE) {
                    return ThreadStepResult::Error(ThreadError::RuntimeException(RuntimeException::IncompatibleClassChangeError));
                }

                let java_lang_object = jvm.bootstrap_classloader.get_class(thread, "java/lang/Object").unwrap();

                let (resolved_method, resolved_method_class) = if let Some(method) = interface_class.get_method(&interface_method.name_and_type) {
                    (method, &interface_class)
                } else if let Some(method) = java_lang_object.get_method(&interface_method.name_and_type)
                    && method.access_flags.contains(AccessFlags::PUBLIC) && !method.access_flags.contains(AccessFlags::STATIC) {
                    (method, &java_lang_object)
                } else {
                    todo!()
                };

                let (selected_method, _) = routine_select_method(resolved_method, resolved_method_class, object_class);

                let handle = jvm.get_method(&selected_method.class, &selected_method.name, &selected_method.descriptor.string, class.class_loader.get_id());

                if let Some(op) = thread.invoke(&handle, nargs.into_boxed_slice()) {
                    frame.push([op]);
                }

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
                    .retrieve_class(classpath, &*class.class_loader, thread)
                    .unwrap();

                let resolved_method = routine_resolve_method(&thread, &method_ref, &method_ref_class).unwrap();
                let resolved_method_class = class.class_loader.find_class(thread, &method_ref.class).unwrap();

                let mut nargs = frame.pop_dynamic(resolved_method.descriptor.args.len());

                let [objectref] = frame.pop();

                let objectref = unsafe { thread.jvm.environment.object_from_operand(objectref) };
                let object_class = thread.jvm.environment.get_object_class(&objectref);

                nargs.insert(0, jvm.environment.clone_reference(&objectref).consume());

                if !resolved_method.is_signature_polymorphic(classpath) {

                    let (selected_method, _) = routine_select_method(resolved_method, &resolved_method_class, object_class);
                    let handle = jvm.get_method(&selected_method.class, &selected_method.name, &selected_method.descriptor.string, class.class_loader.get_id());

                    frame.push_dynamic(thread.invoke(&handle, Vec::from(nargs).into_boxed_slice()));

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

                return ThreadStepResult::Return(Some(JValue::Int(unsafe { int.data.data } as Int)));
            }
            Bytecode::Dreturn => {
                let [double] = frame.pop();

                return ThreadStepResult::Return(Some(JValue::Double(f64::from_bits(double.data()))));
            }
            Bytecode::Freturn => {
                let [float] = frame.pop();

                return ThreadStepResult::Return(Some(JValue::Float(Float::from_bits(float.data() as u32))));
            }
            Bytecode::Lreturn => {
                let [long] = frame.pop();

                return ThreadStepResult::Return(Some(JValue::Long(long.data() as Long)));
            }
            Bytecode::Bipush(value) => frame.push([Operand::new_data(*value as i32 as u32)]),
            Bytecode::Aload_n(index) | Bytecode::Aload(index) => {
                let operand = &frame.locals[*index as usize];

                frame.push([unsafe { jvm.environment.clone_reference(jvm.environment.object_ref_from_operand(&operand)) }.consume()]);
            }
            Bytecode::Areturn => {
                let [ptr] = frame.pop();

                return ThreadStepResult::Return(Some(JValue::Reference(unsafe {
                    thread.jvm.environment.object_from_operand(ptr)
                })));
            }
            Bytecode::Putfield(constant_index) => {
                let constant = class.constant_pool.constants.get(constant_index).unwrap();

                let field_ref = constant.as_ref().unwrap();

                let classpath = &field_ref.class;
                let target_class = jvm
                    .retrieve_class(classpath, &*class.class_loader, thread)
                    .unwrap();

                let (field, field_class) =
                    routine_resolve_field(&field_ref.name_and_type, &target_class).unwrap();

                let [object, value] = frame.pop();
                let object = unsafe { thread.jvm.environment.object_from_operand(object) };

                unsafe {
                    thread.jvm.environment.set_object_field(&object, &field_class, &field.name, &field_ref.name_and_type.descriptor, value);
                }
            }
            Bytecode::Getfield(constant_index) => {
                let constant = class.constant_pool.constants.get(constant_index).unwrap();

                let field_ref = constant.as_ref().unwrap();

                let classpath = &field_ref.class;
                let target_class = jvm
                    .retrieve_class(classpath, &*class.class_loader, thread)
                    .unwrap();

                let (_, field_class) = routine_resolve_field(&field_ref.name_and_type, &target_class).unwrap();

                let [ptr] = frame.pop();

                let object = unsafe { thread.jvm.environment.object_from_operand(ptr) };
                let operand = unsafe { thread.jvm.environment.get_object_field(&object, &field_class, &field_ref.name_and_type.name, &field_ref.name_and_type.descriptor) };

                if operand.tag() == OperandTag::Ref {
                    let ref_clone = jvm.environment.clone_reference(unsafe { jvm.environment.object_ref_from_operand(&operand) });

                    frame.push([ref_clone.consume()]);
                } else {
                    frame.push([operand]);
                }
            }
            Bytecode::Iconst_n_m1(value) => frame.push([Operand::new_data(*value as u32)]),
            Bytecode::Lconst_n(value) => frame.push([Operand::new_data2(*value as u64)]),
            Bytecode::Fconst_n(index) => {
                frame.push([Operand::new_data(u32::from_ne_bytes(match index {
                    0 => 0.0f32,
                    1 => 1.0f32,
                    2 => 2.0f32,
                    _ => unreachable!()
                }.to_be_bytes()))]);
            }
            Bytecode::Dconst_n(double) => {
                frame.push([Operand::new_data2(double.to_bits())]);
            }
            Bytecode::Fcmpg | Bytecode::Fcmpl => {
                let [a, b] = frame.pop();

                let a = f32::from_ne_bytes((a.data() as u32).to_ne_bytes());
                let b = f32::from_ne_bytes((b.data() as u32).to_ne_bytes());

                let nan_out = if matches!(bytecode, Bytecode::Fcmpg) { 1 } else { -1i32 as u32 };

                frame.push([Operand::new_data(if a > b { 1 } else if a == b { 0 } else if a < b { -1i32 as u32 } else { nan_out })]);
            }
            Bytecode::L2f => {
                let [op] = frame.pop();

                frame.push([Operand::new_data((f64::from_bits(op.data()) as f32).to_bits())]);
            }
            Bytecode::If_acmpne(offset) => {
                let [a, b] = frame.pop();

                let object_a = unsafe { jvm.environment.object_from_operand(a) };
                let object_b = unsafe { jvm.environment.object_from_operand(b) };

                if !jvm.environment.object_ref_eq(&object_a, &object_b) {
                    let target_offset = bytes_index as isize + *offset as isize;
                    let idx = code
                        .instructions
                        .iter()
                        .find(|instr| instr.bytes_index == target_offset as u32)
                        .unwrap();
                    pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
                }
            }
            Bytecode::If_acmpeq(offset) => {
                let [a, b] = frame.pop();

                let object_a = unsafe { jvm.environment.object_from_operand(a) };
                let object_b = unsafe { jvm.environment.object_from_operand(b) };

                if jvm.environment.object_ref_eq(&object_a, &object_b) {
                    let target_offset = bytes_index as isize + *offset as isize;
                    let idx = code
                        .instructions
                        .iter()
                        .find(|instr| instr.bytes_index == target_offset as u32)
                        .unwrap();
                    pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
                }
            }
            Bytecode::If_icmpeq(offset) => {
                let [a, b] = frame.pop();

                let a = a.data() as i32;
                let b = b.data() as i32;

                if a == b {
                    let target_offset = bytes_index as isize + *offset as isize;
                    let idx = code
                        .instructions
                        .iter()
                        .find(|instr| instr.bytes_index == target_offset as u32)
                        .unwrap();
                    pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
                }
            }
            Bytecode::If_icmpge(offset) => {
                let [a, b] = frame.pop();

                let a = a.data() as i32;
                let b = b.data() as i32;

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
            Bytecode::If_icmpgt(offset) => {
                let [a, b] = frame.pop();

                let a = a.data() as i32;
                let b = b.data() as i32;

                if a > b {
                    let target_offset = bytes_index as isize + *offset as isize;
                    let idx = code
                        .instructions
                        .iter()
                        .find(|instr| instr.bytes_index == target_offset as u32)
                        .unwrap();
                    pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
                }
            }
            Bytecode::If_icmpne(offset) => {
                let [a, b] = frame.pop();

                let a = a.data() as i32;
                let b = b.data() as i32;

                if a != b {
                    let target_offset = bytes_index as isize + *offset as isize;
                    let idx = code
                        .instructions
                        .iter()
                        .find(|instr| instr.bytes_index == target_offset as u32)
                        .unwrap();
                    pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
                }
            }
            Bytecode::If_icmplt(offset) => {
                let [a, b] = frame.pop();

                let a = a.data() as i32;
                let b = b.data() as i32;

                if a < b {
                    let target_offset = bytes_index as isize + *offset as isize;
                    let idx = code
                        .instructions
                        .iter()
                        .find(|instr| instr.bytes_index == target_offset as u32)
                        .unwrap();
                    pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
                }
            }
            Bytecode::If_icmple(offset) => {
                let [a, b] = frame.pop();

                let a = a.data() as i32;
                let b = b.data() as i32;

                if a <= b {
                    let target_offset = bytes_index as isize + *offset as isize;
                    let idx = code
                        .instructions
                        .iter()
                        .find(|instr| instr.bytes_index == target_offset as u32)
                        .unwrap();
                    pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
                }
            }
            Bytecode::Ifeq(offset) => {
                let [value] = frame.pop();

                let value = value.data() as i32;

                if value == 0 {
                    let target_offset = bytes_index as isize + *offset as isize;
                    let idx = code
                        .instructions
                        .iter()
                        .find(|instr| instr.bytes_index == target_offset as u32)
                        .unwrap();
                    pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
                }
            }
            Bytecode::Ifne(offset) => {
                let [value] = frame.pop();

                let value = value.data() as i32;

                if value != 0 {
                    let target_offset = bytes_index as isize + *offset as isize;
                    let idx = code
                        .instructions
                        .iter()
                        .find(|instr| instr.bytes_index == target_offset as u32)
                        .unwrap();
                    pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
                }
            }
            Bytecode::I2f => {
                let [value] = frame.pop();

                let value = value.data() as i32;

                frame.push([Operand::new_data((value as f32).to_bits() as u32)])
            }
            Bytecode::F2i => {
                let [value] = frame.pop();

                let value = f32::from_bits(value.data() as u32);

                frame.push([Operand::new_data(value as i32 as u32)])
            }
            Bytecode::F2d => {
                let [value] = frame.pop();

                let value = f32::from_bits(value.data() as u32) as f64;

                frame.push([Operand::new_data2(value.to_bits())])
            }
            Bytecode::D2l => {
                let [value] = frame.pop();

                let value = f64::from_bits(value.data());

                frame.push([Operand::new_data2(value as u64)])
            }
            Bytecode::Fmul => {
                let [a, b] = frame.pop();
                let a = a.data() as f32;
                let b = b.data() as f32;

                frame.push([Operand::new_data((a*b).to_bits())]);
            }
            Bytecode::Fadd => {
                let [a, b] = frame.pop();
                let a = a.data() as f32;
                let b = b.data() as f32;

                frame.push([Operand::new_data((a+b).to_bits())]);
            }
            Bytecode::Fsub => {
                let [a, b] = frame.pop();
                let a = a.data() as f32;
                let b = b.data() as f32;

                frame.push([Operand::new_data((a-b).to_bits())]);
            }
            Bytecode::Dmul => {
                let [a, b] = frame.pop();
                let a = f64::from_bits(a.data());
                let b = f64::from_bits(b.data());

                frame.push([Operand::new_data2((a*b).to_bits())]);
            }
            Bytecode::Dadd => {
                let [a, b] = frame.pop();
                let a = f64::from_bits(a.data());
                let b = f64::from_bits(b.data());

                frame.push([Operand::new_data2((a+b).to_bits())]);
            }
            Bytecode::Dsub => {
                let [a, b] = frame.pop();
                let a = f64::from_bits(a.data());
                let b = f64::from_bits(b.data());

                frame.push([Operand::new_data2((a-b).to_bits())]);
            }
            Bytecode::Ddiv => {
                let [a, b] = frame.pop();
                let a = f64::from_bits(a.data());
                let b = f64::from_bits(b.data());

                frame.push([Operand::new_data2((a/b).to_bits())]);
            }
            Bytecode::Fdiv => {
                let [a, b] = frame.pop();
                let a = a.data() as f32;
                let b = b.data() as f32;

                frame.push([Operand::new_data((a/b).to_bits())]);
            }
            Bytecode::Ifge(offset) => {
                let [value] = frame.pop();

                let value = value.data() as i32;

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

                let value = value.data() as i32;

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
            Bytecode::Iflt(offset) => {
                let [value] = frame.pop();

                let value = value.data() as i32;

                if value < 0 {
                    let target_offset = bytes_index as isize + *offset as isize;
                    let idx = code
                        .instructions
                        .iter()
                        .find(|instr| instr.bytes_index == target_offset as u32)
                        .unwrap();
                    pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
                }
            }
            Bytecode::Ifgt(offset) => {
                let [value] = frame.pop();

                let value = value.data() as i32;

                if value > 0 {
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
                let object = unsafe { jvm.environment.object_from_operand(reference) };

                if !jvm.environment.is_null(&object) {
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
                let object = unsafe { jvm.environment.object_from_operand(reference) };

                if jvm.environment.is_null(&object) {
                    let target_offset = bytes_index as isize + *offset as isize;
                    let idx = code
                        .instructions
                        .iter()
                        .find(|instr| instr.bytes_index == target_offset as u32)
                        .unwrap();
                    pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
                }
            }
            Bytecode::Astore(index)
            | Bytecode::Astore_n(index) => {
                let old = std::mem::replace(&mut frame.locals[*index as usize], Operand::new_ref(0 as _));
                if old.tag() == OperandTag::Ref {
                    drop(unsafe { jvm.environment.object_from_operand(old) });
                }

                let [op] = frame.pop::<1>();
                frame.locals[*index as usize] = op;
            }
            Bytecode::Istore(index)
            | Bytecode::Istore_n(index)
            | Bytecode::Fstore(index)
            | Bytecode::Fstore_n(index) => {
                let [op] = frame.pop::<1>();
                frame.locals[*index as usize] = op;
            }
            Bytecode::Lstore(index)
            | Bytecode::Lstore_n(index) => {
                let [op] = frame.pop();
                let long = op.data();

                frame.locals[*index as usize] = Operand::new_data2(long & ((u32::MAX as u64) << 32));
                frame.locals[*index as usize + 1] = Operand::new_data2(long & (u32::MAX as u64));
            }
            Bytecode::Lload(index) | Bytecode::Lload_n(index) => {
                let op1 = frame.locals[*index as usize].data();
                let op2 = frame.locals[*index as usize + 1].data();

                frame.push([Operand::new_data2((op1 << 32) | op2)]);
            }
            Bytecode::Iload(index) | Bytecode::Iload_n(index) | Bytecode::Fload_n(index) | Bytecode::Fload(index) => {
                frame.push([frame.locals[*index as usize].copy_data()]);
            }
            Bytecode::Monitorenter | Bytecode::Monitorexit => {
                let [op] = frame.pop();
                drop(unsafe { jvm.environment.object_from_operand(op) });
            }
            Bytecode::I2l => {
                let [int] = frame.pop::<1>();
                frame.push([
                    Operand::new_data2(int.data() as i64 as u64)
                ]);
            }
            Bytecode::I2c => {
                let [int] = frame.pop::<1>();
                frame.push([
                    Operand::new_data(int.data() as u32 & u16::MAX as u32)
                ]);
            }
            Bytecode::L2i => {
                let [int] = frame.pop::<1>();
                frame.push([
                    Operand::new_data(int.data() as u32 & u32::MAX)
                ]);
            }
            Bytecode::I2b => {
                let [int] = frame.pop::<1>();
                frame.push([
                    Operand::new_data(int.data() as u8 as u32)
                ]);
            }
            Bytecode::Ladd => {
                let [a, b] = frame.pop();

                frame.push([
                    Operand::new_data2(((a.data() as i64) + (b.data() as i64)) as u64)
                ]);
            }
            Bytecode::Lmul => {
                let [a, b] = frame.pop();

                frame.push([
                    Operand::new_data2(((a.data() as i64) * (b.data() as i64)) as u64)
                ]);
            }
            Bytecode::Lsub => {
                let [a, b] = frame.pop();

                frame.push([
                    Operand::new_data2(((a.data() as i64) - (b.data() as i64)) as u64)
                ]);
            }
            Bytecode::Lcmp => {
                let [a, b] = frame.pop();

                let a = a.data() as i64;
                let b = b.data() as i64;

                let result: i64 = if a > b { 1 } else if a == b { 0 } else { -1 };

                frame.push([
                    Operand::new_data2(result as u64)
                ]);
            }
            Bytecode::Iushr => {
                let [a, b] = frame.pop();
                frame.push([
                    Operand::new_data((a.data() as i32 >> ((b.data() as i32) & 0b11111)) as u32)
                ]);
            }
            Bytecode::Lshl => {
                let [a, b] = frame.pop();
                frame.push([
                    Operand::new_data2(((a.data() as i64) << ((b.data() as i32 as i64) & 0b111111)) as u64)
                ]);
            }
            Bytecode::Lshr => {
                let [a, b] = frame.pop();
                frame.push([
                    Operand::new_data2(((a.data() as i64) >> ((b.data() as i32 as i64) & 0b111111)) as u64)
                ]);
            }
            Bytecode::Ishl => {
                let [a, b] = frame.pop();
                frame.push([
                    Operand::new_data(((a.data() as i32) << ((b.data() as i32) & 0b11111)) as u32)
                ]);
            }
            Bytecode::Ishr => {
                let [a, b] = frame.pop();
                frame.push([
                    Operand::new_data(((a.data() as i32) >> ((b.data() as i32) & 0b11111)) as u32)
                ]);
            }
            Bytecode::Ixor => {
                let [a, b] = frame.pop();
                frame.push([
                    Operand::new_data((a.data() as i32).bitxor(b.data() as i32) as u32)
                ]);
            }
            Bytecode::Ior => {
                let [a, b] = frame.pop();
                frame.push([
                    Operand::new_data((a.data() as i32).bitor(b.data() as i32) as u32)
                ]);
            }
            Bytecode::Iand => {
                let [a, b] = frame.pop();
                frame.push([
                    Operand::new_data((a.data() as i32).bitand(b.data() as i32) as u32)
                ]);
            }
            Bytecode::Land => {
                let [a, b] = frame.pop();
                frame.push([
                    Operand::new_data2((a.data() as i64).bitand(b.data() as i64) as u64)
                ]);
            }
            Bytecode::Lor => {
                let [a, b] = frame.pop();
                frame.push([
                    Operand::new_data2((a.data() as i64).bitor(b.data() as i64) as u64)
                ]);
            }
            Bytecode::Lxor => {
                let [a, b] = frame.pop();
                frame.push([
                    Operand::new_data2((a.data() as i64).bitxor(b.data() as i64) as u64)
                ]);
            }
            Bytecode::Lneg => {
                let [a] = frame.pop();
                frame.push([
                    Operand::new_data2((a.data() as i64).neg() as u64)
                ]);
            }
            Bytecode::Ineg => {
                let [a] = frame.pop();
                frame.push([
                    Operand::new_data((a.data() as i32).neg() as u32)
                ]);
            }
            Bytecode::Iadd => {
                let [a, b] = frame.pop();
                frame.push([
                    Operand::new_data(((a.data() as i32) + (b.data() as i32)) as u32)
                ]);
            }
            Bytecode::Irem => {
                let [a, b] = frame.pop();

                let a = a.data() as i32;
                let b = b.data() as i32;

                assert_ne!(b, 0);

                frame.push([
                    Operand::new_data((a - (a / b) * b) as u32)
                ]);
            }
            Bytecode::Imul => {
                let [a, b] = frame.pop();
                frame.push([
                    Operand::new_data((a.data() as i32).overflowing_mul(b.data() as i32).0 as u32)
                ]);
            }
            Bytecode::Idiv => {
                let [a, b] = frame.pop();
                frame.push([
                    Operand::new_data(((a.data() as i32) / (b.data() as i32)) as u32)
                ]);
            }
            Bytecode::Isub => {
                let [a, b] = frame.pop();
                frame.push([
                    Operand::new_data(((a.data() as i32) - (b.data() as i32)) as u32)
                ]);
            }
            Bytecode::Sipush(short) => {
                frame.push([Operand::new_data(*short as u32)]);
            }
            Bytecode::Iinc(index, inc) => {
                let int = frame.locals[*index as usize].data() as i32;
                frame.locals[*index as usize] = Operand::new_data((int + (*inc as i32)) as u32);
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
                        frame.push([Operand::new_data( *int as u32)]);
                    }
                    Constant::Float(float) => {
                        frame.push([Operand::new_data(u32::from_ne_bytes(float.to_ne_bytes()))]);
                    }
                    Constant::String(string) => {
                        let string = jvm.new_string(&string, thread);

                        frame.push([
                            string.consume()
                        ]);
                    }
                    Constant::Class {
                        path, depth
                    } => {
                        if *depth == 0 {
                            let class = class.class_loader.find_class(thread, path).unwrap();
                            let object = jvm.environment.clone_reference(class.class_instance.get().unwrap());

                            let class = jvm.environment.get_object_class(&object);

                            frame.push([object.consume()]);
                        } else {
                            let class = class.class_loader.find_class(thread, path).unwrap();
                            let class = jvm.find_array_class(thread, class, *depth);
                            let object = jvm.environment.clone_reference(class.class_instance.get().unwrap());

                            frame.push([object.consume()]);
                        }
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
                        frame.push([Operand::new_data(*int as u32)]);
                    }
                    Constant::Float(float) => {
                        frame.push([Operand::new_data(u32::from_ne_bytes(float.to_ne_bytes()))]);
                    }
                    Constant::String(string) => {
                        let string = jvm.new_string(string, thread);

                        frame.push([string.consume()]);
                    }
                    Constant::Class {
                        path, depth
                    } => {
                        if *depth == 0 {
                            let class = class.class_loader.find_class(thread, path).unwrap();
                            let object = jvm.environment.clone_reference(class.class_instance.get().unwrap());

                            frame.push([object.consume()]);
                        } else {
                            let class = class.class_loader.find_class(thread, path).unwrap();
                            let class = jvm.find_array_class(thread, class, *depth);
                            let object = jvm.environment.clone_reference(class.class_instance.get().unwrap());

                            frame.push([object.consume()]);
                        }
                    }
                    _ => unimplemented!("Ldc_w {constant:?} unimplemented"),
                }
            }
            Bytecode::Ldc2_w(constant_index) => {
                let constant = class
                    .constant_pool
                    .constants
                    .get(&(*constant_index))
                    .unwrap();

                match constant {
                    Constant::Long(long) => {
                        frame.push([Operand::new_data2(*long as u64)]);
                    }
                    Constant::Double(double) => {
                        frame.push([Operand::new_data2(double.to_bits())]);
                    }
                    _ => unimplemented!("Invalid Ldc2_w constant"),
                }
            }
            Bytecode::Tableswitch { default, low, high, offsets } => {
                let [val] = frame.pop();
                let int = val.data() as i32;

                let byte_offset;

                if int < *low || int > *high {
                    byte_offset = *default;
                } else {
                    byte_offset = offsets[(int - *low) as usize];
                }

                let target_offset = bytes_index as isize + byte_offset as isize;
                let idx = code
                    .instructions
                    .iter()
                    .find(|instr| instr.bytes_index == target_offset as u32)
                    .unwrap();
                pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;
            }
            Bytecode::Iastore => {
                let [arrayref, index, value] = frame.pop();

                let array = unsafe { thread.jvm.environment.object_from_operand(arrayref) };

                if jvm.environment.is_null(&array) {
                    return ThreadStepResult::Error(ThreadError::RuntimeException(
                        RuntimeException::NullPointer,
                    ));
                }

                let index = index.data() as i32;

                unsafe { thread.jvm.environment.set_array_element(ArrayType::Int, &array, index, value) };
            }
            Bytecode::Bastore => {
                let [arrayref, index, value] = frame.pop();

                let array = unsafe { thread.jvm.environment.object_from_operand(arrayref) };

                if jvm.environment.is_null(&array) {
                    return ThreadStepResult::Error(ThreadError::RuntimeException(
                        RuntimeException::NullPointer,
                    ));
                }

                let index = index.data() as i32;

                unsafe { thread.jvm.environment.set_array_element(ArrayType::Byte, &array, index, value) };
            }
            Bytecode::Castore => {
                let [arrayref, index, value] = frame.pop();

                let array = unsafe { thread.jvm.environment.object_from_operand(arrayref) };
                if jvm.environment.is_null(&array) {
                    return ThreadStepResult::Error(ThreadError::RuntimeException(
                        RuntimeException::NullPointer,
                    ));
                }

                let index = index.data() as i32;

                unsafe { thread.jvm.environment.set_array_element(ArrayType::Char, &array, index, value) };
            }
            Bytecode::Aastore => {
                let [arrayref, index, value] = frame.pop();

                let array = unsafe { thread.jvm.environment.object_from_operand(arrayref) };
                if jvm.environment.is_null(&array) {
                    return ThreadStepResult::Error(ThreadError::RuntimeException(
                        RuntimeException::NullPointer,
                    ));
                }

                let index = index.data() as i32;

                unsafe { thread.jvm.environment.set_object_array_element(&array, index, value) };
            }
            Bytecode::Aaload => {
                let [arrayref, index] = frame.pop();

                let array = unsafe { thread.jvm.environment.object_from_operand(arrayref) };
                if jvm.environment.is_null(&array) {
                    return ThreadStepResult::Error(ThreadError::RuntimeException(
                        RuntimeException::NullPointer,
                    ));
                }

                let index = index.data() as i32;

                let element = unsafe { thread.jvm.environment.get_object_array_element(&array, index) };

                frame.push([element.consume()]);
            }
            Bytecode::Caload => {
                let [arrayref, index] = frame.pop();

                let array = unsafe { thread.jvm.environment.object_from_operand(arrayref) };
                if jvm.environment.is_null(&array) {
                    return ThreadStepResult::Error(ThreadError::RuntimeException(
                        RuntimeException::NullPointer,
                    ));
                }

                let index = index.data() as i32;

                let element = unsafe { thread.jvm.environment.get_array_element(ArrayType::Char, &array, index) };

                frame.push([element]);
            }
            Bytecode::Baload => {
                let [arrayref, index] = frame.pop();

                let array = unsafe { thread.jvm.environment.object_from_operand(arrayref) };
                if jvm.environment.is_null(&array) {
                    return ThreadStepResult::Error(ThreadError::RuntimeException(
                        RuntimeException::NullPointer,
                    ));
                }

                let index = index.data() as i32;

                let element = unsafe { thread.jvm.environment.get_array_element(ArrayType::Byte, &array, index) };

                frame.push([element]);
            }
            Bytecode::Newarray(type_) => {
                let [count] = frame.pop();
                let count = unsafe { count.data() as i32 };

                let object = jvm.environment.new_array(*type_, count);

                frame.push([object.consume()]);
            }
            Bytecode::Instanceof(constant_index) => {
                let [objectref] = frame.pop();
                let object = unsafe { thread.jvm.environment.object_from_operand(objectref) };

                let constant = class.constant_pool.constants.get(constant_index).unwrap();

                let (classpath, depth) = if let Constant::Class { path, depth } = constant {
                    (path, *depth)
                } else {
                    panic!()
                };

                let component_class = class.class_loader.find_class(thread, classpath).unwrap();
                let type_class = jvm.find_array_class(thread, component_class, depth);

                if !jvm.environment.is_null(&object) {
                    let object_class = jvm.environment.get_object_class(&object);

                    if &*type_class == object_class || object_class.parents_and_interfaces().iter().any(|super_| &***super_ == &*type_class) {
                        frame.push([Operand::new_data(1)]);
                    } else {
                        frame.push([Operand::new_data(0)]);
                    }
                } else {
                    frame.push([Operand::new_data(0)]);
                }
            }
            Bytecode::Checkcast(constant_index) => {
                let [objectref] = frame.pop();
                let object = unsafe { thread.jvm.environment.object_from_operand(objectref) };

                let constant = class.constant_pool.constants.get(constant_index).unwrap();

                let type_class;

                if let Constant::Class { path, depth } = constant {
                    if *depth == 0 {
                        type_class = class.class_loader.find_class(thread, path).unwrap()
                    } else {
                        let component = class.class_loader.find_class(thread, path).unwrap();
                        type_class = jvm.find_array_class(thread, component, *depth);
                    }
                } else {
                    panic!()
                };

                if !jvm.environment.is_null(&object) {
                    let object_class = jvm.environment.get_object_class(&object);

                    if &*type_class == object_class || object_class.parents_and_interfaces().iter().any(|super_| &***super_ == &*type_class) || object_class.interfaces.iter().any(|super_| &**super_ == &*type_class) {

                    } else {
                        panic!("{} {}", object_class.this_class, type_class.this_class);
                    }
                }

                frame.push([object.consume()]);
            }
            Bytecode::Arraylength => {
                let [objectref] = frame.pop();
                let object = unsafe { jvm.environment.object_from_operand(objectref) };

                frame.push([Operand::new_data(jvm.environment.get_array_length(&object) as u32)]);
            }
            Bytecode::Anewarray(constant_index) => {
                let [count] = frame.pop();
                let count = count.data() as i32 ;

                let constant = class.constant_pool.constants.get(constant_index).unwrap();
                if let Constant::Class { path, depth } = constant {
                    let component_class = jvm
                        .retrieve_class(&path, &*class.class_loader, thread)
                        .unwrap();

                    let array_class = jvm.find_array_class(thread, component_class, *depth + 1);

                    let object = thread.jvm.environment.new_object_array(&array_class, count);

                    frame.push([object.consume()]);
                } else {
                    panic!()
                };
            }
            Bytecode::Athrow => {
                let [objectref] = frame.pop();
                let object = unsafe { jvm.environment.object_from_operand(objectref) };
                let object_class = jvm.environment.get_object_class(&object);

                console_log!("Athrow {}", object_class.this_class);

                'a: {

                    for exception in &code.exception_table {
                        if !(bytes_index >= exception.start_pc as u32 && bytes_index <= exception.end_pc as u32) {
                            continue;
                        }

                        if &*exception.catch_type == &*object_class.this_class || {
                            let clazz = class.class_loader.find_class(thread, &exception.catch_type).unwrap();
                            JVM::is_subclass(object_class, &clazz)
                        } {
                            let target_offset = exception.handler_pc as u32;
                            let idx = code
                                .instructions
                                .iter()
                                .find(|instr| instr.bytes_index == target_offset)
                                .unwrap();
                            pc_inc = idx.bytecode_index as i32 - instruction.bytecode_index as i32;

                            for op in frame.stack.iter_mut() {
                                *op = Operand::new_data(0);
                            }

                            *frame.stack_length = 0;

                            break 'a;
                        }
                    }

                    let to_string = jvm.get_method("java/lang/Throwable", "toString", "()Ljava/lang/String;", class.class_loader.get_id());

                    let string_op = thread.invoke(&to_string, Box::new([
                        object.consume()
                    ])).unwrap();

                    let string = jvm.get_string(&unsafe { jvm.environment.object_from_operand(string_op) });

                    panic!("Athrow fail\n{string}");

                }
            }
            Bytecode::Putstatic(constant_index) => {
                let constant = class.constant_pool.constants.get(constant_index).unwrap();

                let field_ref = constant.as_ref().unwrap();

                let classpath = &field_ref.class;
                let target_class = jvm
                    .retrieve_class(classpath, &*class.class_loader, thread)
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
                    .retrieve_class(classpath, &*class.class_loader, thread)
                    .unwrap();

                let (_, resolved_class) = routine_resolve_field(&field_ref.name_and_type, &target_class).unwrap();

                if resolved_class.this_class != class.this_class {
                    Self::init_static(&resolved_class, thread);
                }

                let index = resolved_class
                    .fields
                    .iter()
                    .filter(|field| field.access_flags.contains(AccessFlags::STATIC))
                    .position(|field| field.name == field_ref.name_and_type.name)
                    .unwrap();

                let operand = unsafe { &(*resolved_class.statics)[index] };

                if operand.tag() == OperandTag::Ref {
                    let object = unsafe { thread.jvm.environment.object_ref_from_operand(operand) };
                    let ref_clone = jvm.environment.clone_reference(&object);

                    frame.push([ref_clone.consume()]);
                } else {
                    frame.push([operand.copy_data()]);
                }
            }
            Bytecode::New(constant_index) => {
                let constant = class.constant_pool.constants.get(constant_index).unwrap();

                if let Constant::Class { path, .. } = constant {

                    let target_class = jvm
                        .retrieve_class(path, &*class.class_loader, thread)
                        .unwrap();

                    if target_class.this_class != class.this_class {
                        Self::init_static(&target_class, thread);
                    }

                    let object = thread.jvm.environment.new_object(&target_class);
                    frame.push([object.consume()]);
                } else {
                    panic!()
                }
            }
            Bytecode::Dup => {
                let [operand] = frame.pop::<1>();

                if operand.tag() == OperandTag::Ref {
                    let object = unsafe { thread.jvm.environment.object_from_operand(operand) };
                    let ref_clone = jvm.environment.clone_reference(&object);

                    frame.push([object.consume(), ref_clone.consume()]);
                } else {
                    frame.push([operand.copy_data(), operand]);
                }
            }
            Bytecode::Dup2 => {
                let [a] = frame.pop();

                if matches!(a.tag(), OperandTag::Data2) {
                    frame.push([a.copy_data(), a]);
                } else {
                    let [b] = frame.pop();

                    if matches!(
                        (a.tag(), b.tag()),
                        (OperandTag::Ref | OperandTag::Data, OperandTag::Ref | OperandTag::Data)
                    ) {
                        let [a1, a2] = if matches!(a.tag(), OperandTag::Ref) {
                            let object = unsafe { thread.jvm.environment.object_from_operand(a) };
                            [jvm.environment.clone_reference(&object).consume(), object.consume()]
                        } else {
                            [a.copy_data(), a]
                        };

                        let [b1, b2] = if matches!(b.tag(), OperandTag::Ref) {
                            let object = unsafe { thread.jvm.environment.object_from_operand(b) };
                            [jvm.environment.clone_reference(&object).consume(), object.consume()]
                        } else {
                            [b.copy_data(), b]
                        };

                        frame.push([b1, a1, b2, a2]);
                    } else {
                        panic!()
                    }
                }
            }
            Bytecode::Dup_x1 => {
                let [a, b] = frame.pop();

                if b.tag() == OperandTag::Ref {
                    let object = unsafe { thread.jvm.environment.object_from_operand(b) };
                    let ref_clone = jvm.environment.clone_reference(&object);

                    frame.push([object.consume(), a, ref_clone.consume()]);
                } else {
                    frame.push([b.copy_data(), a, b]);
                }
            }
            Bytecode::Aconst_null => {
                frame.push([Object::NULL.consume()]);
            }
            Bytecode::Lookupswitch(default, entries) => {
                let [value] = frame.pop::<1>();
                let int = unsafe { value.data() as i32 };

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

        thread.instructions_executed.fetch_add(1, Ordering::Relaxed);

        *frame.program_counter = frame.program_counter.checked_add_signed(pc_inc).unwrap();

        drop(frame);

        ThreadStepResult::Ok
    }
}
