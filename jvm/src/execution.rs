use crate::classfile::resolved::{AccessFlags, Class, FieldType, Method, Ref};
use crate::thread::{FrameStack, Operand, RawFrame, Thread};

use std::fmt::{Debug, DebugStruct, Formatter, FormattingOptions};
use std::io::Cursor;
use std::sync::Arc;
use std::sync::atomic::Ordering;
use crate::env::Object;

pub type ABIHandlePtr = Option<unsafe extern "C" fn(thread: &mut Thread) -> Option<Box<Operand>>>;

pub struct InterpreterContext {
    //RawFrame contains a pointer to the Ref and the Class, these keep the pointers alive
    ref_: Arc<Ref>,
    class: Arc<Class>,
}

impl InterpreterContext {
    pub fn new(ref_: Arc<Ref>, class: Arc<Class>) -> Self {
        Self { ref_, class }
    }
}

pub enum ExecutionContext {
    //Interpret requires that any caller pushes the appropriate frame onto the stack before calling
    Interpret(InterpreterContext),
    Compiled,
    Native,
}

impl Debug for ExecutionContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ExecutionContext::{}",
            match self {
                ExecutionContext::Interpret(_) => "Interpret",
                ExecutionContext::Compiled => "JIT",
                ExecutionContext::Native => "Native",
            }
        )
    }
}

pub struct MethodHandle {
    pub(crate) ptr: ABIHandlePtr,
    pub(crate) context: ExecutionContext,
    pub(crate) class: Arc<Class>,
    pub(crate) method: Arc<Method>,
    pub(crate) class_loader: u32,
}

impl Debug for MethodHandle {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MethodHandle").finish()
    }
}

impl MethodHandle {
    pub unsafe fn new(
        ptr: ABIHandlePtr,
        context: ExecutionContext,
        method: Arc<Method>,
        class: Arc<Class>,
        class_loader: u32,
    ) -> Self {
        Self {
            ptr,
            context,
            class,
            method,
            class_loader,
        }
    }
}

pub fn debug_object(object: &Object, thread: &mut Thread) -> String {
    let jvm = thread.jvm.clone();
    
    if jvm.environment.is_null(object) {
        return "null".into();
    }
    
    let to_string = thread.jvm.get_method("java/lang/Object", "toString", "()Ljava/lang/String;", 0);
    
    let to_string_res = thread.invoke(&to_string, Box::new([
        jvm.environment.clone_reference(object).consume()
    ])).unwrap();
    
    let object_string = jvm.get_string(&jvm.environment.object_from_operand(to_string_res));
    
    let mut out = String::new();
    let object_class = thread.jvm.environment.get_object_class(object);

    if &*object_class.this_class == "java/lang/String" {
        return jvm.get_string(object);
    }
    
    let mut fmt = Formatter::new(&mut out, FormattingOptions::new());

    let mut struct_debug = fmt.debug_struct(&object_string);

    for field in object_class.fields.iter().filter(|field| !field.access_flags.contains(AccessFlags::STATIC)) {
        let value = jvm.environment.get_object_field(object, object_class, &field.name, &field.descriptor_string);

        match &field.descriptor {
            FieldType::Long => {
                struct_debug.field(&field.name, &(value.data() as i64));
            }
            FieldType::Short => {
                struct_debug.field(&field.name, &(value.data() as i16));
            }
            FieldType::Double => {
                struct_debug.field(&field.name, &f64::from_bits(value.data()));
            }
            FieldType::Int => {
                struct_debug.field(&field.name, &(value.data() as i32));
            }
            FieldType::Byte => {
                struct_debug.field(&field.name, &(value.data() as u8));
            }
            FieldType::Char => {
                struct_debug.field(&field.name, &(value.data() as u16));
            }
            FieldType::Float => {
                struct_debug.field(&field.name, &f32::from_bits(value.data() as u32));
            }
            FieldType::Boolean => {
                struct_debug.field(&field.name, &(value.data() == 1));
            }
            FieldType::Array { .. }
            | FieldType::Class(_) => {
                let obj = jvm.environment.object_from_operand(value);
                if jvm.environment.is_null(&obj) {
                    struct_debug.field_with(&field.name, |fmt| write!(fmt, "null"));
                } else {
                    let obj_class = jvm.environment.get_object_class(&obj);
                    
                    if &*obj_class.this_class == "java/lang/String" {
                        struct_debug.field(&field.name, &jvm.get_string(&obj));
                    } else {
                        let to_string_res = thread.invoke(&to_string, Box::new([
                            jvm.environment.clone_reference(&obj).consume()
                        ])).unwrap();

                        let object_string = jvm.get_string(&jvm.environment.object_from_operand(to_string_res));
                        
                        struct_debug.field_with(&field.name, move |fmt| {
                            write!(fmt, "{object_string}")
                        });
                    }
                }
            }
        }
    }
    
    struct_debug.finish().unwrap();
    
    drop(fmt);
    out
}
