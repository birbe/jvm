use crate::classfile::resolved::Ref;
use crate::execution::ABIHandlePtr;
use crate::heap::{Object, StringObject};

use crate::thread::{FrameStack, Operand, Thread};
use std::io::Write;


extern "C" fn println(frame_stack: &mut FrameStack, thread: &mut Thread) -> Operand {
    let frame_stack = unsafe { &mut *frame_stack };
    let frame = unsafe { &*frame_stack.get_top() };

    let string_class = unsafe {
        let loaders = (*thread).jvm.class_loaders.read();
        let bootstrapper = &loaders[0];
        bootstrapper.loader.get_class("java/lang/String").unwrap()
    };

    let object = Object {
        ptr: unsafe { (*frame.locals).objectref },
    };

    let string_object = StringObject {
        value: object,
        class: string_class,
    };

    let mut stdout = unsafe { (*thread).jvm.stdout.lock() };
    stdout
        .write(format!("{}\n", string_object.get_string()).as_bytes())
        .unwrap();

    Operand { data: 0 }
}

#[allow(non_snake_case)]
extern "C" fn java_lang_System_registerNatives(
    _frame_stack: &mut FrameStack,
    _thread: &mut Thread,
) -> Operand {
    Operand { data: 0 }
}

#[allow(non_snake_case)]
extern "C" fn java_lang_Object_registerNatives(
    _frame_stack: *mut FrameStack,
    _thread: *mut Thread,
) -> Operand {
    Operand { data: 0 }
}

pub fn link(method_ref: &Ref) -> Option<ABIHandlePtr> {
    match (
        &method_ref.class[..],
        &method_ref.name_and_type.name[..],
        &method_ref.name_and_type.descriptor[..],
    ) {
        ("Main", "println", "(Ljava/lang/String;)V") => Some(println),
        ("java/lang/System", "registerNatives", "()V") => Some(java_lang_System_registerNatives),
        ("java/lang/Object", "registerNatives", "()V") => Some(java_lang_System_registerNatives),
        _ => None,
    }
}
