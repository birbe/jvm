use crate::classfile::resolved::Ref;
use crate::execution::ABIHandlePtr;
use crate::heap::{Object, RawArray, StringObject};
use crate::heap::{ObjectInternal, RawObject};
use crate::thread::{FrameStack, Thread};

unsafe extern "C" fn println(frame_stack: *mut FrameStack, thread: *mut Thread) -> u64 {
    let frame_stack = unsafe { &mut *frame_stack };
    let frame = unsafe { (*frame_stack.get_first()).as_frame() };

    let object = Object {
        ptr: frame.locals[0]
    };

    let string_object = StringObject { value: object };

    println!("{}", string_object.get_string());

    0
}

pub fn link(method_ref: &Ref) -> Option<ABIHandlePtr> {
    match (&method_ref.class[..], &method_ref.name_and_type.name[..], &method_ref.name_and_type.descriptor[..]) {
        ("Main", "println", "(Ljava/lang/String;)V") => {
            Some(println)
        }
        _ => None
    }
}