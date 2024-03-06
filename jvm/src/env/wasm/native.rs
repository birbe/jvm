use bitflags::Flags;
use crate::classfile::resolved::AccessFlags;
use crate::execution::MethodHandle;
use crate::thread::{Operand, Thread};

pub fn native_func(method_handle: &MethodHandle, thread: &mut Thread) -> Operand {
    let jvm = thread.jvm.clone();
    let class_loader = thread.class_loader.clone();

    match (&method_handle.class.this_class[..], &method_handle.method.name[..], &method_handle.method.descriptor.string[..]) {
        ("java/lang/Object", "registerNatives", "()V")
        | ("java/security/AccessController", "doPrivileged", _ )=> {
            Operand {
                data: 0
            }
        },
        ("java/lang/System", "registerNatives", "()V") => {
            let method_handle = thread.jvm.get_method("java/lang/System", "initializeSystemClass", "()V", 0);

            thread.invoke(&method_handle, Box::new([]));

            Operand {
                data: 0
            }
        }
        ("java/lang/Class", "registerNatives", "()V") => {
            Operand {
                data: 0
            }
        }
        _ => unimplemented!("{} {:?}", method_handle.class.this_class, method_handle.method)
    }

}