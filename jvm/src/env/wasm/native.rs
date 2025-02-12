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
        },
        ("java/lang/Class", "getPrimitiveClass", "(Ljava/lang/String;)Ljava/lang/Class;") => {
            // let mut frame = unsafe { &mut *thread.frame_stack.get_top() };
            // let frame = frame.as_frame();

            Operand {
                data: 0
            }
        }
        ("WASMClassLoader", "fetch", "(Ljava/lang/String;)[B") => {
            let byte_array = jvm.environment.new_array(8, 0);

            Operand {
                objectref: byte_array.ptr
            }
        }
        ("java/lang/ClassLoader", "registerNatives", "()V") => {
            Operand {
                data: 0
            }
        }
        // ("java/lang/System", "initProperties")
        _ => {
            // unimplemented!("{} {:?}", method_handle.class.this_class, method_handle.method)
            console_log!("warning: {} {} unimplemented, returning null", method_handle.class.this_class, method_handle.method.name);
            Operand {
                data: 0
            }
        }
    }

}