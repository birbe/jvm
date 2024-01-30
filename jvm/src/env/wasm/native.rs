use crate::execution::MethodHandle;
use crate::thread::Operand;

pub fn native_func(method_handle: &MethodHandle) -> Operand {

    match (&method_handle.class.this_class[..], &method_handle.method.name[..], &method_handle.method.descriptor.string[..]) {
        ("java/lang/Object", "registerNatives", "()V") => {
            Operand {
                data: 0
            }
        }
        _ => unimplemented!("{} {:?}", method_handle.class.this_class, method_handle.method)
    }

}