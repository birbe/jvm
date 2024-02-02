use std::sync::Arc;
use crate::classfile::resolved::{Class, Method, Ref};
use crate::env::{Environment, Object};
use crate::execution::MethodHandle;
use crate::linker::ClassLoader;
use crate::thread::{Operand, Thread};

pub mod heap;

struct JVMBinding;

struct NativeEnvironment {
}

impl Environment for NativeEnvironment {
    fn link_class(&self, class: Arc<Class>) {
        todo!()
    }

    fn invoke_handle(&self, thread: &mut Thread, method_handle: &MethodHandle, args: Box<[Operand]>) -> Option<Operand> {
        todo!()
    }

    fn create_method_handle(&self, class_loader: &dyn ClassLoader, ref_: Arc<Ref>, method: Arc<Method>, class: Arc<Class>) -> MethodHandle {
        todo!()
    }

    fn get_object_class<'a, 'b>(&'a self, object: &'b Object) -> &'b Class {
        todo!()
    }

    fn get_object_field(&self, object: Object, class: &Class, field: &Ref) -> Operand {
        todo!()
    }

    fn set_object_field(&self, object: Object, class: &Class, field: &Ref, value: Operand) {
        todo!()
    }

    fn allocate_object_array(&self, class: &Class, size: i32) -> Object {
        todo!()
    }

    fn get_array_element(&self, array: Object, index: i32) -> Operand {
        todo!()
    }

    fn set_object_array_element(&self, array: Object, index: i32, value: Operand) -> Operand {
        todo!()
    }

    fn new_object(&self, class: &Class) -> Object {
        todo!()
    }

    unsafe fn object_from_operand(&self, operand: &Operand) -> Object {
        todo!()
    }
}