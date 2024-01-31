use crate::classfile::resolved::{Class, Method, Ref};
use crate::execution::MethodHandle;
use crate::linker::ClassLoader;
use crate::thread::{Operand, Thread};
use std::sync::Arc;
use std::alloc::Layout;
use std::mem::{align_of, size_of};
use crate::env::native::heap::{NonArrayObject, ObjectHeader, ObjectInternal, RawArray, RawObject};

pub mod aot;
pub mod interpreter;
pub mod native;
pub mod wasm;

pub trait Environment {
    fn link_class(&self, class: Arc<Class>);

    fn invoke_handle(
        &self,
        thread: &mut Thread,
        method_handle: &MethodHandle,
        args: Box<[Operand]>,
    ) -> Option<Operand>;

    fn create_method_handle(
        &self,
        class_loader: &dyn ClassLoader,
        ref_: Arc<Ref>,
        method: Arc<Method>,
        class: Arc<Class>,
    ) -> MethodHandle;

    fn get_object_class<'a, 'b>(&'a self, object: &'b Object) -> &'b Class;

    fn get_object_field(&self, object: Object, class: &Class, field: &Ref) -> Operand;

    fn set_object_field(&self, object: Object, class: &Class, field: &Ref, value: Operand);

    fn allocate_object_array(&self, class: &Class, size: i32) -> Object;

    fn get_array_element(&self, array: Object, index: i32) -> Operand;

    fn set_object_array_element(&self, array: Object, index: i32, value: Operand) -> Operand;

    fn new_object(&self, class: &Class) -> Object;

    unsafe fn object_from_operand(&self, operand: &Operand) -> Object;

}

pub trait Compiler {}

#[derive(Debug, PartialEq)]
pub struct Object {
    pub(crate) ptr: *mut (),
    pub(crate) drop: Option<extern "C" fn(*mut ())>
}

impl Drop for Object {
    fn drop(&mut self) {
        if let Some(drop) = self.drop {
            drop(self.ptr);
        }
    }
}

impl Object {
    pub const NULL: Self = Self {
        ptr: std::ptr::null_mut(),
        drop: None,
    };

    pub unsafe fn from_raw(ptr: *mut (), drop: Option<extern "C" fn(i32)>) -> Self {
        Self {
            ptr: ptr.to_raw_parts().0,
            drop: None,
        }
    }

}
