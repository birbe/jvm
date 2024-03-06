use crate::classfile::resolved::{Class, Method, Ref};
use crate::execution::MethodHandle;
use crate::linker::{ClassLoader, ClassLoaderObject};
use crate::thread::{Operand, Thread};
use std::sync::Arc;

pub mod interpreter;
#[cfg(not(target_arch = "wasm32"))]
pub mod native;
#[cfg(target_arch = "wasm32")]
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
        ref_: Arc<Ref>,
        method: Arc<Method>,
        class: Arc<Class>,
        class_loader: &dyn ClassLoader
    ) -> MethodHandle;

    fn new_string(&self, contents: &str, thread: &mut Thread) -> Object;

    fn get_object_class<'a, 'b>(&'a self, object: &'b Object) -> &'b Class;

    unsafe fn get_object_field(&self, object: &Object, class: &Class, field_name: &str, field_descriptor: &str) -> Operand;

    unsafe fn set_object_field(&self, object: &Object, class: &Class, field_name: &str, field_descriptor: &str, value: Operand);

    fn new_object_array(&self, class: &Class, size: i32) -> Object;

    fn new_array(&self, type_: i32, size: i32) -> Object;

    fn get_array_length(&self, array: &Object) -> i32;

    fn set_array_element(&self, array_type: u8, array: &Object, index: i32, value: Operand);

    fn get_array_element(&self, array: &Object, index: i32) -> Operand;

    fn set_object_array_element(&self, array: &Object, index: i32, value: Operand);

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

    pub unsafe fn from_raw(ptr: *mut (), drop: Option<extern "C" fn(*mut ())>) -> Self {
        Self {
            ptr,
            drop,
        }
    }

}
