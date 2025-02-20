use std::any::Any;
use std::collections::HashMap;
use crate::classfile::resolved::{Class, Method, Ref};
use crate::execution::MethodHandle;
use crate::linker::ClassLoader;
use crate::thread::{Operand, Thread};
use std::sync::Arc;
use crate::bytecode::ArrayType;

#[cfg(not(target_arch = "wasm32"))]
pub mod native;
#[cfg(target_arch = "wasm32")]
pub mod wasm;

trait ToAny: 'static {
    
    fn to_any(&self) -> &dyn Any;
    
}

impl<T: 'static> ToAny for T {
    fn to_any(&self) -> &dyn Any {
        self
    }
}

pub trait Environment: ToAny {

    fn get_object_hash(&self, object: &Object) -> i32;
    
    fn store_primitive_classes(&self, primitives: HashMap<ArrayType, Arc<Class>>);

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

    fn get_object_class<'a, 'b>(&'a self, object: &'b Object) -> &'b Class;

    unsafe fn get_object_field(&self, object: &Object, class: &Class, field_name: &str, field_descriptor: &str) -> Operand;

    unsafe fn set_object_field(&self, object: &Object, class: &Class, field_name: &str, field_descriptor: &str, value: Operand);

    fn new_object_array(&self, class: &Class, size: i32) -> Object;

    fn clone_reference(&self, object: &Object) -> Object;

    fn is_null(&self, object: &Object) -> bool;
    
    fn new_array(&self, type_: ArrayType, size: i32) -> Object;

    fn get_array_length(&self, array: &Object) -> i32;

    unsafe fn set_array_element(&self, array_type: ArrayType, array: &Object, index: i32, value: Operand);

    unsafe fn get_array_element(&self, array_type: ArrayType, array: &Object, index: i32) -> Operand;

    unsafe fn set_object_array_element(&self, array: &Object, index: i32, value: Operand);

    unsafe fn get_object_array_element(&self, array: &Object, index: i32) -> Object;

    fn new_object(&self, class: &Class) -> Object;

    unsafe fn object_from_operand(&self, operand: Operand) -> Object;
    unsafe fn object_ref_from_operand(&self, operand: &Operand) -> &Object;
    
    fn object_ref_eq(&self, a: &Object, b: &Object) -> bool;

}

pub trait Compiler {}

#[repr(C)]
#[derive(Debug)]
pub struct Object {
    pub ptr: *mut (),
    padding: [u64; 0],
    drop: extern "C" fn(*mut ())
}

impl Into<Operand> for Object {
    fn into(self) -> Operand {
        self.consume()
    }
}

impl Into<Operand> for i32 {
    fn into(self) -> Operand {
        Operand::new_data(self as u32)
    }
}

impl Into<Operand> for u32 {
    fn into(self) -> Operand {
        Operand::new_data(self)
    }
}

impl Drop for Object {
    fn drop(&mut self) {
        (self.drop)(self.ptr)
    }
}

pub(crate) extern "C" fn nothing(_: *mut ()) {}

impl Object {
    pub const NULL: Self = Self {
        ptr: std::ptr::null_mut(),
        padding: [],
        drop: nothing,
    };

    pub fn consume(self) -> Operand {
        let ptr = self.ptr;
        
        std::mem::forget(self);
        
        Operand::new_ref(ptr)
    }
    
    pub unsafe fn from_raw(ptr: *mut (), drop: extern "C" fn(*mut ())) -> Self {
        Self {
            ptr,
            padding: [],
            drop,
        }
    }

}
