use crate::classfile::resolved::{AccessFlags, Class, Field, FieldType};
use crate::JVM;
use bitflags::Flags;
use parking_lot::RwLock;
use std::alloc::{alloc, Layout};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::mem::{align_of, size_of};

use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU8};
use mutf8::utf8_to_mutf8;
use crate::thread::Operand;

pub unsafe trait ObjectInternal {}
pub unsafe trait NonArrayObject {}

#[derive(Clone, Debug, PartialEq)]
#[repr(transparent)]
pub struct Object {
    pub(crate) ptr: *mut (),
}

//May seem weird but it's convenient for representing Object[] as *mut RawObject<RawArray<Object>>
unsafe impl ObjectInternal for Object {}

impl Object {
    pub const NULL: Self = Self { ptr: std::ptr::null_mut() };

    pub unsafe fn from_raw<T: ?Sized>(ptr: *mut RawObject<T>) -> Self {
        Self {
            ptr: ptr.to_raw_parts().0,
        }
    }

    pub fn get_body(&self) -> *mut () {
        let layout = Layout::from_size_align(size_of::<ObjectHeader>(), align_of::<ObjectHeader>()).unwrap();
        let padding = layout.padding_needed_for(align_of::<Operand>());

        unsafe { (self.ptr.byte_offset(padding as isize)) }
    }

    pub fn get_raw(&self) -> *mut () {
        self.ptr as usize as *mut ()
    }

    pub fn cast_class<T: NonArrayObject>(&self) -> *mut RawObject<T> {
        std::ptr::from_raw_parts_mut(self.ptr, ())
    }


    ///SAFETY: The pointer this [Object] represents must still be valid
    pub unsafe fn get_header(&self) -> &ObjectHeader {
        assert_ne!(self, &Self::NULL);

        let this = self.cast_class::<()>();
        unsafe { &(*this).header }
    }

    pub unsafe fn cast_array<T: ObjectInternal>(&self) -> *mut RawObject<RawArray<T>> {
        //TODO type checking

        #[repr(C)]
       struct PartialArray {
           header: ObjectHeader,
           length: i32
       }

        unsafe {
            let partial = self.ptr as *const PartialArray;
            let length = unsafe { (*partial).length };

            std::ptr::from_raw_parts_mut(self.ptr, length as usize)
        }
    }
}



#[derive(Debug)]
#[repr(C)]
pub struct ObjectHeader {
    pub type_: ObjectType,
    pub synchronized: AtomicU8
}

#[derive(Debug)]
#[repr(C)]
pub enum ObjectType {
    Class(*const Class),
    Array(*const FieldType)
}

impl ObjectType {

    pub unsafe fn get_class(&self) -> Option<&Class> {
        match self {
            Self::Class(class) => Some(&**class),
            _ => None
        }
    }

    pub unsafe fn get_type(&self) -> Option<&FieldType> {
        match self {
            Self::Array(array) => Some(&**array),
            _ => None
        }
    }

}

#[derive(Debug)]
#[repr(C)]
pub struct RawObject<T: ?Sized> {
    pub header: ObjectHeader,
    pub body: T
}

macro_rules! impl_primitive {
    ($a:ty, $b:expr, $c:expr) => {
        unsafe impl ObjectInternal for $a {}
        impl AsJavaPrimitive for $a {
            fn get(&self) -> JavaPrimitive {
                $b
            }

            fn field_type() -> FieldType {
                $c
            }
        }
        unsafe impl NonArrayObject for $a {}
    };
}

unsafe impl ObjectInternal for () {}
unsafe impl NonArrayObject for () {}
unsafe impl ObjectInternal for u64 {}

impl_primitive!(i8, JavaPrimitive::Byte, FieldType::Byte);
impl_primitive!(i16, JavaPrimitive::Short, FieldType::Short);
impl_primitive!(u16, JavaPrimitive::Char, FieldType::Char);
impl_primitive!(i32, JavaPrimitive::Int, FieldType::Int);
impl_primitive!(i64, JavaPrimitive::Long, FieldType::Long);
impl_primitive!(f32, JavaPrimitive::Float, FieldType::Float);
impl_primitive!(f64, JavaPrimitive::Double, FieldType::Double);

pub type Byte = i8;
pub type Short = i16;
pub type Char = u16;
pub type Int = i32;
pub type Long = i64;
pub type Float = f32;
pub type Double = f64;

pub struct Heap {
    //Unused until GC is implemented
    pub data: *mut [u8],
    pub strings: RwLock<HashMap<String, StringObject>>,
    pub types: RwLock<HashSet<Box<FieldType>>>,
}

impl Heap {
    pub fn new() -> Self {
        let box_ = Vec::<u8>::with_capacity(1024).into_boxed_slice();
        let ptr = Box::into_raw(box_);

        Self {
            data: ptr,
            strings: RwLock::new(HashMap::new()),
            types: RwLock::new(HashSet::new()),
        }
    }

    fn recurse_get_heap_size(class: &Class) -> usize {
        let mut size = class
            .fields
            .iter()
            .map(|field| (!field.access_flags.contains(AccessFlags::STATIC)) as usize)
            .sum::<usize>()
            * size_of::<Operand>();
        match &class.super_class {
            None => {}
            Some(superclass) => size += Self::recurse_get_heap_size(superclass),
        }

        size
    }

    ///Safety: Class is instantiated only by the JVM, and it contains an Arc to its instantiating [ClassLoader],
    /// which means Class will live for as long as the ClassLoader.
    /// T must be ObjectInternal, which is a type guard to imply that the unsized field T in the DST [RawObject] has a constant length of 1
    pub fn allocate_raw_object<T: ObjectInternal + NonArrayObject>(
        &self,
        class: &Class,
    ) -> *mut RawObject<T> {
        let object_header = Layout::from_size_align(size_of::<RawObject<T>>(), align_of::<RawObject<T>>()).unwrap();

        let object_body = Layout::from_size_align(class.heap_size, align_of::<u64>()).unwrap();
        let layout = object_header.extend(object_body).unwrap().0;

        let alloc = unsafe { alloc(layout) };

        let object_ptr: *mut RawObject<T> = std::ptr::from_raw_parts_mut(alloc as *mut (), ());

        unsafe {
            (*object_ptr).header = ObjectHeader {
                type_: ObjectType::Class(class as *const Class),
                synchronized: AtomicU8::new(0),
            };
        }

        object_ptr
    }

    ///Safety: Class is instantiated only by the JVM, and it contains an Arc to its instantiating [ClassLoader],
    /// which means Class will live for as long as the ClassLoader.
    pub fn allocate_raw_object_array(
        &self,
        class: &Class,
        length: i32,
    ) -> *mut RawObject<RawArray<Object>> {

        let object_header = Layout::from_size_align(size_of::<RawObject<()>>(), align_of::<RawObject<()>>()).unwrap();

        let length_header = Layout::from_size_align(4, 4).unwrap();
        let array_body = Layout::array::<Object>(length as usize).unwrap();

        let layout = object_header.extend(length_header).unwrap().0.extend(array_body).unwrap().0;

        let alloc = unsafe { alloc(layout) };

        let object_ptr: *mut RawObject<RawArray<Object>> =
            std::ptr::from_raw_parts_mut(alloc as *mut (), length as usize);

        let mut types = self.types.write();
        let field_type = FieldType::Array {
            type_: Box::new(FieldType::Class(class.this_class.to_string())),
            dimensions: 1,
        };

        if !types.contains(&field_type) {
            types.insert(Box::new(field_type.clone()));
        }

        let field_type = &**types.get(&field_type).unwrap() as *const FieldType;

        unsafe {
            (*object_ptr).body.length = length;
            (*object_ptr).header = ObjectHeader {
                type_: ObjectType::Array(field_type),
                synchronized: AtomicU8::new(0)
            };
        }

        object_ptr
    }

    ///Safety: Class is instantiated only by the JVM, and it contains an Arc to its instantiating [ClassLoader],
    /// which means Class will live for as long as the ClassLoader.
    pub fn allocate_raw_primitive_array<T: AsJavaPrimitive + ObjectInternal + Default>(
        &self,
        length: i32,
    ) -> *mut RawObject<RawArray<T>> {
        let padding = size_of::<(i32, T)>() - size_of::<T>();

        let size = size_of::<ObjectHeader>() + size_of::<i32>() + padding + (size_of::<T>() * length as usize);

        let layout = Layout::from_size_align(size, align_of::<u64>()).unwrap();

        assert_ne!(size, 0);
        let alloc = unsafe { alloc(layout) };

        let object_ptr: *mut RawObject<RawArray<T>> =
            std::ptr::from_raw_parts_mut(alloc as *mut (), length as usize);

        let mut types = self.types.write();
        let field_type = T::field_type();

        if !types.contains(&field_type) {
            types.insert(Box::new(field_type.clone()));
        }

        let field_type = &**types.get(&field_type).unwrap() as *const FieldType;

        unsafe {
            (*object_ptr).header = ObjectHeader {
                type_: ObjectType::Array(field_type),
                synchronized: AtomicU8::new(0),
            };
            (*object_ptr).body.length = length;
        }

        object_ptr
    }

    pub fn allocate_class(&self, class: &Class) -> Object {
        let ptr = self.allocate_raw_object::<()>(class);
        unsafe { Object::from_raw(ptr) }
    }

    ///SAFETY: the [Field] type must belong to either the [Class] that represents the [Object], or one of its child classes
    pub unsafe fn set_class_field<T: Copy>(&self, object: &Object, field: &Field, value: T) {
        let ptr = unsafe { (object.ptr as *mut u8).offset(field.heap_offset as isize) };

        unsafe {
            *(ptr as *mut T) = value;
        }
    }

    ///SAFETY: the [Field] type must belong to either the [Class] that represents the [Object], or one of its child classes
    pub unsafe fn get_class_field<T: Copy>(&self, object: &Object, field: &Field) -> T {
        let ptr = unsafe { (object.get_body() as *mut u8).offset(field.heap_offset as isize) };

        unsafe {
            *(ptr as *mut T)
        }
    }

    pub fn allocate_string(&self, string: &str, jvm: &JVM) -> StringObject {
        let class_loaders = jvm.class_loaders.read();
        let bootstrapper = &class_loaders[0];

        let class_loader = bootstrapper.loader.clone();
        drop(class_loaders);

        let string_class = jvm.find_class("java/lang/String", class_loader).unwrap();

        let cesu = utf8_to_mutf8(string.as_bytes()).unwrap();
        let chars: Vec<u16> = cesu.iter().map(|b| *b as u16).collect();

        let string = self.allocate_class(&string_class);

        let char_array = self.allocate_raw_primitive_array::<u16>(chars.len() as i32);
        unsafe {
            (*char_array).body.body.copy_from_slice(&chars);
            (*string.cast_class::<RawString>()).body.value = Object::from_raw(char_array);
        }

        StringObject {
            value: string,
            class: string_class.clone(),
        }
    }

    pub fn get_string(&self, string: &str, jvm: &JVM) -> StringObject {
        let strings = self.strings.read();
        if strings.contains_key(string) {
            strings.get(string).unwrap().clone()
        } else {
            drop(strings);
            let string_object = self.allocate_string(string, jvm);

            let mut strings = self.strings.write();
            strings.insert(string.into(), string_object.clone());
            string_object
        }
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        unsafe {
            drop(Box::from_raw(self.data));
        }
    }
}

///Raw representation of an array on the heap. Due to how type-casting works in Java,
/// this will always be encapsulated by a [RawObject], however if this array is a primitive array
/// and not a reference array, the [RawObject] will have it's class field set to 0
#[derive(Debug)]
#[repr(C)]
pub struct RawArray<T: ObjectInternal> {
    pub length: i32,
    pub body: [T],
}

#[repr(C, align(8))]
pub struct RawString {
    pub value: Object,
}

unsafe impl NonArrayObject for RawString {}

#[derive(Clone, Debug)]
#[repr(C)]
pub struct StringObject {
    pub value: Object,
    pub class: Arc<Class>,
}

impl StringObject {
    pub fn get_string(&self) -> String {
        let raw_string = self.value.cast_class::<RawString>();
        let array = unsafe { &(*(*raw_string).body.value.cast_array::<u16>()).body.body };

        let bytes: Vec<u8> = array.iter().map(|char| *char as u8).collect();

        String::from_utf8(mutf8::mutf8_to_utf8(&bytes).unwrap().into()).unwrap()
    }
}

unsafe impl ObjectInternal for RawString {}

pub enum JavaPrimitive {
    Byte,
    Short,
    Char,
    Int,
    Long,
    Double,
    Float,
}

pub trait AsJavaPrimitive: Default {
    fn get(&self) -> JavaPrimitive;

    fn field_type() -> FieldType
    where
        Self: Sized;
}
