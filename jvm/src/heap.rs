use std::alloc::{alloc, Layout};
use parking_lot::RwLock;
use std::collections::{HashMap, HashSet};
use std::mem::{align_of, size_of};
use std::ptr::Pointee;
use cesu8str::Cesu8String;
use crate::classfile::resolved::{Class, FieldType};
use crate::JVM;

pub unsafe trait ObjectInternal {}
pub unsafe trait NonArrayObject {}

#[derive(Debug)]
#[repr(transparent)]
pub struct Object {
    ptr: u64
}

impl Object {

    pub const NULL: Self = Self {
        ptr: 0
    };

    pub unsafe fn from_raw<T: ?Sized>(ptr: *mut RawObject<T>) -> Self {
        Self {
            ptr: ptr.to_raw_parts().0 as usize as u64
        }
    }

    pub fn get_raw(&self) -> *mut () {
        self.ptr as usize as *mut ()
    }

    pub fn cast_class<T: NonArrayObject>(&self) -> *mut RawObject<T> {
        std::ptr::from_raw_parts_mut(self.ptr as usize as *mut (), ())
    }

    pub unsafe fn cast_array<T: ObjectInternal>(&self) -> *mut RawObject<RawArray<T>> {
        //TODO type checking

        unsafe {
            let ptr = self.ptr as usize as *mut ();
            let length = *ptr.cast_const().cast::<u8>().add(size_of::<*const FieldType>()).cast::<i32>();
            std::ptr::from_raw_parts_mut(ptr, length as usize)
        }
    }

}

#[derive(Debug)]
#[repr(C)]
pub struct RawObject<T: ?Sized> {
    pub descriptor: *const FieldType,
    pub body: T
}

macro_rules! impl_primitive {

    ($a:ty, $b:expr, $c:expr) => {
        unsafe impl ObjectInternal for $a {}
        impl AsJavaPrimitive for $a {
            fn get(&self) -> JavaPrimitive {
                $b
            }

            fn as_field_type(&self) -> FieldType {
                $c
            }
        }
        unsafe impl NonArrayObject for $a {}
    }

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

pub struct Heap {
    //Unused until GC is implemented
    pub data: *mut [u8],
    pub strings: RwLock<HashMap<String, *const RawObject<StringObject>>>,
    pub types: RwLock<HashSet<Box<FieldType>>>
}

impl Heap {

    pub fn new() -> Self {
        let mut box_ = Vec::<u8>::with_capacity(1024).into_boxed_slice();
        let ptr = Box::into_raw(box_);

        Self {
            data: ptr,
            strings: RwLock::new(HashMap::new()),
            types: RwLock::new(HashSet::new()),
        }
    }

    fn recurse_get_heap_size(class: &Class) -> usize {
        let mut size = class.fields.len() * size_of::<u64>();
        match &class.super_class {
            None => {}
            Some(superclass) => size += Self::recurse_get_heap_size(superclass)
        }

        size
    }

    ///Safety: Class is instantiated only by the JVM, and it contains an Arc to its instantiating [ClassLoader],
    /// which means Class will live for as long as the ClassLoader.
    /// T must be ObjectInternal, which is a type guard to imply that the unsized field T in the DST [RawObject] has a constant length of 1
    pub fn allocate_raw_object<T: ObjectInternal + NonArrayObject>(&self, class: &Class) -> *mut RawObject<T>
    {
        let size = size_of::<RawObject<T>>() + Self::recurse_get_heap_size(class);

        let mut layout = Layout::from_size_align(size, 8).unwrap();
        let mut alloc = unsafe { alloc(layout) };

        let mut types = self.types.write();
        let field_type = FieldType::Class(class.this_class.to_string());

        if !types.contains(&field_type) {
            types.insert(Box::new(field_type.clone()));
        }

        let field_type = &**types.get(&field_type).unwrap() as *const FieldType;

        let object_ptr: *mut RawObject<T> = std::ptr::from_raw_parts_mut(alloc as *mut (), ());

        unsafe {
            (*object_ptr).descriptor = field_type;
        }
        print!("1");

        object_ptr
    }

    ///Safety: Class is instantiated only by the JVM, and it contains an Arc to its instantiating [ClassLoader],
    /// which means Class will live for as long as the ClassLoader.
    pub fn allocate_raw_object_array(&self, class: &Class, length: i32) -> *mut RawObject<RawArray<u64>> {
        let size = size_of::<u64>() + size_of::<i32>() + (size_of::<u64>() * length as usize);

        let mut layout = Layout::from_size_align(size, 8).unwrap();
        let mut alloc = unsafe { alloc(layout) };

        let object_ptr: *mut RawObject<RawArray<u64>> = std::ptr::from_raw_parts_mut(alloc as *mut (), length as usize);
        
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
            (*object_ptr).descriptor = field_type;
        }

        object_ptr
    }

    ///Safety: Class is instantiated only by the JVM, and it contains an Arc to its instantiating [ClassLoader],
    /// which means Class will live for as long as the ClassLoader.
    pub fn allocate_raw_primitive_array<T: AsJavaPrimitive + Default>(&self, length: i32) -> *mut RawObject<RawArray<T>> {
        let dummy = T::default();

        let size = size_of::<u64>() + size_of::<i32>() + (size_of::<T>() * length as usize);
        let mut layout = Layout::from_size_align(size, align_of::<u64>()).unwrap();
        let mut alloc = unsafe { alloc(layout) };

        let object_ptr: *mut RawObject<RawArray<T>> = std::ptr::from_raw_parts_mut(alloc as *mut (), length as usize);

        let mut types = self.types.write();
        let field_type = dummy.as_field_type();

        if !types.contains(&field_type) {
            types.insert(Box::new(field_type.clone()));
        }

        let field_type = &**types.get(&field_type).unwrap() as *const FieldType;

        unsafe {
            (*object_ptr).descriptor = field_type;
            (*object_ptr).body.length = length;
        }

        object_ptr
    }

    pub fn allocate_class(&self, class: &Class) -> Object {
        let ptr = self.allocate_raw_object::<()>(class);
        unsafe { Object::from_raw(ptr) }
    }

    pub fn allocate_string(&self, string: &str, jvm: &JVM) -> StringObject {
        let class_loaders = jvm.class_loaders.read();
        let bootstrapper = &class_loaders[0];
        let string_class = jvm.find_class("java/lang/String", bootstrapper.loader.clone()).unwrap();

        let cesu = Cesu8String::from(string);
        let chars: Vec<u16> = cesu.into_bytes().iter().map(|b| *b as u16).collect();

        let string = self.allocate_class(&string_class);
        let char_array = self.allocate_raw_primitive_array::<u16>(chars.len() as i32);
        unsafe {
            (*char_array).body.body.copy_from_slice(&chars);
            (*string.cast_class::<RawString>()).body.value = Object::from_raw(char_array);
        }

        StringObject {
            value: string,
        }
    }

    // pub fn get_string(&self, string: &str) -> *mut RawObject<StringObject> {
    //
    // }
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
    pub body: [T]
}

#[repr(C, align(8))]
pub struct RawString {
    pub value: Object,
}

unsafe impl NonArrayObject for RawString {}

#[derive(Debug)]
#[repr(C)]
pub struct StringObject {
    pub value: Object
}

unsafe impl ObjectInternal for RawString {}

pub enum JavaPrimitive {
    Byte,
    Short,
    Char,
    Int,
    Long,
    Double,
    Float
}

pub trait AsJavaPrimitive: Default + ObjectInternal {

    fn get(&self) -> JavaPrimitive;

    fn as_field_type(&self) -> FieldType;

}