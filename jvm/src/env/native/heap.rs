use crate::classfile::resolved::{AccessFlags, Class, Field, FieldType};
use crate::JVM;
use bitflags::Flags;
use parking_lot::RwLock;
use std::alloc::{alloc, Layout};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::mem::{align_of, size_of};

use crate::thread::Operand;
use std::sync::atomic::AtomicU8;
use std::sync::Arc;
use wasm_bindgen::intern;
use crate::env::Object;

pub unsafe trait ObjectInternal {}
pub unsafe trait NonArrayObject {}

#[derive(Debug)]
#[repr(C)]
pub struct ObjectHeader {
    pub type_: ObjectType,
    pub synchronized: AtomicU8,
}

#[derive(Debug)]
#[repr(C)]
pub enum ObjectType {
    Class(*const Class),
    Array(*const FieldType),
}

impl ObjectType {
    pub unsafe fn get_class(&self) -> Option<&Class> {
        match self {
            Self::Class(class) => Some(&**class),
            _ => None,
        }
    }

    pub unsafe fn get_type(&self) -> Option<&FieldType> {
        match self {
            Self::Array(array) => Some(&**array),
            _ => None,
        }
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct RawObject<T: ?Sized> {
    pub header: ObjectHeader,
    pub body: T,
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
