use crate::classfile::resolved::Ref;
use crate::execution::ABIHandlePtr;
use crate::{Array, Object, ObjectInternal};

#[repr(C)]
pub struct StringInternal {
    pub value: Object<Array<i16>>
}

impl ObjectInternal for StringInternal {}

pub fn link(method_ref: &Ref) -> Option<ABIHandlePtr> {
    match (&method_ref.class, &method_ref.name_and_type.name, &method_ref.name_and_type.descriptor) {
        _ => None
    }
}