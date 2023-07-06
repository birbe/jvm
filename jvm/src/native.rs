use crate::classfile::resolved::Ref;
use crate::execution::ABIHandlePtr;
use crate::heap::{Object, RawArray};
use crate::heap::{ObjectInternal, RawObject};

pub fn link(method_ref: &Ref) -> Option<ABIHandlePtr> {
    match (&method_ref.class, &method_ref.name_and_type.name, &method_ref.name_and_type.descriptor) {
        _ => None
    }
}