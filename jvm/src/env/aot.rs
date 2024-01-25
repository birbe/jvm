use std::collections::HashSet;
use std::sync::Arc;
use crate::classfile::resolved::{Class, Constant, FieldType};
use crate::JVM;
use crate::linker::ClassLoader;

pub fn analyze_class(class: Arc<Class>, class_loader: &Arc<dyn ClassLoader>, jvm: &JVM, classes: &mut HashSet<Arc<Class>>) {
    classes.insert(class.clone());

    class.constant_pool.constants.iter().for_each(|(_index, constant)| {
        match constant {
            Constant::Class { path, depth } => {
                if *depth != 0 {
                    return;
                }

                if let Ok(clazz) = jvm.find_class(path, class_loader.clone()) {
                    if !classes.contains(&clazz) {
                        analyze_class(clazz, class_loader, jvm, classes);
                    }
                }

            }
            _ => {}
        }
    });
}