use std::cell::RefCell;
use crate::classfile::resolved::{AccessFlags, Class, Field, FieldType, Method, NameAndType, Ref};

use std::fmt::Debug;
use std::hash::Hasher;
use std::io::Write;
use std::sync::Arc;
use highway::HighwayHasher;
use crate::env::Object;
use crate::execution::MethodHandle;
use crate::thread::{Operand, Thread};
use crate::{ClassLoadError, ResolveMethodError};
use crate::classfile::ClassFile;

pub trait BootstrapLoader: ClassLoader {

    fn get_classfile(&self, classpath: &str) -> Option<ClassFile>;

    fn has_class(&self, classpath: &str) -> bool;

    ///This method checks the JVM method area to see if the class has been defined already;
    /// if not, then it will retrieve the bytes defining the classfile through some implementation-dependent manner
    /// and call JVM::generate_class. This method will not generate a corresponding Class<?> instance
    fn get_class(&self, thread: &mut Thread, classpath: &str) -> Result<Arc<Class>, ClassLoadError>;

}

pub struct ClassLoaderThreadedContext<'a> {
    pub loader: &'a dyn ClassLoader,
    pub context: RefCell<&'a mut Thread>
}

// impl<'a> ClassContext for ClassLoaderThreadedContext<'a> {
//     fn get_class(&self, _: &JVM, classpath: &str) -> Result<Arc<Class>, ClassLoadError> {
//         let mut context = self.context.borrow_mut();
//         let jvm = context.jvm.clone();
//
//         jvm.retrieve_class(classpath, self.loader, *context)
//     }
//
//     fn id(&self) -> u32 {
//         self.loader.get_id()
//     }
// }

pub struct ClassLoaderObject {
    pub instance: Object,
    pub loader_classpath: String,
    pub find_class: Arc<MethodHandle>,
    pub id: u32
}

unsafe impl Send for ClassLoaderObject {}

pub trait ClassLoader {

    fn find_class(&self, thread: &mut Thread, classpath: &str) -> Option<Arc<Class>>;

    fn get_class_loader_object_handle(&self) -> &Object;

    fn get_id(&self) -> u32;

}

impl ClassLoader for Arc<ClassLoaderObject> {
    fn find_class(&self, thread: &mut Thread, classpath: &str) -> Option<Arc<Class>> {
        let jvm = thread.jvm.clone();
        let classpath_string: Object = jvm.environment.new_string(classpath, thread);

        {
            let loaded_classes = jvm.loaded_classes.read();

            let mut hasher = HighwayHasher::default();
            hasher.write_all(classpath.as_bytes()).unwrap();
            hasher.write_u32(self.get_id());
            let hash_key = hasher.finish();
            
            match loaded_classes.get(&hash_key) {
                None => {}
                Some(class) => return Some(class.clone())
            }
        }

        //defineClass should be called by the ClassLoader when invoked
        let operands = Box::new([
            thread.jvm.environment.clone_reference(&self.instance).consume(),
            classpath_string.consume()
        ]);
        
        let object = unsafe {
            jvm.environment.object_from_operand(&thread.invoke(&self.find_class, operands).unwrap())
        };
        let class = jvm.retrieve_class(classpath, self, thread).ok()?;
        
        Some(class)
    }

    fn get_class_loader_object_handle(&self) -> &Object {
        &self.instance
    }

    fn get_id(&self) -> u32 {
        self.id
    }
}

pub fn routine_resolve_invokespecial(class: &Class, method_ref: &Ref) {
    match class.get_method(&method_ref.name_and_type) {
        None => {}
        Some(_other_method) => {}
    }
}

pub fn routine_resolve_field<'a>(
    name_and_type: &'a NameAndType,
    class: &'a Class,
) -> Option<(&'a Field, &'a Class)> {
    match class.get_field(name_and_type) {
        None => {
            //TODO: superinterfaces?

            if let Some(superclass) = &class.super_class {
                let super_resolve = routine_resolve_field(name_and_type, superclass);

                if let Some(resolved) = super_resolve {
                    return Some(resolved);
                }
            }

            None
        }
        Some(field) => Some((field, class)),
    }
}

//https://docs.oracle.com/javase/specs/jvms/se20/html/jvms-5.html#jvms-5.4.6
pub fn routine_select_method<'a, 'b: 'a>(resolved_method: &'a Method, object_type: &'b Class) -> (&'a Method, &'b Class) {
    if resolved_method.access_flags.contains(AccessFlags::PRIVATE) {
        (resolved_method, object_type)
    } else {
        let mut out = None;

        let classes = object_type.parents();

        for parent in [object_type].into_iter().chain(classes.iter().map(|parent| &***parent)) {
            if let Some(overrider) = parent.get_method_by_name(&resolved_method.name, &resolved_method.descriptor.string) && parent.find_overriding_method(&resolved_method, object_type).is_some() {
                out = Some((&**overrider, parent));
                break;
            }
        }

        if let Some((overrider, parent)) = out {
            (overrider, parent)
        } else {
            todo!()
        }
    }
}

pub fn routine_resolve_method<'class>(
    thread: &Thread,
    ref_: &Ref,
    class: &'class Class,
) -> Result<&'class Method, ResolveMethodError> {
    if class.access_flags.contains(AccessFlags::INTERFACE) {
        return Err(ResolveMethodError::IncompatibleClassChangeError);
    }

    fn step2<'class>(ref_: &Ref, class: &'class Class) -> Option<&'class Method> {
        let methods: Vec<&Arc<Method>> = class
            .methods
            .iter()
            .filter(|m| m.name == ref_.name_and_type.name)
            .collect();

        if methods.len() == 1 && methods[0].is_signature_polymorphic(&class.this_class) {
            let resolved_method = methods[0];

            for arg in &resolved_method.descriptor.args {
                match arg {
                    FieldType::Array { type_, dimensions } => {
                        match &**type_ {
                            FieldType::Class(classpath) => {
                                todo!()
                            },
                            _ => {}
                        }
                    }
                    FieldType::Class(_) => {},
                    _ => {}
                }
            }

            return Some(&*resolved_method);
        }

        if let Some(method) = class.get_method(&ref_.name_and_type) {
            return Some(method);
        }

        class
            .super_class
            .as_ref()
            .and_then(|super_class| step2(ref_, super_class))
    }

    if let Some(method) = step2(ref_, class) {
        return Ok(method);
    }

    todo!()
}