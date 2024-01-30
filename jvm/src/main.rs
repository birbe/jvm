use jvm::bytecode::JValue;

use jvm::heap::Object;

use jvm::linker::ClassLoader;

use jvm::{wasm_test, JVM};

use parking_lot::{Mutex, RwLock};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::fs;

use std::io::stdout;

use std::sync::Arc;
use std::time::Instant;
use wasm_encoder::{
    CodeSection, CompositeType, ExportKind, ExportSection, FieldType, Function, FunctionSection,
    HeapType, Instruction, Module, RefType, StorageType, StructType, SubType, TypeSection, ValType,
};
use wasmparser::{Validator, WasmFeatures};

use jvm::classfile::resolved::Class;

extern crate jvm;

struct NativeClassLoader {
    pub classes: RwLock<HashMap<String, Arc<Class>>>,
}

impl Debug for NativeClassLoader {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "NativeClassLoader")
    }
}

impl ClassLoader for NativeClassLoader {
    fn get_bytes(&self, classpath: &str) -> Option<Vec<u8>> {
        // if cfg!(miri) {
        match classpath {
            "Main" => Some(include_bytes!("../test_classes/Main.class")[..].into()),
            "java/lang/Object" => {
                Some(include_bytes!("../test_classes/java/lang/Object.class")[..].into())
            }
            "java/lang/String" => {
                Some(include_bytes!("../test_classes/java/lang/String.class")[..].into())
            }
            _ => panic!("{classpath} not found"),
        }
        // } else {
        //     fs::read(format!("./jvm/test_classes/{}.class", classpath)).ok()
        // }
    }

    fn register_class(&self, classpath: &str, class: Arc<Class>) {
        let mut classes = self.classes.write();
        classes.insert(classpath.into(), class);
    }

    fn get_class(&self, classpath: &str) -> Option<Arc<Class>> {
        let classes = self.classes.read();
        classes.get(classpath).cloned()
    }

    fn id(&self) -> usize {
        0
    }
}

fn start_native(jvm: Arc<JVM>, bootstrapper: Arc<NativeClassLoader>) {
    let mut handle = jvm.create_thread(bootstrapper.clone());

    let now = Instant::now();

    let string = jvm.heap.allocate_string("OwO", &jvm);

    let string_array = jvm.heap.allocate_raw_object_array(&string.class, 1);

    unsafe { (&mut *string_array).body.body[0] = string.value };

    let array_object = unsafe { Object::from_raw(string_array) };

    let result = handle
        .call(
            "Main",
            "main",
            "([Ljava/lang/String;)[Ljava/lang/String;",
            &[JValue::Reference(array_object)],
        )
        .unwrap();

    println!(
        "main(null) = {:?} in {}ms",
        result,
        Instant::now().duration_since(now).as_millis()
    );
}

fn main() {
    wasm_test();
}
