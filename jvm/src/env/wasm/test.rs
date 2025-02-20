use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::hash::Hasher;
use std::io::{Cursor, Write};
use std::sync::Arc;
use std::sync::mpsc::channel;
use arc_swap::ArcSwap;
use async_channel::{Receiver, Sender, TrySendError};
use highway::{HighwayHasher, Key};
use parking_lot::{Mutex, RwLock};
use wasm_bindgen::prelude::wasm_bindgen;
use crate::linker::{BootstrapLoader, ClassLoader, ClassLoaderObject};
use crate::{ClassContext, ClassInstance, ClassLoadError, JVM};
use crate::classfile::resolved::{Class, Method, Ref};
use crate::execution::MethodHandle;
use highway::HighwayHash;
use js_sys::{ArrayBuffer, Uint8Array};
use once_cell::sync::Lazy;
use wasm_bindgen::{JsCast, JsValue};
use wasm_bindgen_futures::JsFuture;
use web_sys::{Request, RequestInit, RequestMode, Response, Worker, WorkerGlobalScope};
use jvm_types::JParse;
use crate::classfile::ClassFile;
use crate::env::{Object, ToAny};
use crate::env::wasm::jit::{jit_module, translate_method_java_to_wasm};
use crate::env::wasm::native::native_func;
use crate::thread::Thread;

static CHANNEL: Lazy<(Sender<String>, Mutex<Receiver<String>>)> = Lazy::new(|| {
    console_log!("initializing lazy");

    let (sender, recv) = async_channel::bounded(1);
    (sender, Mutex::new(recv))
});
static CLASSES: Lazy<ArcSwap<HashMap<String, Arc<Vec<u8>>>>> = Lazy::new(|| ArcSwap::new(Arc::new(HashMap::new())));

struct NativeClassLoader {}

unsafe impl Send for NativeClassLoader {}

impl Debug for NativeClassLoader {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "NativeClassLoader")
    }
}

impl NativeClassLoader {

    fn get_bytes(&self, classpath: &str) -> Option<Vec<u8>> {
        loop {
            match CHANNEL.0.try_send(classpath.into()) {
                Ok(_) => break,
                Err(_) => {}
            };
        }

        loop {
            let classes = CLASSES.load();
            match classes.get(classpath) {
                None => {}
                Some(data) => {
                    return Some((**data).clone())
                }
            }
        }
    }

}

impl BootstrapLoader for Arc<NativeClassLoader> {
    fn get_classfile(&self, classpath: &str) -> Option<ClassFile> {
        ClassFile::from_bytes(Cursor::new(self.get_bytes(classpath)?)).ok()
    }

    fn has_class(&self, classpath: &str) -> bool {
        classpath.starts_with("java/")
    }

    fn get_class(&self, thread: &mut Thread, classpath: &str) -> Result<Arc<Class>, ClassLoadError> {
        let classfile = self.get_classfile(classpath).unwrap();

        match thread.jvm.is_class_linked(classpath, self.get_id()) {
            None => thread.jvm.clone().generate_class(classpath, &classfile, Arc::new(self.clone()), thread),
            Some(class) => Ok(class)
        }
    }
}

const THE_NULL: &Object = &Object::NULL;

impl ClassLoader for Arc<NativeClassLoader> {
    fn find_class(&self, thread: &mut Thread, classpath: &str) -> Result<Arc<Class>, ClassLoadError> {
        if let Some(class) = thread.jvm.is_class_linked(classpath, self.get_id()) {
            return Ok(class);
        }

        let classfile = self.get_classfile(classpath).unwrap();
        let jvm = thread.jvm.clone();
        let class = jvm.generate_class(classpath, &classfile, Arc::new(self.clone()), thread)?;

        if classpath != "java/lang/Class" { jvm.generate_class_object(class.clone(), self, thread)? };

        Ok(class)
    }

    fn get_class_loader_object_handle(&self) -> &Object {
        THE_NULL
    }

    fn get_id(&self) -> u32 {
        0
    }
}

// impl ClassContext for &NativeClassLoader {
//
//     fn get_class(&self, jvm: &JVM, classpath: &str) -> Result<Arc<Class>, ClassLoadError> {
//         let mut hasher = HighwayHasher::default();
//         hasher.write_all(classpath.as_bytes()).unwrap();
//         hasher.write_u32(0);
//         let id = hasher.finish();
//
//         let loaded_classes = jvm.loaded_classes.read();
//
//         match loaded_classes.get(&id) {
//             None => {}
//             Some(class) => return Ok(class.class.clone())
//         };
//
//         drop(loaded_classes);
//
//         let classfile = self.get_classfile(classpath).unwrap();
//         let class = jvm.generate_class(&classfile, self)?;
//         jvm.generate_class_object(class.clone(), );
//
//         Ok(class)
//     }
//
//     fn id(&self) -> u32 {
//         0
//     }
// }


struct StdoutWrapper;

impl Write for StdoutWrapper {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        web_sys::console::log_1(&std::str::from_utf8(buf).unwrap().into());
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

#[wasm_bindgen]
pub fn main_thread_begin() -> JsValue {
    console_error_panic_hook::set_once();

    let array = js_sys::Array::new();
    array.push(&wasm_bindgen::module());
    array.push(&wasm_bindgen::memory());

    array.into()
}

#[wasm_bindgen]
pub async fn listen() {
    let recv = CHANNEL.1.lock();
    let classpath = recv.recv().await.unwrap();
    drop(recv);

    let mut opts = RequestInit::new();
    opts.method("GET");
    opts.mode(RequestMode::Cors);

    let url = format!("classes/{}.class", classpath);

    let request = Request::new_with_str_and_init(&url, &opts).unwrap();

    request
        .headers()
        .set("Accept", "*/*").unwrap();

    let scope: WorkerGlobalScope = js_sys::global().dyn_into().unwrap();
    let resp_value = JsFuture::from(scope.fetch_with_request(&request)).await.unwrap();

    // `resp_value` is a `Response` object.
    assert!(resp_value.is_instance_of::<Response>());
    let resp: Response = resp_value.dyn_into().unwrap();

    // Convert this other `Promise` into a rust `Future`.
    let buffer = JsFuture::from(resp.array_buffer().unwrap()).await.unwrap();
    let buffer = Uint8Array::new(&buffer);
    let vec = buffer.to_vec();

    let mut classes = (*CLASSES.load_full()).clone();
    classes.insert(classpath, Arc::new(vec));
    CLASSES.store(Arc::new(classes));
}

#[wasm_bindgen]
pub fn wasm_test() {
    use crate::env::wasm::WasmEnvironment;

    console_error_panic_hook::set_once();

    let bootstrapper = NativeClassLoader {};

    let native_bootstrapper = Arc::new(Arc::new(bootstrapper));

    let jvm = JVM::new(
        Mutex::new(Box::new(StdoutWrapper)),
        Box::new(WasmEnvironment::new()),
        native_bootstrapper.clone()
    );

    let mut main_thread = jvm.create_thread(native_bootstrapper.clone());

    let class = main_thread.class_loader.clone().find_class(&mut main_thread, "Main").unwrap();
    let string = main_thread.class_loader.clone().find_class(&mut main_thread, "java/lang/String").unwrap();

    // let method = string.get_method_by_name("newStringUTF8NoRepl", "([BIIZ)Ljava/lang/String;").unwrap();
    let method = class.get_method_by_name("jitTest", "()I").unwrap();

    let env = (*jvm.environment).to_any().downcast_ref::<WasmEnvironment>().unwrap();

    let to_jit: [(&Method, &Class); 1] = [(method, &class)];


    let time = js_sys::Date::now();

    let jit = jit_module(&jvm, env, &to_jit, &mut main_thread);

    let new_time = js_sys::Date::now();

    // console_log!("Generated JIT in {}ms", (new_time - time) / 1000.0);

    env.link_jit(&jvm, jit);

    let handle = jvm.get_method("Main", "main", "()I", 0);
    //
    console_log!("{:?}", main_thread.invoke(&handle, Box::new([])));

}
