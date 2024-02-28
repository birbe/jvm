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
use crate::linker::{BootstrapLoader, ClassLoader};
use crate::{JVM};
use crate::classfile::resolved::{Class, Ref};
use crate::execution::MethodHandle;
use highway::HighwayHash;
use js_sys::{ArrayBuffer, Uint8Array};
use once_cell::sync::Lazy;
use wasm_bindgen::{JsCast, JsValue};
use wasm_bindgen_futures::JsFuture;
use web_sys::{Request, RequestInit, RequestMode, Response, Worker};
use jvm_types::JParse;
use crate::classfile::ClassFile;
use crate::env::Object;
use crate::env::wasm::native::native_func;
use crate::thread::Thread;

static CHANNEL: Lazy<(Sender<String>, Mutex<Receiver<String>>)> = Lazy::new(|| {
    let (sender, recv) = async_channel::unbounded();
    (sender, Mutex::new(recv))
});
static CLASSES: Lazy<ArcSwap<HashMap<String, Arc<Vec<u8>>>>> = Lazy::new(|| ArcSwap::new(Arc::new(HashMap::new())));

struct NativeClassLoader {
    pub classes: RwLock<HashMap<String, Arc<Class>>>,
    pub handles: RwLock<HashMap<u64, Arc<MethodHandle>>>,
    instance: Object
}

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

impl BootstrapLoader for NativeClassLoader {

    fn get_classfile(&self, classpath: &str) -> Option<ClassFile> {
        let bytes = self.get_bytes(classpath)?;
        ClassFile::from_bytes(Cursor::new(bytes)).ok()
    }

    fn has_class(&self, classpath: &str) -> bool {
        true
    }

}


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

    let mut opts = RequestInit::new();
    opts.method("GET");
    opts.mode(RequestMode::Cors);

    let url = format!("classes/{}.class", classpath);

    let request = Request::new_with_str_and_init(&url, &opts).unwrap();

    request
        .headers()
        .set("Accept", "*/*").unwrap();

    let window = web_sys::window().unwrap();
    let resp_value = JsFuture::from(window.fetch_with_request(&request)).await.unwrap();

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

    let bootstrapper = NativeClassLoader {
        classes: RwLock::new(HashMap::new()),
        handles: RwLock::new(HashMap::new()),
        instance: unsafe { Object::NULL }
    };

    let native_bootstrapper = Arc::new(bootstrapper);

    let jvm = JVM::new(
        Mutex::new(Box::new(StdoutWrapper)),
        Box::new(WasmEnvironment::new()),
        native_bootstrapper
    );
}
