use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::hash::Hasher;
use std::io::Write;
use std::sync::Arc;
use std::sync::mpsc::channel;
use arc_swap::ArcSwap;
use async_channel::{Receiver, Sender, TrySendError};
use highway::{HighwayHasher, Key};
use parking_lot::{Mutex, RwLock};
use wasm_bindgen::prelude::wasm_bindgen;
use crate::linker::{ClassLoader, JavaClassLoader};
use crate::{JVM};
use crate::classfile::resolved::{Class, Ref};
use crate::execution::MethodHandle;
use highway::HighwayHash;
use js_sys::{ArrayBuffer, Uint8Array};
use once_cell::sync::Lazy;
use wasm_bindgen::{JsCast, JsValue};
use wasm_bindgen_futures::JsFuture;
use web_sys::{Request, RequestInit, RequestMode, Response, Worker};
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

impl ClassLoader for NativeClassLoader {

    fn find_class(&self, thread: &mut Thread, classpath: &str) -> Option<Object> {
        let bytes = self.get_bytes(classpath).unwrap();
        let jvm = thread.jvm.clone();

        jvm.define_class(thread, &*self, &bytes).unwrap()
    }

    fn register_class(&self, class: Arc<Class>) {
        let mut classes = self.classes.write();
        classes.insert((*class.this_class).clone(), class);
    }

    fn get_class(&self, classpath: &str) -> Option<Arc<Class>> {
        let classes = self.classes.read();
        classes.get(classpath).cloned()
    }

    fn link_ref(&self, ref_: Arc<Ref>, method_handle: Arc<MethodHandle>) {
        let mut hasher = HighwayHasher::new(Key([0; 4]));
        hasher.append(ref_.class.as_bytes());
        hasher.append(ref_.name_and_type.name.as_bytes());
        hasher.append(ref_.name_and_type.descriptor.as_bytes());
        let key = hasher.finish();

        self.handles.write().insert(key, method_handle);
    }

    fn get_method_handle(&self, ref_: &Ref) -> Arc<MethodHandle> {
        let handles = self.handles.read();

        let mut hasher = HighwayHasher::new(Key([0; 4]));
        hasher.append(ref_.class.as_bytes());
        hasher.append(ref_.name_and_type.name.as_bytes());
        hasher.append(ref_.name_and_type.descriptor.as_bytes());
        let key = hasher.finish();

        match handles.get(&key) {
            None => {
                panic!("Couldn't find handle {ref_:?}");
            }
            Some(handle) => handle.clone()
        }
    }

    fn get_method_by_name(&self, classpath: &str, name: &str, descriptor: &str) -> Arc<MethodHandle> {
        let handles = self.handles.read();

        let mut hasher = HighwayHasher::new(Key([0; 4]));
        hasher.append(classpath.as_bytes());
        hasher.append(name.as_bytes());
        hasher.append(descriptor.as_bytes());
        let key = hasher.finish();

        handles.get(&key).unwrap().clone()
    }

    fn id(&self) -> usize {
        0
    }

    fn get_class_loader_object_handle(&self) -> &Object {
        &self.instance
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

    let mock = NativeClassLoader {
        classes: RwLock::new(HashMap::new()),
        handles: RwLock::new(HashMap::new()),
        instance: unsafe { Object::NULL }
    };

    let native_bootstrapper = Arc::new(mock);
    let classloader = native_bootstrapper.clone() as Arc<dyn ClassLoader>;

    let jvm = JVM::new(
        Mutex::new(Box::new(StdoutWrapper)),
        Box::new(WasmEnvironment::new()),
    );

    let mut thread = jvm.create_thread(classloader.clone());

    {
        let class = native_bootstrapper.get_bytes("java/lang/Class").unwrap();
        let class_struct = jvm.make_class_struct(&mut *thread, &*classloader, &class).unwrap();
        native_bootstrapper.register_class(class_struct.clone());
        jvm.link_class(&*classloader, class_struct);
    }
    //
    // let classloader = jvm.find_class("WASMClassLoader", &*classloader, &mut *thread).unwrap();
    // let loader_instance = jvm.environment.new_object(&classloader);
    //
    // let java_loader = Arc::new(JavaClassLoader {
    //     instance: loader_instance,
    //     loader_classpath: "WASMClassLoader".into(),
    //     classes: Mutex::new(HashMap::new()),
    //     handles: RwLock::new(HashMap::new())
    // }) as Arc<dyn ClassLoader>;
    //
    //
    // thread.class_loader = java_loader.clone();
    //
    // console_log!("Thread has been bootstrapped, switching to custom class loader");

    let main = jvm.find_class("Main", &*native_bootstrapper, &mut *thread).unwrap();
    //
    let out = thread.call("Main", "main", "()I", &[]);
    console_log!("{out:?}");
}
