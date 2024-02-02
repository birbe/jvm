use std::io::Write;
use std::sync::Arc;
use std::time::Instant;
use parking_lot::Mutex;
use wasm_bindgen::JsValue;
use wasm_bindgen::prelude::wasm_bindgen;
use crate::linker::ClassLoader;
use crate::{JVM, NativeClassLoader};

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
pub fn wasm_test() {
    use crate::env::wasm::WasmEnvironment;
    use std::io::stdout;

    console_error_panic_hook::set_once();

    let mock = NativeClassLoader {
        ..Default::default()
    };

    let bootstrapper = Arc::new(mock) as Arc<dyn ClassLoader>;

    let jvm = JVM::new(
        Mutex::new(Box::new(StdoutWrapper)),
        Box::new(WasmEnvironment::new()),
    );
    let main = jvm.find_class("Main", bootstrapper.clone()).unwrap();

    let out = jvm.create_thread(bootstrapper.clone()).call("Main", "main", "()I", &[]).unwrap();

    console_log!("{out:?}");
}
