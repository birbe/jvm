// synchronously, using the browser, import out shim JS scripts
importScripts('jvm.js');

// Wait for the main thread to send us the shared module/memory. Once we've got
// it, initialize it all with the `wasm_bindgen` global we imported via
// `importScripts`.
//
// After our first message all subsequent messages are an entry point to run,
// so we just do that.
self.onmessage = event => {

    const { main_thread_begin, listen } = wasm_bindgen;

    let initialised = wasm_bindgen(...event.data).catch(err => {
        // Propagate to main `onerror`:
        setTimeout(() => {
            throw err;
        });
        // Rethrow to keep promise rejected and prevent execution of further commands:
        throw err;
    });

    initialised.then(() => {
        (async ()=>{
            while(true) {
                await listen();
            }
        })().then(()=>{})
    });

};