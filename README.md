# Experimental JVM on WASM

This repo is my experiment in making a pure-Rust JVM that supports running both on native platforms
and WebAssembly.

On WebAssembly, it takes advantage of the GC extension to the WASM spec, meaning in theory the Java objects get garbage-collected
by the browser's garbage collector instead of one written within WASM.

At the moment it's just a toy and is non-functional.

Things I've worked on so far, include but are not limited to (and in no particular order or amount of success):
- [x] Java classfile parsing
- [x] Extremely basic non-garbage collected heap for native
- [x] Dynamic linking of the WASM modules generated at runtime
- [x] Basic interop between Rust and WASM objects
- [x] Implemented part of the interpreter
- [x] Java bytecode control flow to WASM (using the stackifier algorithm)

Things that are notably missing:
- [x] A lot of instructions
- [x] Most of the important instructions like for dynamic dispatch (they're either partially (incorrectly) implemented or entirely unimplemented)
- [x] Garbage collection (on native platforms)

Originally it only ran on native, but now it only runs on WASM as that's where I'm doing most of my testing.
Eventually I'll properly refactor it's internal APIs to support conditionally compiling with the proper parts enabled
for each specific platform.