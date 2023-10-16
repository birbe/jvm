




use cranelift::prelude::*;
use cranelift_jit::{JITModule};


pub enum CompileError {
    NoCode,
}

struct JITContext {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    module: JITModule,
}

// impl JITContext {
//
//     pub fn new() -> Self {
//         let mut flag_builder = settings::builder();
//         flag_builder.set("use_colocated_libcalls", "false").unwrap();
//         flag_builder.set("is_pic", "false").unwrap();
//         let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
//             panic!("host machine is not supported: {}", msg);
//         });
//         let isa = isa_builder
//             .finish(settings::Flags::new(flag_builder))
//             .unwrap();
//         let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
//
//         let module = JITModule::new(builder);
//         Self {
//             builder_context: FunctionBuilderContext::new(),
//             ctx: module.make_context(),
//             module,
//         }
//     }
//
//     pub fn compile_method(&mut self, method: &Method) -> Result<*const (), CompileError> {
//         let code = match method.attributes.get("Code") {
//             Some(Attribute::Code(code)) => code,
//             _ => return Err(CompileError::NoCode)
//         };
//
//         let (loops, nodes, scc_s) = find_loops(&code.instructions);
//
//         let labeled = label_nodes(&loops, &nodes, &scc_s);
//
//         let zipped: Vec<_> = nodes.iter().zip(labeled.iter()).collect();
//
//         let metrics = identify_scopes(&zipped, &scc_s);
//
//         let mut block_ranges = &metrics.blocks[..];
//         let mut loop_ranges = &metrics.loops[..];
//
//         let loop_out = loop_ranges.get(0) == Some(&(0..labeled.len()));
//
//         let scopes = create_scopes(&mut block_ranges, &mut loop_ranges, &labeled, 0..labeled.len(), loop_out);
//
//         let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
//
//         let entry = builder.create_block();
//
//
//
//         Err(CompileError::NoCode)
//     }
//
//     fn translate(&mut self, java_block: &jit::Block, builder: &mut FunctionBuilder, instructions: &[Instruction]) {
//         match java_block {
//             jit::Block::Loop(blocks) => {
//                 let loop_block = builder.create_block();
//                 builder.switch_to_block(loop_block);
//                 for other_block in blocks.iter() {
//                     self.translate(other_block, builder);
//                 }
//             }
//             jit::Block::Normal(_) => {}
//             jit::Block::Nodes(nodes) => {
//                 for node in nodes.iter() {
//
//                 }
//             }
//         }
//     }
//
// }
