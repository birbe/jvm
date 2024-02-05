use std::borrow::Cow;
use wasm_encoder::{BlockType, CodeSection, CompositeType, EntityType, ExportKind, ExportSection, FieldType, Function, FunctionSection, HeapType, ImportSection, Instruction, Module, RefType, StorageType, StructType, SubType, TableType, TypeSection, ValType};
use crate::classfile::resolved;

pub mod stub;

pub enum CompileError {
    NoCode,
}

pub fn generate_helper_wasm() -> Module {
    let mut module = Module::new();

    let mut types = TypeSection::new();
    let mut imports = ImportSection::new();
    let mut code = CodeSection::new();
    let mut funcs = FunctionSection::new();
    let mut exports = ExportSection::new();

    types.struct_([
        FieldType {
            element_type: StorageType::Val(ValType::Ref(RefType { nullable: false, heap_type: HeapType::Any })),
            mutable: false,
        },
        FieldType {
            element_type: StorageType::Val(ValType::I32),
            mutable: false,
        },
    ]);

    //Excluding object arrays
    let storage_type_start = types.len() + 1;

    for storage_type in [StorageType::Val(ValType::Ref(RefType { nullable: true, heap_type: HeapType::Struct})), StorageType::I8, StorageType::I16, StorageType::Val(ValType::I32), StorageType::Val(ValType::I64), StorageType::Val(ValType::F32), StorageType::Val(ValType::F64)] {
        types.array(&storage_type, true);
    }

    let alloc_signature = types.len();
    types.function([], [ValType::I32]);

    imports.import("jvm", "objects", EntityType::Table(TableType {
        element_type: RefType { nullable: true, heap_type: HeapType::Any },
        minimum: 0,
        maximum: None,
    }));

    let object_class_signature = types.len();
    types.function([ValType::I32], [ValType::I32]);
    let new_object_array_signature = types.len();
    types.function([ValType::I32; 2], [ValType::I32]);
    let aastore_signature = types.len();
    types.function([ValType::I32; 3], []);

    imports.import("jvm", "alloc_ref", EntityType::Function(alloc_signature));

    {

        funcs.function(object_class_signature);

        let mut func = Function::new([]);
        func.instruction(&Instruction::LocalGet(0));
        func.instruction(&Instruction::TableGet(0));
        func.instruction(&Instruction::RefCastNullable(HeapType::Concrete(0)));
        func.instruction(&Instruction::StructGet { struct_type_index: 0, field_index: 1 });
        func.instruction(&Instruction::End);
        code.function(&func);

        exports.export("obj_class", ExportKind::Func, 1);
    }

    {
        funcs.function(new_object_array_signature);

        let mut func = Function::new([(1, ValType::I32)]);

        func.instruction(&Instruction::Call(0));
        func.instruction(&Instruction::LocalTee(2));
        func.instruction(&Instruction::LocalGet(0));
        func.instruction(&Instruction::ArrayNewDefault(1));
        func.instruction(&Instruction::LocalGet(1));
        func.instruction(&Instruction::StructNew(0));
        func.instruction(&Instruction::TableSet(0));
        func.instruction(&Instruction::LocalGet(2));

        func.instruction(&Instruction::End);
        code.function(&func);

        exports.export("new_object_array", ExportKind::Func, 2);
    }

    {
        funcs.function(aastore_signature);

        let mut func = Function::new([(1, ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Struct,
        }))]);

        //arrayref, index, value

        //get the value

            func.instruction(&Instruction::Block(BlockType::Empty));

                func.instruction(&Instruction::Block(BlockType::Empty));
                    func.instruction(&Instruction::LocalGet(2));
                    func.instruction(&Instruction::BrIf(1));
                func.instruction(&Instruction::End);

                func.instruction(&Instruction::LocalGet(2));
                func.instruction(&Instruction::TableGet(0));
                func.instruction(&Instruction::RefCastNullable(HeapType::Concrete(0)));
                func.instruction(&Instruction::StructGet { struct_type_index: 0, field_index: 0 });
                func.instruction(&Instruction::RefCastNullable(HeapType::Concrete(0)));
                func.instruction(&Instruction::LocalSet(3));

            func.instruction(&Instruction::End);

        func.instruction(&Instruction::LocalGet(0));
        func.instruction(&Instruction::TableGet(0));
        func.instruction(&Instruction::RefCastNullable(HeapType::Concrete(0)));
        func.instruction(&Instruction::StructGet { struct_type_index: 0, field_index: 0 });
        func.instruction(&Instruction::RefCastNullable(HeapType::Concrete(1)));

        func.instruction(&Instruction::LocalGet(1));
        func.instruction(&Instruction::LocalGet(3));
        func.instruction(&Instruction::ArraySet(1));

        func.instruction(&Instruction::End);
        code.function(&func);

        exports.export("set_object_array_element", ExportKind::Func, 3);
    }

    let array_types = [
        (0, "bool", ValType::I32),
        (1, "char", ValType::I32),
        (4, "float", ValType::F32),
        (5, "double", ValType::F64),
        (0, "byte", ValType::I32),
        (1, "short", ValType::I32),
        (2, "int", ValType::I32),
        (3, "long", ValType::I64),
    ];

    {

        funcs.function(new_object_array_signature);

        let mut func = Function::new([(1, ValType::I32)]);

        (0..array_types.len() - 1).for_each(|_| { func.instruction(&Instruction::Block(BlockType::Empty)); });

        func.instruction(&Instruction::Block(BlockType::Empty));

        func.instruction(&Instruction::LocalGet(0));
        func.instruction(&Instruction::I32Const(4));
        func.instruction(&Instruction::I32Sub);

        func.instruction(&Instruction::BrTable(Cow::Owned(vec![0, 1, 2, 3, 4, 5, 6, 7]), 0));

        func.instruction(&Instruction::End);

        array_types.iter().enumerate().for_each(|(index, (type_index, name, ..))| {
            func.instruction(&Instruction::Call(0));
            func.instruction(&Instruction::LocalTee(2));
            func.instruction(&Instruction::LocalGet(1));
            func.instruction(&Instruction::ArrayNewDefault(type_index + storage_type_start));
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::StructNew(0));
            func.instruction(&Instruction::TableSet(0));

            func.instruction(&Instruction::LocalGet(2));
            func.instruction(&Instruction::Return);

            func.instruction(&Instruction::End);
        });

        code.function(&func);

        exports.export("new_array", ExportKind::Func, 4);

    }

    {
        array_types.iter().enumerate().for_each(|(index, (type_index, name, val_type))| {
            funcs.function(types.len());
            types.function(vec![ValType::I32, ValType::I32, *val_type], []);

            let mut func = Function::new([]);

            func.instruction(&Instruction::LocalGet(0));
            func.instruction(&Instruction::TableGet(0));
            func.instruction(&Instruction::RefCastNullable(HeapType::Concrete(0)));
            func.instruction(&Instruction::StructGet { struct_type_index: 0, field_index: 0 });
            func.instruction(&Instruction::RefCastNullable(HeapType::Concrete(type_index + storage_type_start)));
            func.instruction(&Instruction::LocalGet(1));
            func.instruction(&Instruction::LocalGet(2));
            func.instruction(&Instruction::ArraySet(type_index + storage_type_start));

            func.instruction(&Instruction::End);

            code.function(&func);

            exports.export(&format!("{}_array_store", name), ExportKind::Func, index as u32 + 5);
        });

    }

    module.section(&types);
    module.section(&imports);
    module.section(&funcs);
    module.section(&exports);
    module.section(&code);

    module

}