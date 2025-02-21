use crate::bytecode::ArrayType;
use crate::classfile::resolved::{AccessFlags, Class, FieldType};
use crate::env::Object;
use crate::execution::MethodHandle;
use crate::thread::{Operand, Thread, ThreadHandle};
use bitflags::Flags;
use highway::HighwayHasher;
use std::hash::Hasher;
use std::sync::atomic::AtomicU64;

pub fn native_func(
    method_handle: &MethodHandle,
    thread: &mut Thread,
    mut args: Box<[Operand]>,
) -> Operand {
    let jvm = thread.jvm.clone();
    let class_loader = thread.class_loader.clone();

    match (
        &method_handle.class.this_class[..],
        &method_handle.method.name[..],
        &method_handle.method.descriptor.string[..],
    ) {
        // ("java/lang/Object", "registerNatives", "()V")
        // | ("java/security/AccessController", "doPrivileged", _ )=> {
        //     Operand::new_data(0)
        // },
        // ("java/lang/System", "registerNatives", "()V") => {
        //     Operand::new_data(0)
        // }
        // ("java/lang/Class", "registerNatives", "()V") => {
        //     Operand::new_data(0)
        // },
        ("java/lang/Class", "getPrimitiveClass", "(Ljava/lang/String;)Ljava/lang/Class;") => {
            let op = std::mem::replace(&mut args[0], Object::NULL.consume());
            let string_ref = unsafe { thread.jvm.environment.object_from_operand(op) };
            let string = thread.jvm.get_string(&string_ref);

            let classpath = match &*string {
                "int" => "I",
                "double" => "D",
                "float" => "F",
                "char" => "C",
                "long" => "J",
                "boolean" => "Z",
                "short" => "S",
                "byte" => "B",
                _ => unreachable!("Invalid primitive {string}"),
            };

            let class = jvm
                .bootstrap_classloader
                .find_class(thread, classpath)
                .unwrap();

            jvm.environment
                .clone_reference(class.class_instance.get().unwrap())
                .consume()
        }
        ("Main", "println", "(Ljava/lang/String;)V") => {
            let op = std::mem::replace(&mut args[0], Object::NULL.consume());
            let string_ref = unsafe { thread.jvm.environment.object_from_operand(op) };
            console_log!("{}", thread.jvm.get_string(&string_ref));

            Operand::new_data(0)
        }
        ("java/lang/Throwable", "fillInStackTrace", "(I)Ljava/lang/Throwable;") => {
            std::mem::replace(&mut args[0], Object::NULL.consume())
        }
        ("java/lang/Class", "initClassName", "()Ljava/lang/String;") => {
            let op = std::mem::replace(&mut args[0], Object::NULL.consume());
            let object = unsafe { jvm.environment.object_from_operand(op) };

            let class_ptr = unsafe {
                jvm.environment.get_object_field(
                    &object,
                    jvm.environment.get_object_class(&object),
                    "__jvm_internal_id",
                    "I",
                )
            }
            .data() as *const Class;

            jvm.new_string(&unsafe { &*class_ptr }.this_class, thread)
                .consume()
        }
        ("java/lang/Class", "getDeclaredFields0", "(Z)[Ljava/lang/reflect/Field;") => {
            let op = std::mem::replace(&mut args[0], Object::NULL.consume());
            let object = unsafe { jvm.environment.object_from_operand(op) };

            let public_only = args[1].data() == 1;

            let class_ptr = unsafe {
                jvm.environment.get_object_field(
                    &object,
                    jvm.environment.get_object_class(&object),
                    "__jvm_internal_id",
                    "I",
                )
            }
            .data() as *const Class;

            let class = unsafe { &*class_ptr };

            let field_class = jvm
                .bootstrap_classloader
                .find_class(thread, "java/lang/reflect/Field")
                .unwrap();
            let field_array_class = jvm.find_array_class(thread, field_class.clone(), 1);

            let field_init = jvm.get_method(
                "java/lang/reflect/Field",
                "<init>",
                "(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/Class;IZILjava/lang/String;[B)V",
                0,
            );

            ThreadHandle::init_static(&field_class, thread);

            let fields: Vec<Object> = class
                .fields
                .iter()
                .filter(|field| !public_only || field.access_flags.contains(AccessFlags::PUBLIC))
                .enumerate()
                .map(|(slot, field)| {
                    let field_object = jvm.environment.new_object(&field_class);

                    let type_classpath = match &field.descriptor {
                        FieldType::Array { .. } => &field.descriptor_string,
                        FieldType::Byte => "B",
                        FieldType::Char => "C",
                        FieldType::Double => "D",
                        FieldType::Float => "F",
                        FieldType::Int => "I",
                        FieldType::Long => "J",
                        FieldType::Short => "S",
                        FieldType::Boolean => "Z",
                        FieldType::Class(classpath) => &classpath,
                    };

                    let declaring_class = jvm.environment.clone_reference(&object).consume();
                    let name = jvm.new_string(&field.name, thread).consume();

                    let type_class = class
                        .class_loader
                        .find_class(thread, type_classpath)
                        .unwrap();

                    let type_ = jvm
                        .environment
                        .clone_reference(type_class.class_instance.get().unwrap())
                        .consume();
                    let modifiers = Operand::new_data(field.access_flags.bits() as u32);
                    let trusted_final =
                        Operand::new_data(field.access_flags.contains(AccessFlags::FINAL) as u32);
                    let slot = Operand::new_data(slot as u32);
                    let signature = jvm
                        .new_string(
                            &format!("{}{}", field.name, field.descriptor_string),
                            thread,
                        )
                        .consume();
                    let annotations = jvm.environment.new_array(ArrayType::Byte, 0).consume();

                    let args = Box::new([
                        jvm.environment.clone_reference(&field_object).consume(),
                        declaring_class,
                        name,
                        type_,
                        modifiers,
                        trusted_final,
                        slot,
                        signature,
                        annotations,
                    ]);

                    drop(thread.invoke(&field_init, args));

                    field_object
                })
                .collect();

            let object_array = jvm
                .environment
                .new_object_array(&field_array_class, fields.len() as i32);
            for (index, field_object) in fields.into_iter().enumerate() {
                unsafe {
                    jvm.environment.set_object_array_element(
                        &object_array,
                        index as i32,
                        field_object.consume(),
                    );
                }
            }

            object_array.consume()
        }
        ("java/lang/Class", "isPrimitive", "()Z") => {
            let op = std::mem::replace(&mut args[0], Object::NULL.consume());
            let object = unsafe { jvm.environment.object_from_operand(op) };

            let class_ptr = unsafe {
                jvm.environment.get_object_field(
                    &object,
                    jvm.environment.get_object_class(&object),
                    "__jvm_internal_id",
                    "I",
                )
            }
            .data() as *const Class;

            Operand::new_data(unsafe { &*class_ptr }.is_primitive as u32)
        }
        ("WASMClassLoader", "fetch", "(Ljava/lang/String;)[B") => {
            let byte_array = jvm.environment.new_array(ArrayType::Byte, 0);

            Operand::new_data(0)
        }
        // ("java/lang/ClassLoader", "registerNatives", "()V") => {
        //     Operand::new_data(0)
        // }
        ("sun/reflect/Reflection", "getCallerClass", "()Ljava/lang/Class;") => {
            console_log!("called");
            let lower_frame =
                &thread.frame_stack.frames[(thread.frame_stack.frame_index - 1) as usize];
            let class = unsafe { &*lower_frame.class };
            jvm.environment
                .clone_reference(class.class_instance.get().unwrap())
                .consume()
        }
        ("jdk/internal/util/SystemProps$Raw", "vmProperties", "()[Ljava/lang/String;") => {
            let string = jvm
                .bootstrap_classloader
                .find_class(thread, "java/lang/String")
                .unwrap();

            let props = [
                ["file.separator", "/"],
                ["java.class.path", ""],
                ["java.home", "/"],
                ["java.vendor", "birb"],
                ["user.home", "~/"],
                ["user.dir", "~/"],
                ["user.name", ""],
                ["java.io.tmpdir", "/temp/"],
                ["native.encoding", "UTF-8"],
                ["os.name", "web"],
                ["os.arch", "wasm32"],
                ["os.version", ""],
            ];

            let string_array = jvm.find_array_class(thread, string, 1);
            let properties = jvm.environment.new_object_array(&string_array, 40);

            for (index, entry) in props.iter().flatten().enumerate() {
                let string = jvm.new_string(entry, thread).consume();
                unsafe {
                    jvm.environment
                        .set_object_array_element(&properties, index as i32, string);
                }
            }

            properties.consume()
        }
        ("jdk/internal/util/SystemProps$Raw", "platformProperties", "()[Ljava/lang/String;") => {
            let string = jvm
                .bootstrap_classloader
                .find_class(thread, "java/lang/String")
                .unwrap();
            let string_array = jvm.find_array_class(thread, string, 1);
            jvm.environment
                .new_object_array(&string_array, 40)
                .consume()
        }
        ("java/lang/Object", "getClass", "()Ljava/lang/Class;") => {
            let op = std::mem::replace(&mut args[0], Object::NULL.consume());
            let object = unsafe { jvm.environment.object_from_operand(op) };
            let class = jvm.environment.get_object_class(&object);

            jvm.environment
                .clone_reference(class.class_instance.get().unwrap())
                .consume()
        }
        ("java/lang/Object", "clone", "()Ljava/lang/Object;") => {
            let op = std::mem::replace(&mut args[0], Object::NULL.consume());
            let src = unsafe { jvm.environment.object_from_operand(op) };
            let class = jvm.environment.get_object_class(&src);

            let clone;

            if class.is_array {
                //TODO: slow, implement arraycopy in wasm
                let length = jvm.environment.get_array_length(&src);

                let array_type = match &class.this_class[1..] {
                    "[" => {
                        clone = jvm.environment.new_object_array(class, length);

                        for i in 0..length {
                            let element =
                                unsafe { jvm.environment.get_object_array_element(&src, i) };
                            unsafe {
                                jvm.environment.set_object_array_element(
                                    &clone,
                                    i,
                                    element.consume(),
                                )
                            };
                        }

                        return clone.consume();
                    }
                    "B" => ArrayType::Byte,
                    "I" => ArrayType::Int,
                    _ => unimplemented!(),
                };

                clone = jvm.environment.new_array(array_type, length);

                for i in 0..length {
                    let element = unsafe { jvm.environment.get_array_element(array_type, &src, i) };
                    unsafe {
                        jvm.environment
                            .set_array_element(array_type, &clone, i, element)
                    };
                }
            } else {
                clone = jvm.environment.new_object(class);

                for field in &class.fields {
                    let op = unsafe {
                        jvm.environment.get_object_field(
                            &src,
                            &class,
                            &field.name,
                            &field.descriptor_string,
                        )
                    };
                    unsafe {
                        jvm.environment.set_object_field(
                            &src,
                            &class,
                            &field.name,
                            &field.descriptor_string,
                            op,
                        )
                    };
                }
            };

            clone.consume()
        }
        ("java/lang/Object", "hashCode", "()I") => {
            let java_lang_object = jvm
                .bootstrap_classloader
                .find_class(thread, "java/lang/Object")
                .unwrap();
            let op = std::mem::replace(&mut args[0], Object::NULL.consume());
            let object = unsafe { jvm.environment.object_from_operand(op) };

            Operand::new_data(jvm.environment.get_object_hash(&object) as u32)
        }
        ("java/lang/System", "arraycopy", "(Ljava/lang/Object;ILjava/lang/Object;II)V") => {
            let src = std::mem::replace(&mut args[0], Object::NULL.consume());
            let src = unsafe { jvm.environment.object_from_operand(src) };
            let src_class = jvm.environment.get_object_class(&src);

            let dest = std::mem::replace(&mut args[2], Object::NULL.consume());
            let dest = unsafe { jvm.environment.object_from_operand(dest) };
            let dest_class = jvm.environment.get_object_class(&dest);

            let src_pos = args[1].data() as i32;
            let dest_pos = args[3].data() as i32;
            let length = args[4].data() as i32;

            assert!(!jvm.environment.object_ref_eq(&src, &dest));

            let src_len = jvm.environment.get_array_length(&src);
            let dest_len = jvm.environment.get_array_length(&dest);

            let array_type = match &src_class.this_class[1..] {
                "[" => {
                    for i in 0..length {
                        let element =
                            unsafe { jvm.environment.get_object_array_element(&src, i + src_pos) };
                        unsafe {
                            jvm.environment.set_object_array_element(
                                &dest,
                                i + dest_pos,
                                element.consume(),
                            )
                        };
                    }

                    return Operand::new_data(0);
                }
                "B" => ArrayType::Byte,
                "I" => ArrayType::Int,
                _ => unimplemented!(),
            };

            for i in 0..length {
                let element = unsafe {
                    jvm.environment
                        .get_array_element(array_type, &src, i + src_pos)
                };
                unsafe {
                    jvm.environment
                        .set_array_element(array_type, &dest, i + dest_pos, element)
                };
            }

            Operand::new_data(0)
        }
        (
            "jdk/internal/misc/Unsafe",
            "objectFieldOffset1",
            "(Ljava/lang/Class;Ljava/lang/String;)J",
        ) => {
            let op = std::mem::replace(&mut args[1], Object::NULL.consume());
            let object = unsafe { jvm.environment.object_from_operand(op) };

            let java_lang_class = jvm
                .bootstrap_classloader
                .find_class(thread, "java/lang/Class")
                .unwrap();

            let class_ptr = unsafe {
                jvm.environment.get_object_field(
                    &object,
                    &java_lang_class,
                    "__jvm_internal_id",
                    "I",
                )
            }
            .data() as *const Class;
            let class = unsafe { &*class_ptr };

            let string_obj = std::mem::replace(&mut args[2], Object::NULL.consume());
            let string_ref = unsafe { thread.jvm.environment.object_from_operand(string_obj) };
            let string = thread.jvm.get_string(&string_ref);

            let base_index = class
                .fields
                .iter()
                .position(|field| &*field.name == &string)
                .unwrap();
            let parent_fields = class
                .parents()
                .iter()
                .map(|parent| &parent.fields)
                .flatten()
                .count();

            Operand::new_data((base_index + parent_fields) as u32)
        }
        ("jdk/internal/misc/Unsafe", "compareAndSetInt", "(Ljava/lang/Object;JII)Z") => {
            let op = std::mem::replace(&mut args[1], Object::NULL.consume());
            let object = unsafe { jvm.environment.object_from_operand(op) };

            let index = args[2].data() as usize;
            let test = args[3].data() as i32;
            let target = args[4].data() as i32;

            if jvm.environment.is_null(&object) {
                let offset = index as *mut i32;
                assert!(!offset.is_null());

                unsafe {
                    if *offset == test {
                        *offset = target;
                        Operand::new_data(1)
                    } else {
                        Operand::new_data(0)
                    }
                }
            } else {
                let obj_class = jvm.environment.get_object_class(&object);

                let (field, field_class) = [obj_class]
                    .into_iter()
                    .chain(obj_class.parents().into_iter().map(|p| &**p))
                    .map(|class| class.fields.iter().map(move |field| (field, class)))
                    .flatten()
                    .enumerate()
                    .find(|(field_index, x)| *field_index == index)
                    .map(|(_, x)| x)
                    .unwrap();

                assert_eq!(field.descriptor, FieldType::Int);

                if unsafe {
                    jvm.environment.get_object_field(
                        &object,
                        field_class,
                        &*field.name,
                        &*field.descriptor_string,
                    )
                }
                .data() as i32
                    == test
                {
                    unsafe {
                        jvm.environment.set_object_field(
                            &object,
                            field_class,
                            &*field.name,
                            &*field.descriptor_string,
                            Operand::new_data(target as u32),
                        )
                    };

                    Operand::new_data(1)
                } else {
                    Operand::new_data(0)
                }
            }
        }
        ("jdk/internal/misc/Unsafe", "compareAndSetLong", "(Ljava/lang/Object;JJJ)Z") => {
            let op = std::mem::replace(&mut args[1], Object::NULL.consume());
            let object = unsafe { jvm.environment.object_from_operand(op) };

            let index = args[2].data() as usize;
            let test = args[3].data() as i64;
            let target = args[4].data() as i64;

            if jvm.environment.is_null(&object) {
                let offset = index as *mut i64;
                assert!(!offset.is_null());

                unsafe {
                    if *offset == test {
                        *offset = target;
                        Operand::new_data(1)
                    } else {
                        Operand::new_data(0)
                    }
                }
            } else {
                let obj_class = jvm.environment.get_object_class(&object);

                let (field, field_class) = [obj_class]
                    .into_iter()
                    .chain(obj_class.parents().into_iter().map(|p| &**p))
                    .map(|class| class.fields.iter().map(move |field| (field, class)))
                    .flatten()
                    .enumerate()
                    .find(|(field_index, x)| *field_index == index)
                    .map(|(_, x)| x)
                    .unwrap();

                assert_eq!(field.descriptor, FieldType::Long);

                if unsafe {
                    jvm.environment.get_object_field(
                        &object,
                        field_class,
                        &*field.name,
                        &*field.descriptor_string,
                    )
                }
                .data() as i64
                    == test
                {
                    unsafe {
                        jvm.environment.set_object_field(
                            &object,
                            field_class,
                            &*field.name,
                            &*field.descriptor_string,
                            Operand::new_data2(target as u64),
                        )
                    };

                    Operand::new_data(1)
                } else {
                    Operand::new_data(0)
                }
            }
        }
        (
            "jdk/internal/misc/Unsafe",
            "compareAndSetReference",
            "(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z",
        ) => {
            let op = std::mem::replace(&mut args[1], Object::NULL.consume());
            let object = unsafe { jvm.environment.object_from_operand(op) };
            let obj_class = jvm.environment.get_object_class(&object);

            let index = args[2].data() as usize;

            let test = unsafe {
                jvm.environment
                    .object_from_operand(std::mem::replace(&mut args[3], Object::NULL.consume()))
            };
            let target = unsafe {
                jvm.environment
                    .object_from_operand(std::mem::replace(&mut args[4], Object::NULL.consume()))
            };

            if obj_class.is_array {
                let current = unsafe {
                    jvm.environment
                        .get_object_array_element(&object, index as i32)
                }
                .consume();
                let current_ref = unsafe { jvm.environment.object_ref_from_operand(&current) };

                if jvm.environment.object_ref_eq(&test, &current_ref) {
                    unsafe {
                        jvm.environment.set_object_array_element(
                            &object,
                            index as i32,
                            target.consume(),
                        )
                    };

                    Operand::new_data(1)
                } else {
                    Operand::new_data(0)
                }
            } else {
                let (field, field_class) = [obj_class]
                    .into_iter()
                    .chain(obj_class.parents().into_iter().map(|p| &**p))
                    .map(|class| class.fields.iter().map(move |field| (field, class)))
                    .flatten()
                    .enumerate()
                    .find(|(field_index, x)| *field_index == index)
                    .map(|(_, x)| x)
                    .unwrap();

                let current = unsafe {
                    jvm.environment.get_object_field(
                        &object,
                        field_class,
                        &*field.name,
                        &*field.descriptor_string,
                    )
                };
                let current_ref = unsafe { jvm.environment.object_ref_from_operand(&current) };

                if jvm.environment.object_ref_eq(&test, &current_ref) {
                    unsafe {
                        jvm.environment.set_object_field(
                            &object,
                            field_class,
                            &*field.name,
                            &*field.descriptor_string,
                            target.consume(),
                        )
                    };

                    Operand::new_data(1)
                } else {
                    Operand::new_data(0)
                }
            }
        }
        ("java/lang/Thread", "getNextThreadIdOffset", "()J") => {
            Operand::new_data2(&jvm.thread_ids as *const AtomicU64 as u64)
        }
        ("jdk/internal/misc/Unsafe", "getLongVolatile", "(Ljava/lang/Object;J)J") => {
            let op = std::mem::replace(&mut args[1], Object::NULL.consume());
            let object = unsafe { jvm.environment.object_from_operand(op) };

            let index = args[2].data() as usize;

            if jvm.environment.is_null(&object) {
                return Operand::new_data2(unsafe { *(index as *mut u64) });
            }

            let obj_class = jvm.environment.get_object_class(&object);

            if obj_class.is_array {
                unsafe {
                    jvm.environment
                        .get_array_element(ArrayType::Long, &object, index as i32)
                }
            } else {
                let (field, field_class) = [obj_class]
                    .into_iter()
                    .chain(obj_class.parents().into_iter().map(|p| &**p))
                    .map(|class| class.fields.iter().map(move |field| (field, class)))
                    .flatten()
                    .enumerate()
                    .find(|(field_index, x)| *field_index == index)
                    .map(|(_, x)| x)
                    .unwrap();

                unsafe {
                    jvm.environment.get_object_field(
                        &object,
                        field_class,
                        &*field.name,
                        &*field.descriptor_string,
                    )
                }
            }
        }
        ("jdk/internal/misc/Unsafe", "putLongVolatile", "(Ljava/lang/Object;JJ)V") => {
            let op = std::mem::replace(&mut args[1], Object::NULL.consume());
            let object = unsafe { jvm.environment.object_from_operand(op) };

            let long = std::mem::replace(&mut args[3], Object::NULL.consume());
            let index = args[2].data() as usize;

            if jvm.environment.is_null(&object) {
                unsafe {
                    *(index as *mut u64) = long.data();
                }

                return Operand::new_data(0);
            }

            let obj_class = jvm.environment.get_object_class(&object);

            if obj_class.is_array {
                unsafe {
                    jvm.environment
                        .set_array_element(ArrayType::Long, &object, index as i32, long);
                }
            } else {
                let (field, field_class) = [obj_class]
                    .into_iter()
                    .chain(obj_class.parents().into_iter().map(|p| &**p))
                    .map(|class| class.fields.iter().map(move |field| (field, class)))
                    .flatten()
                    .enumerate()
                    .find(|(field_index, x)| *field_index == index)
                    .map(|(_, x)| x)
                    .unwrap();

                unsafe {
                    jvm.environment.set_object_field(
                        &object,
                        field_class,
                        &*field.name,
                        &*field.descriptor_string,
                        long,
                    );
                }
            }

            Operand::new_data(0)
        }
        (
            "jdk/internal/misc/Unsafe",
            "getReferenceVolatile",
            "(Ljava/lang/Object;J)Ljava/lang/Object;",
        ) => {
            let op = std::mem::replace(&mut args[1], Object::NULL.consume());
            let object = unsafe { jvm.environment.object_from_operand(op) };

            let obj_class = jvm.environment.get_object_class(&object);

            let index = args[2].data() as usize;

            if obj_class.is_array {
                unsafe {
                    jvm.environment
                        .get_object_array_element(&object, index as i32)
                }
                .consume()
            } else {
                let (field, field_class) = [obj_class]
                    .into_iter()
                    .chain(obj_class.parents().into_iter().map(|p| &**p))
                    .map(|class| class.fields.iter().map(move |field| (field, class)))
                    .flatten()
                    .enumerate()
                    .find(|(field_index, x)| *field_index == index)
                    .map(|(_, x)| x)
                    .unwrap();

                let field_op = unsafe {
                    jvm.environment.get_object_field(
                        &object,
                        field_class,
                        &*field.name,
                        &*field.descriptor_string,
                    )
                };
                let field_ref = unsafe { jvm.environment.object_ref_from_operand(&field_op) };
                jvm.environment.clone_reference(&field_ref).consume()
            }
        }
        (
            "jdk/internal/misc/Unsafe",
            "putReferenceVolatile",
            "(Ljava/lang/Object;JLjava/lang/Object;)V",
        ) => {
            let op = std::mem::replace(&mut args[1], Object::NULL.consume());
            let object = unsafe { jvm.environment.object_from_operand(op) };
            let obj_class = jvm.environment.get_object_class(&object);

            let ref_ = std::mem::replace(&mut args[3], Object::NULL.consume());

            let index = args[2].data() as usize;

            if obj_class.is_array {
                unsafe {
                    jvm.environment
                        .set_object_array_element(&object, index as i32, ref_);
                }
            } else {
                let (field, field_class) = [obj_class]
                    .into_iter()
                    .chain(obj_class.parents().into_iter().map(|p| &**p))
                    .map(|class| class.fields.iter().map(move |field| (field, class)))
                    .flatten()
                    .enumerate()
                    .find(|(field_index, x)| *field_index == index)
                    .map(|(_, x)| x)
                    .unwrap();

                unsafe {
                    jvm.environment.set_object_field(
                        &object,
                        field_class,
                        &*field.name,
                        &*field.descriptor_string,
                        ref_,
                    );
                }
            }

            Operand::new_data(0)
        }
        ("jdk/internal/misc/Unsafe", "arrayIndexScale0", "(Ljava/lang/Class;)I") => {
            let op = std::mem::replace(&mut args[1], Object::NULL.consume());
            let object = unsafe { jvm.environment.object_from_operand(op) };

            let java_lang_class = jvm
                .bootstrap_classloader
                .find_class(thread, "java/lang/Class")
                .unwrap();

            let class_ptr = unsafe {
                jvm.environment.get_object_field(
                    &object,
                    &java_lang_class,
                    "__jvm_internal_id",
                    "I",
                )
            }
            .data() as *const Class;

            let class = unsafe { &*class_ptr };

            assert!(class.is_array);

            Operand::new_data(1)
        }
        ("jdk/internal/misc/Unsafe", "arrayBaseOffset0", "(Ljava/lang/Class;)I") => {
            let op = std::mem::replace(&mut args[1], Object::NULL.consume());
            let object = unsafe { jvm.environment.object_from_operand(op) };

            let java_lang_class = jvm
                .bootstrap_classloader
                .find_class(thread, "java/lang/Class")
                .unwrap();

            let class_ptr = unsafe {
                jvm.environment.get_object_field(
                    &object,
                    &java_lang_class,
                    "__jvm_internal_id",
                    "I",
                )
            }
            .data() as *const Class;

            let class = unsafe { &*class_ptr };

            assert!(class.is_array);

            Operand::new_data(0)
        }
        ("jdk/internal/misc/VM", "initialize", "()V") => {
            let hash_map_class = jvm
                .bootstrap_classloader
                .find_class(thread, "java/util/HashMap")
                .unwrap();
            let vm_class = jvm
                .bootstrap_classloader
                .find_class(thread, "jdk/internal/misc/VM")
                .unwrap();

            ThreadHandle::init_static(&hash_map_class, thread);
            let saved_props = jvm.environment.new_object(&hash_map_class);

            let handle = jvm.get_method("java/util/HashMap", "<init>", "()V", 0);
            drop(thread.invoke(
                &handle,
                Box::new([jvm.environment.clone_reference(&saved_props).consume()]),
            ));

            let index = vm_class
                .fields
                .iter()
                .filter(|field| field.access_flags.contains(AccessFlags::STATIC))
                .position(|field| &*field.name == "savedProps")
                .unwrap();

            unsafe {
                *(vm_class.statics as *mut Operand).add(index) = saved_props.consume();
            }

            Operand::new_data(0)
        }
        ("sun/io/Win32ErrorMode", "setErrorMode", "(J)J") => Operand::new_data2(0),
        ("java/lang/Runtime", "availableProcessors", "()I") => Operand::new_data(1),
        ("java/lang/Thread", "currentThread", "()Ljava/lang/Thread;") => jvm
            .environment
            .clone_reference(thread.thread_obj.get().unwrap())
            .consume(),
        ("java/lang/Class", "desiredAssertionStatus0", "(Ljava/lang/Class;)Z") => {
            Operand::new_data(0)
        }
        ("java/lang/System", "setOut0", "(Ljava/io/PrintStream;)V") => {
            let system_class = jvm
                .bootstrap_classloader
                .find_class(thread, "java/lang/System")
                .unwrap();

            let op = std::mem::replace(&mut args[0], Object::NULL.consume());
            let object = unsafe { jvm.environment.object_from_operand(op) };

            let index = system_class
                .fields
                .iter()
                .filter(|field| field.access_flags.contains(AccessFlags::STATIC))
                .position(|field| &*field.name == "out")
                .unwrap();

            unsafe {
                *(system_class.statics as *mut Operand).add(index) = object.consume();
            }

            Operand::new_data(0)
        }
        ("java/io/FileDescriptor", "initIDs", "()V") => {
            let fd_class = jvm
                .bootstrap_classloader
                .find_class(thread, "java/io/FileDescriptor")
                .unwrap();

            let fds = [("out", 0), ("in", 1), ("err", 2)];

            let handle = jvm.get_method("java/io/FileDescriptor", "<init>", "(I)V", 0);

            for (name, val) in fds {
                let fd = jvm.environment.new_object(&fd_class);

                drop(thread.invoke(
                    &handle,
                    Box::new([
                        jvm.environment.clone_reference(&fd).consume(),
                        Operand::new_data(val),
                    ]),
                ));

                let index = fd_class
                    .fields
                    .iter()
                    .filter(|field| field.access_flags.contains(AccessFlags::STATIC))
                    .position(|field| &*field.name == name)
                    .unwrap();

                unsafe {
                    *(fd_class.statics as *mut Operand).add(index) = fd.consume();
                }
            }

            Operand::new_data(0)
        }
        _ => {
            console_log!(
                "unimplemented native method {} {:?}",
                method_handle.class.this_class,
                method_handle.method
            );
            Operand::new_data(0)
        }
    }
}
