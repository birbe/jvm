use crate::env::Object;
use crate::thread::Operand;
use byteorder::{BigEndian, ReadBytesExt};
use jvm_types::JParse;
use parse_macro::JParse;
use std::fmt::{Display, Formatter};
use std::io::{Error, Read, Seek};
use std::ops::Deref;

#[derive(JParse, Copy, Debug, Clone, PartialEq)]
pub struct LookupEntry {
    pub key: i32,
    pub offset: i32,
}

#[derive(Debug)]
pub enum JValue {
    Reference(Object),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    Short(i16),
    Byte(i8),
}

impl JValue {
    pub fn as_operand(&self) -> Operand {
        match self {
            JValue::Reference(object) => Operand {
                objectref: object.ptr,
            },
            JValue::Int(int) => Operand { data: *int as u64 },
            JValue::Long(long) => Operand { data: *long as u64 },
            JValue::Float(float) => Operand {
                data: u32::from_ne_bytes(float.to_ne_bytes()) as u64,
            },
            JValue::Double(double) => Operand {
                data: u64::from_ne_bytes(double.to_ne_bytes()),
            },
            JValue::Short(short) => Operand {
                data: *short as u64,
            },
            JValue::Byte(byte) => Operand { data: *byte as u64 },
        }
    }
}

#[derive(Debug)]
pub enum BytecodeParseError {
    InvalidOpcode,
    IOError(std::io::Error),
}

impl From<std::io::Error> for BytecodeParseError {
    fn from(value: Error) -> Self {
        Self::IOError(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
#[allow(non_camel_case_types)]
pub enum Bytecode {
    Aaload,                              //0x32
    Aastore,                             //0x53
    Aconst_null,                         //0x1
    Aload(u8),                           //0x19
    Aload_n(u8),                         //0x2a - 0x2d
    Anewarray(u16),                      //0xbd
    Areturn,                             //0xb0
    Arraylength,                         //0xbe
    Astore(u8),                          //0x3a
    Astore_n(u8),                        //0x4b - 0x4e
    Athrow,                              //0xbf
    Baload,                              //0x33
    Bastore,                             //0x54
    Bipush(i8),                          //0x10
    Caload,                              //0x34
    Castore,                             //0x55
    Checkcast(u16),                      //0xc0
    D2f,                                 //0x90
    D2i,                                 //0x8e
    D2l,                                 //0x8f
    Dadd,                                //0x63
    Daload,                              //0x31
    Dastore,                             //0x52
    Dcmpg,                               //0x98
    Dcmpl,                               //0x97
    Dconst_n(u8),                        //0xe - 0xf
    Ddiv,                                //0x6f
    Dload(u8),                           //0x18
    Dload_n(u8),                         //0x26 - 0x29
    Dmul,                                //0x6b
    Dneg,                                //0x77
    Drem,                                //0x73
    Dreturn,                             //0xaf
    Dstore(u8),                          //0x39
    Dstore_n(u8),                        //0x47 - 0x4a
    Dsub,                                //0x67
    Dup,                                 //0x59
    Dup_x2,                              //0x5b
    Dup2,                                //0x5c
    Dup2_x1,                             //0x5d
    Dup2_x2,                             //0x5e
    F2d,                                 //0x8d
    F2i,                                 //0x8b
    F2l,                                 //0x8c
    Fadd,                                //0x62
    Faload,                              //0x30
    Fastore,                             //0x51
    Fcmpg,                               //0x96
    Fcmpl,                               //0x95
    Fconst_n(u8),                        //0xb - 0xd
    Fdiv,                                //0x6e
    Fload(u8),                           //0x17
    Fload_n(u8),                         //0x22 - 0x25
    Fmul,                                //0x6a
    Fneg,                                //0x76
    Frem,                                //0x72
    Freturn,                             //0xae
    Fstore(u8),                          //0x38
    Fstore_n(u8),                        //0x43 - 0x46
    Fsub,                                //0x66
    Getfield(u16),                       //0xb4
    Getstatic(u16),                      //0xb2
    Goto(i16),                           //0xa7
    Goto_w(i32),                         //0xc8
    I2b,                                 //0x91
    I2c,                                 //0x92
    I2d,                                 //0x87
    I2f,                                 //0x86
    I2l,                                 //0x85
    I2s,                                 //0x93
    Iadd,                                //0x60
    Iaload,                              //0x2e
    Iand,                                //0x7e
    Iastore,                             //0x4f
    Iconst_n_m1(i8),                     //0x2 - 0x8 (minus 1)
    Idiv,                                //0x6c
    If_acmpeq(i16),                      //0xa5
    If_acmpne(i16),                      //0xa6
    If_icmpeq(i16),                      //0x9f
    If_icmpne(i16),                      //0xa0
    If_icmplt(i16),                      //0xa1
    If_icmpge(i16),                      //0xa2
    If_icmpgt(i16),                      //0xa3
    If_icmple(i16),                      //0xa4
    Ifeq(i16),                           //0x99
    Ifne(i16),                           //0x9a
    Iflt(i16),                           //0x9b
    Ifge(i16),                           //0x9c
    Ifgt(i16),                           //0x9d
    Ifle(i16),                           //0x9e
    Ifnonnull(i16),                      //0xc7
    Ifnull(i16),                         //0xc6
    Iinc(u8, i8),                        //0x84
    Iload(u8),                           //0x15
    Iload_n(u8),                         //0x1a - 0x1d
    Imul,                                //0x68
    Ineg,                                //0x74
    Instanceof(u16),                     //0xc1
    Invokedynamic(u16),                  //0xba
    Invokeinterface(u16, u8),            //0xb9
    Invokespecial(u16),                  //0xb7
    Invokestatic(u16),                   //0xb8
    Invokevirtual(u16),                  //0xb6
    Ior,                                 //0x80
    Irem,                                //0x70
    Ireturn,                             //0xac
    Ishl,                                //0x78
    Ishr,                                //0x7a
    Istore(u8),                          //0x36
    Istore_n(u8),                        //0x3b - 0x3e
    Isub,                                //0x64
    Iushr,                               //0x7c
    Ixor,                                //0x82
    Jsr(u16),                            //0xa8
    Jsr_w(u32),                          //0xc9
    L2d,                                 //0x8a
    L2f,                                 //0x89
    L2i,                                 //0x88
    Ladd,                                //0x61
    Laload,                              //0x2f
    Land,                                //0x7f
    Lastore,                             //0x50
    Lcmp,                                //0x94
    Lconst_n(u8),                        //0x9 - 0xa
    Ldc(u8),                             //0x12
    Ldc_w(u16),                          //0x13
    Ldc2_w(u16),                         //0x14
    Ldiv,                                //0x6d
    Lload(u8),                           //0x16
    Lload_n(u8),                         //0x1e - 0x21
    Lmul,                                //0x69
    Lneg,                                //0x75
    Lookupswitch(i32, Vec<LookupEntry>), //0xab
    Lor,                     //0x81
    Lrem,                    //0x71
    Lreturn,                 //0xad
    Lshl,                    //0x79
    Lshr,                    //0x7b
    Lstore,                  //0x37
    Lstore_n(u8),            //0x3f - 0x42
    Lsub,                    //0x65
    Lushr,                   //0x7d
    Lxor,                    //0x83
    Monitorenter,            //0xc2
    Monitorexit,             //0xc3
    Multianewarray(u16, u8), //0xc5
    New(u16),                //0xbb
    Newarray(u8),            //0xbc
    Nop,                     //0x0
    Pop,                     //0x57
    Pop2,                    //0x58
    Putfield(u16),           //0xb5
    Putstatic(u16),          //0xb3
    Ret,                     //0xa9
    Return,                  //0xb1
    Saload,                  //0x35
    Sastore,                 //0x56
    Sipush(i16),             //0x11
    Swap,                    //0x5f
    Tableswitch,             //0xaa
    Wide(Vec<u8>),           //0xc4
}

impl Display for Bytecode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Bytecode {
    pub fn from_bytes<G: Clone + Read, R: Read + Seek + Deref<Target = G>>(
        mut reader: R,
    ) -> Result<Self, BytecodeParseError> {
        let opcode = reader.read_u8()?;

        let bytecode = match opcode {
            0x32 => Self::Aaload,
            0x53 => Self::Aastore,
            0x1 => Self::Aconst_null,
            0x19 => Self::Aload(reader.read_u8()?),
            0x2a..=0x2d => Self::Aload_n(opcode - 0x2a),
            0xbd => Self::Anewarray(reader.read_u16::<BigEndian>()?),
            0xb0 => Self::Areturn,
            0xbe => Self::Arraylength,
            0x3a => Self::Astore(reader.read_u8()?),
            0x4b..=0x4e => Self::Astore_n(opcode - 0x4b),
            0xbf => Self::Athrow,
            0x33 => Self::Baload,
            0x54 => Self::Bastore,
            0x10 => Self::Bipush(reader.read_i8()?),
            0x34 => Self::Caload,
            0x55 => Self::Castore,
            0xc0 => Self::Checkcast(reader.read_u16::<BigEndian>()?),
            0x90 => Self::D2f,
            0x8e => Self::D2i,
            0x8f => Self::D2l,
            0x63 => Self::Dadd,
            0x31 => Self::Daload,
            0x52 => Self::Dastore,
            0x98 => Self::Dcmpg,
            0x97 => Self::Dcmpl,
            0xe..=0xf => Self::Dconst_n(opcode - 0x3),
            0x6f => Self::Ddiv,
            0x18 => Self::Dload(reader.read_u8()?),
            0x26..=0x29 => Self::Dload_n(opcode - 0x26),
            0x6b => Self::Dmul,
            0x77 => Self::Dneg,
            0x73 => Self::Drem,
            0xaf => Self::Dreturn,
            0x39 => Self::Dstore(reader.read_u8()?),
            0x47..=0x4a => Self::Dstore_n(opcode - 0x47),
            0x67 => Self::Dsub,
            0x59 => Self::Dup,
            0x5b => Self::Dup_x2,
            0x5c => Self::Dup2,
            0x5d => Self::Dup2_x1,
            0x5e => Self::Dup2_x2,
            0x8d => Self::F2d,
            0x8b => Self::F2i,
            0x8c => Self::F2l,
            0x62 => Self::Fadd,
            0x30 => Self::Faload,
            0x51 => Self::Fastore,
            0x96 => Self::Fcmpg,
            0x95 => Self::Fcmpl,
            0xb..=0xd => Self::Fconst_n(opcode - 0xb),
            0x6e => Self::Fdiv,
            0x17 => Self::Fload(reader.read_u8()?),
            0x22..=0x25 => Self::Fload_n(opcode - 0x22),
            0x6a => Self::Fmul,
            0x76 => Self::Fneg,
            0x72 => Self::Frem,
            0xae => Self::Freturn,
            0x38 => Self::Fstore(reader.read_u8()?),
            0x43..=0x46 => Self::Fstore_n(opcode - 0x43),
            0x66 => Self::Fsub,
            0xb4 => Self::Getfield(reader.read_u16::<BigEndian>()?),
            0xb2 => Self::Getstatic(reader.read_u16::<BigEndian>()?),
            0xa7 => Self::Goto(reader.read_i16::<BigEndian>()?),
            0xc8 => Self::Goto_w(reader.read_i32::<BigEndian>()?),
            0x91 => Self::I2b,
            0x92 => Self::I2c,
            0x87 => Self::I2d,
            0x86 => Self::I2f,
            0x85 => Self::I2l,
            0x93 => Self::I2s,
            0x60 => Self::Iadd,
            0x2e => Self::Iaload,
            0x7e => Self::Iand,
            0x4f => Self::Iastore,
            0x2..=0x8 => Self::Iconst_n_m1(((opcode as i16) - 0x3) as i8),
            0x6c => Self::Idiv,
            0xa5 => Self::If_acmpeq(reader.read_i16::<BigEndian>()?),
            0xa6 => Self::If_acmpne(reader.read_i16::<BigEndian>()?),
            0x9f => Self::If_icmpeq(reader.read_i16::<BigEndian>()?),
            0xa0 => Self::If_icmpne(reader.read_i16::<BigEndian>()?),
            0xa1 => Self::If_icmplt(reader.read_i16::<BigEndian>()?),
            0xa2 => Self::If_icmpge(reader.read_i16::<BigEndian>()?),
            0xa3 => Self::If_icmpgt(reader.read_i16::<BigEndian>()?),
            0xa4 => Self::If_icmple(reader.read_i16::<BigEndian>()?),
            0x99 => Self::Ifeq(reader.read_i16::<BigEndian>()?),
            0x9a => Self::Ifne(reader.read_i16::<BigEndian>()?),
            0x9b => Self::Iflt(reader.read_i16::<BigEndian>()?),
            0x9c => Self::Ifge(reader.read_i16::<BigEndian>()?),
            0x9d => Self::Ifgt(reader.read_i16::<BigEndian>()?),
            0x9e => Self::Ifle(reader.read_i16::<BigEndian>()?),
            0xc7 => Self::Ifnonnull(reader.read_i16::<BigEndian>()?),
            0xc6 => Self::Ifnull(reader.read_i16::<BigEndian>()?),
            0x84 => Self::Iinc(reader.read_u8()?, reader.read_i8()?),
            0x15 => Self::Iload(reader.read_u8()?),
            0x1a..=0x1d => Self::Iload_n(opcode - 0x1a),
            0x68 => Self::Imul,
            0x74 => Self::Ineg,
            0xc1 => Self::Instanceof(reader.read_u16::<BigEndian>()?),
            0xba => {
                let out = Self::Invokedynamic(reader.read_u16::<BigEndian>()?);
                reader.read_u16::<BigEndian>()?;
                out
            }
            0xb9 => Self::Invokeinterface(reader.read_u16::<BigEndian>()?, reader.read_u8()?),
            0xb7 => Self::Invokespecial(reader.read_u16::<BigEndian>()?),
            0xb8 => Self::Invokestatic(reader.read_u16::<BigEndian>()?),
            0xb6 => Self::Invokevirtual(reader.read_u16::<BigEndian>()?),
            0x80 => Self::Ior,
            0x70 => Self::Irem,
            0xac => Self::Ireturn,
            0x78 => Self::Ishl,
            0x7a => Self::Ishr,
            0x36 => Self::Istore(reader.read_u8()?),
            0x3b..=0x3e => Self::Istore_n(opcode - 0x3b),
            0x64 => Self::Isub,
            0x7c => Self::Iushr,
            0x82 => Self::Ixor,
            0xa8 => Self::Jsr(reader.read_u16::<BigEndian>()?),
            0xc9 => Self::Jsr_w(reader.read_u32::<BigEndian>()?),
            0x8a => Self::L2d,
            0x89 => Self::L2f,
            0x88 => Self::L2i,
            0x61 => Self::Ladd,
            0x2f => Self::Laload,
            0x7f => Self::Land,
            0x50 => Self::Lastore,
            0x94 => Self::Lcmp,
            0x9..=0xa => Self::Lconst_n(opcode - 0x9),
            0x12 => Self::Ldc(reader.read_u8()?),
            0x13 => Self::Ldc_w(reader.read_u16::<BigEndian>()?),
            0x14 => Self::Ldc2_w(reader.read_u16::<BigEndian>()?),
            0x6d => Self::Ldiv,
            0x16 => Self::Lload(reader.read_u8()?),
            0x1e..=0x21 => Self::Lload_n(opcode - 0x1e),
            0x69 => Self::Lmul,
            0x75 => Self::Lneg,
            0xab => {
                #[derive(JParse)]
                struct LookupEntries {
                    #[prefix = 4]
                    entries: Vec<LookupEntry>
                }

                let position = reader.stream_position().unwrap();
                let padding = (position.next_multiple_of(2) - position) as usize;
                reader.read(&mut vec![0; padding])?;

                let default = reader.read_i32::<BigEndian>()?;
                let entries = LookupEntries::from_bytes(&mut reader)?.entries;

                Self::Lookupswitch(default, entries)
            },
            0x81 => Self::Lor,
            0x71 => Self::Lrem,
            0xad => Self::Lreturn,
            0x79 => Self::Lshl,
            0x7b => Self::Lshr,
            0x37 => Self::Lstore,
            0x3f..=0x42 => Self::Lstore_n(opcode - 0x3f),
            0x65 => Self::Lsub,
            0x7d => Self::Lushr,
            0x83 => Self::Lxor,
            0xc2 => Self::Monitorenter,
            0xc3 => Self::Monitorexit,
            0xc5 => Self::Multianewarray(reader.read_u16::<BigEndian>()?, reader.read_u8()?),
            0xbb => Self::New(reader.read_u16::<BigEndian>()?),
            0xbc => Self::Newarray(reader.read_u8()?),
            0x0 => Self::Nop,
            0x57 => Self::Pop,
            0x58 => Self::Pop2,
            0xb5 => Self::Putfield(reader.read_u16::<BigEndian>()?),
            0xb3 => Self::Putstatic(reader.read_u16::<BigEndian>()?),
            0xa9 => Self::Ret,
            0xb1 => Self::Return,
            0x35 => Self::Saload,
            0x56 => Self::Sastore,
            0x11 => Self::Sipush(reader.read_i16::<BigEndian>()?),
            0x5f => Self::Swap,
            0xaa => Self::Tableswitch,
            0xcf => Self::Wide(Vec::new()),
            _ => return Err(BytecodeParseError::InvalidOpcode),
        };

        Ok(bytecode)
    }
}
