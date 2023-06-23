use crate::JVMInternal;
use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::mem::MaybeUninit;
use std::pin::Pin;

pub enum ThreadError {
    UnresolvedClassDefinition,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct MethodIdentifier<'jvm> {
    pub class: &'jvm CString,
    pub method: &'jvm CString,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct Frame<'jvm> {
    pub method_identifier: MethodIdentifier<'jvm>,
    pub program_counter: u32,
}

pub struct FrameStore<'jvm> {
    pub frames: Pin<Box<[MaybeUninit<Frame<'jvm>>; 1024]>>,
    pub frame_index: isize,
}

impl<'jvm> FrameStore<'jvm> {
    pub fn push(&mut self, frame: Frame<'jvm>) {
        self.frame_index += 1;
        self.frames[self.frame_index as usize] = MaybeUninit::new(frame);
    }

    pub fn pop(&mut self) {
        assert!(self.frame_index > 0);
        self.frames[self.frame_index as usize] = MaybeUninit::uninit();
        self.frame_index -= 1;
    }
}

pub enum ThreadStepResult {
    Ok,
    Error(ThreadError),
    Result(i64),
}

pub struct Thread {}

impl Thread {
    //Conforms to the ABI
    pub unsafe fn interpret_trampoline(
        args_length: u64,
        args_ptr: *const i32,
        frames_length: u64,
        frames_ptr: *mut Frame,
        jvm: *mut JVMInternal,
    ) -> i64 {
        let args = std::slice::from_raw_parts(args_ptr, args_length as usize);
        let frames = std::slice::from_raw_parts_mut(
            frames_ptr.offset(frames_length as isize) as *mut MaybeUninit<Frame>,
            1024 - (frames_length as usize),
        );

        assert!(frames_length > 0);
        Self::interpret(
            args,
            &mut *frames_ptr.offset(frames_length as isize - 1),
            frames,
        )
        .unwrap_or(0)
    }

    pub fn interpret(
        args: &[i32],
        frame: &mut Frame,
        frames: &mut [MaybeUninit<Frame>],
    ) -> Option<i64> {
        let return_value = loop {
            match Self::step(args, frame, frames) {
                ThreadStepResult::Ok => {}
                ThreadStepResult::Error(_) => panic!(),
                ThreadStepResult::Result(result) => {
                    break result;
                }
            }
        };

        None
    }

    pub fn step(
        args: &[i32],
        frame: &mut Frame,
        frames: &mut [MaybeUninit<Frame>],
    ) -> ThreadStepResult {
        let pc = frame.program_counter;

        frame.program_counter += 1;

        ThreadStepResult::Ok
    }
}
