use std::io::{Error, Read};
use byteorder::ReadBytesExt;
use paste::paste;

pub trait JParse {

    type Output;

    fn from_bytes<R: Read>(mut r: R) -> Result<Self::Output, Error> {
        Self::from_bytes_prefixed::<R, 2>(r)
    }

    fn from_bytes_prefixed<R: Read, const prefix: usize>(r: R) -> Result<Self::Output, Error>;

    fn to_bytes(&self) -> Vec<u8> {
        self.to_bytes_prefixed::<2>()
    }

    fn to_bytes_prefixed<const prefix: usize>(&self) -> Vec<u8>;

}

macro_rules! impl_jparse {

    ($name:tt) => {

        paste! {

            impl JParse for $name {

                type Output = $name;

                fn from_bytes_prefixed<R: std::io::Read, const prefix: usize>(mut r: R) -> Result<Self::Output, std::io::Error> {
                    use byteorder::BigEndian;
                    r. [< read_ $name >]::<BigEndian>()
                }

                fn to_bytes_prefixed<const prefix: usize>(&self) -> Vec<u8> {
                    self.to_be_bytes().into()
                }

            }

        }


    }

}

impl_jparse!(u16);
impl_jparse!(i16);
impl_jparse!(u32);
impl_jparse!(i32);
impl_jparse!(u64);
impl_jparse!(i64);

impl<T> JParse for Vec<T> where T: JParse<Output = T> {
    type Output = Vec<T>;

    fn from_bytes_prefixed<R: Read, const prefix: usize>(mut r: R) -> Result<Vec<T>, Error> {
        assert_ne!(prefix, 0, "You can't directly nest `Vec<T>`s in a JParse derive, please use a wrapper struct");

        let mut buf = [0; 8];
        r.read(&mut buf[8-prefix..8])?;

        let length = u64::from_be_bytes(buf);

        let mut out = Vec::new();

        for _ in 0..length {
            out.push(T::from_bytes_prefixed::<_, 0>(&mut r)?)
        }

        Ok(out)
    }

    fn to_bytes_prefixed<const prefix: usize>(&self) -> Vec<u8> {
        assert_ne!(prefix, 0, "You can't directly nest `Vec<T>`s in a JParse derive, please use a wrapper struct");

        let mut out = Vec::new();

        let mut length = self.len().to_be_bytes();

        out.extend(&length[8-prefix..8]);

        for item in self {
            out.extend(item.to_bytes_prefixed::<0>())
        }

        out
    }

}