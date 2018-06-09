#![allow(dead_code)]
use bytes::BytesMut;
use std::{fmt, io};

pub const SIZE: usize = 31;

#[derive(Serialize, Deserialize)]
pub struct Message {
    pub message: &'static str,
}

pub struct Writer<'a>(pub &'a mut BytesMut);

impl<'a> io::Write for Writer<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.extend_from_slice(buf);
        Ok(buf.len())
    }
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

pub struct StackWriter<'a>(pub &'a mut [u8], pub usize);

impl<'a> io::Write for StackWriter<'a> {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let l = buf.len();
        let new = self.1 + l;
        self.0[self.1..new].copy_from_slice(buf);
        self.1 = new;
        Ok(l)
    }
    #[inline]
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl<'a> fmt::Write for StackWriter<'a> {
    #[inline]
    fn write_str(&mut self, s: &str) -> Result<(), fmt::Error> {
        if !s.is_empty() {
            let b = s.as_bytes();
            let l = b.len();
            let new = self.1 + l;
            self.0[self.1..new].copy_from_slice(b);
            self.1 = new;
        }
        Ok(())
    }
}
