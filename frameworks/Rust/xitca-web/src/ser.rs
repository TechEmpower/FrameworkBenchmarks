use std::{borrow::Cow, cell::RefMut, io};

use bytes::{Bytes, BytesMut};

use sailfish::TemplateOnce;
use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize)]
pub struct Message {
    message: &'static str,
}

impl Message {
    #[inline]
    pub fn new() -> Self {
        Self {
            message: "Hello, World!",
        }
    }
}

#[allow(non_snake_case)]
#[derive(Serialize, Debug)]
pub struct World {
    pub id: i32,
    pub randomnumber: i32,
}

impl World {
    #[inline]
    pub fn new(id: i32, randomnumber: i32) -> Self {
        Self { id, randomnumber }
    }
}

pub struct Fortune {
    pub id: i32,
    pub message: Cow<'static, str>,
}

impl Fortune {
    #[inline]
    pub fn new(id: i32, message: impl Into<Cow<'static, str>>) -> Self {
        Self {
            id,
            message: message.into(),
        }
    }
}

#[derive(TemplateOnce)]
#[template(path = "fortune.stpl", rm_whitespace = true)]
pub struct Fortunes {
    items: Vec<Fortune>,
}

impl Fortunes {
    #[inline]
    pub fn new(items: Vec<Fortune>) -> Self {
        Self { items }
    }
}

pub struct Writer<'a>(pub RefMut<'a, BytesMut>);

impl Writer<'_> {
    #[inline]
    pub fn take(mut self) -> Bytes {
        self.0.split().freeze()
    }
}

impl io::Write for &mut Writer<'_> {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.extend_from_slice(buf);
        Ok(buf.len())
    }

    #[inline]
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}
