#![allow(dead_code)]
use std::{cmp, io};

use atoi::FromRadix10;
use bytes::BytesMut;
use serde_derive::Serialize;
use yarte::TemplateFixed;

#[allow(non_snake_case)]
#[derive(Serialize, Debug)]
pub struct Fortune {
    pub id: i32,
    pub message: String,
}

pub const SIZE: usize = 27;

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

pub fn get_query_param(query: &str) -> u16 {
    let q = if let Some(pos) = query.find("q") {
        u16::from_radix_10(query.split_at(pos + 2).1.as_ref()).0
    } else {
        1
    };
    cmp::min(500, cmp::max(1, q))
}

#[derive(TemplateFixed)]
#[template(path = "fortune.hbs")]
pub struct FortunesYarteTemplate {
    pub fortunes: Vec<Fortune>,
}
