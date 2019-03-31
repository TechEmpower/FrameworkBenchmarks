#![allow(dead_code)]
use std::{cmp, io};

use bytes::BytesMut;

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

pub fn get_query_param(query: &str) -> u16 {
    let q = if let Some(pos) = query.find("q") {
        query.split_at(pos + 2).1.parse::<u16>().ok().unwrap_or(1)
    } else {
        1
    };
    cmp::min(500, cmp::max(1, q))
}

fn escapable(b: u8) -> bool {
    match b {
        b'<' | b'>' | b'&' | b'"' | b'\'' | b'/' => true,
        _ => false,
    }
}

pub fn escape<T: io::Write>(writer: &mut T, s: String) {
    let bytes = s.as_bytes();
    let mut last_pos = 0;
    for (idx, b) in s.as_bytes().iter().enumerate() {
        if escapable(*b) {
            let _ = writer.write(&bytes[last_pos..idx]);

            last_pos = idx + 1;
            match *b {
                b'<' => {
                    let _ = writer.write(b"&lt;");
                }
                b'>' => {
                    let _ = writer.write(b"&gt;");
                }
                b'&' => {
                    let _ = writer.write(b"&amp;");
                }
                b'"' => {
                    let _ = writer.write(b"&quot;");
                }
                b'\'' => {
                    let _ = writer.write(b"&#x27;");
                }
                b'/' => {
                    let _ = writer.write(b"&#x2f;");
                }
                _ => panic!("incorrect indexing"),
            }
        }
    }
    if last_pos < bytes.len() - 1 {
        let _ = writer.write(&bytes[last_pos..]);
    }
}
