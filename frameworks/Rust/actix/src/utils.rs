#![allow(dead_code)]
use std::{cmp, io};

use bytes::{BufMut, BytesMut};

#[allow(non_snake_case)]
#[derive(Serialize, Debug)]
pub struct Fortune {
    pub id: i32,
    pub message: String,
}

pub const SIZE: usize = 27;

#[derive(Serialize, Deserialize)]
pub struct Message {
    pub message: &'static str,
}

pub struct Writer<'a>(pub &'a mut BytesMut);

impl<'a> io::Write for Writer<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.put_slice(buf);
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

pub fn escape(writer: &mut Writer, s: String) {
    let bytes = s.as_bytes();
    let mut last_pos = 0;
    for (idx, b) in s.as_bytes().iter().enumerate() {
        if escapable(*b) {
            let _ = writer.0.put_slice(&bytes[last_pos..idx]);

            last_pos = idx + 1;
            match *b {
                b'<' => {
                    let _ = writer.0.put_slice(b"&lt;");
                }
                b'>' => {
                    let _ = writer.0.put_slice(b"&gt;");
                }
                b'&' => {
                    let _ = writer.0.put_slice(b"&amp;");
                }
                b'"' => {
                    let _ = writer.0.put_slice(b"&quot;");
                }
                b'\'' => {
                    let _ = writer.0.put_slice(b"&#x27;");
                }
                b'/' => {
                    let _ = writer.0.put_slice(b"&#x2f;");
                }
                _ => panic!("incorrect indexing"),
            }
        }
    }
    if last_pos < bytes.len() - 1 {
        let _ = writer.0.put_slice(&bytes[last_pos..]);
    }
}

markup::define! {
    FortunesTemplate(fortunes: Vec<Fortune>) {
        {markup::doctype()}
        html {
            head {
                title { "Fortunes" }
            }
            body {
                table {
                    tr { th { "id" } th { "message" } }
                    @for item in {fortunes} {
                        tr {
                            td { {item.id} }
                            td { {markup::raw(v_htmlescape::escape(&item.message))} }
                        }
                    }
                }
            }
        }
    }
}
