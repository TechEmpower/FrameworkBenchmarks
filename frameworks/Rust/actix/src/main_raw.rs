#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate diesel;

use std::io;

use actix_codec::{AsyncRead, AsyncWrite, Decoder};
use actix_http::h1;
use actix_http::Request;
use actix_server::{Io, Server};
use actix_service::service_fn;
use bytes::{BufMut, BytesMut};
use futures::{Async, Future, Poll};
use serde_json::to_writer;
use tokio_tcp::TcpStream;

mod models;
mod utils;

use crate::utils::{Message, Writer};

const HTTPOK: &[u8] = b"HTTP/1.1 200 OK\r\n";
const HTTPNFOUND: &[u8] = b"HTTP/1.1 400 OK\r\n";
const HDR_SERVER: &[u8] = b"Server: Actix\r\n";
const HDR_CTPLAIN: &[u8] = b"Content-Type: text/plain\r\n";
const HDR_CTJSON: &[u8] = b"Content-Type: application/json\r\n";
const HDR_PL_LEN: &[u8] = b"Content-Length: 13\r\n";
const HDR_JS_LEN: &[u8] = b"Content-Length: 27\r\n";
const BODY: &[u8] = b"Hello, World!";

struct App<T> {
    io: T,
    read_buf: BytesMut,
    write_buf: BytesMut,
    codec: h1::Codec,
}

impl<T: AsyncRead + AsyncWrite> App<T> {
    fn handle_request(&mut self, req: Request) {
        let path = req.path();
        match path {
            "/json" => {
                let message = Message {
                    message: "Hello, World!",
                };
                self.write_buf.put_slice(HTTPOK);
                self.write_buf.put_slice(HDR_SERVER);
                self.write_buf.put_slice(HDR_CTJSON);
                self.write_buf.put_slice(HDR_JS_LEN);
                self.codec.config().set_date(&mut self.write_buf);
                to_writer(Writer(&mut self.write_buf), &message).unwrap();
            }
            "/plaintext" => {
                self.write_buf.put_slice(HTTPOK);
                self.write_buf.put_slice(HDR_SERVER);
                self.write_buf.put_slice(HDR_CTPLAIN);
                self.write_buf.put_slice(HDR_PL_LEN);
                self.codec.config().set_date(&mut self.write_buf);
                self.write_buf.put_slice(BODY);
            }
            _ => {
                self.write_buf.put_slice(HTTPNFOUND);
                self.write_buf.put_slice(HDR_SERVER);
            }
        }
    }
}

impl<T: AsyncRead + AsyncWrite> Future for App<T> {
    type Item = ();
    type Error = ();

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        loop {
            if self.read_buf.remaining_mut() < 4096 {
                self.read_buf.reserve(32_768);
            }
            let read = unsafe { self.io.read(self.read_buf.bytes_mut()) };
            match read {
                Ok(0) => return Ok(Async::Ready(())),
                Ok(n) => unsafe { self.read_buf.advance_mut(n) },
                Err(e) => {
                    if e.kind() == io::ErrorKind::WouldBlock {
                        break;
                    } else {
                        return Err(());
                    }
                }
            }
        }

        if self.write_buf.remaining_mut() < 8192 {
            self.write_buf.reserve(32_768);
        }

        loop {
            match self.codec.decode(&mut self.read_buf) {
                Ok(Some(h1::Message::Item(req))) => self.handle_request(req),
                Ok(None) => break,
                _ => return Err(()),
            }
        }

        if !self.write_buf.is_empty() {
            let len = self.write_buf.len();
            let mut written = 0;
            while written < len {
                match self.io.write(&self.write_buf[written..]) {
                    Ok(0) => return Ok(Async::Ready(())),
                    Ok(n) => {
                        written += n;
                    }
                    Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                        if written > 0 {
                            let _ = self.write_buf.split_to(written);
                        }
                        break;
                    }
                    Err(_) => return Err(()),
                }
            }
            if written > 0 {
                if written == len {
                    unsafe { self.write_buf.set_len(0) }
                } else {
                    let _ = self.write_buf.split_to(written);
                }
            }
        }
        Ok(Async::NotReady)
    }
}

fn main() -> io::Result<()> {
    let sys = actix_rt::System::builder().stop_on_panic(false).build();

    // start http server
    Server::build()
        .backlog(1024)
        .bind("techempower", "0.0.0.0:8080", || {
            service_fn(|io: Io<TcpStream, _>| App {
                io: io.into_parts().0,
                read_buf: BytesMut::with_capacity(32_768),
                write_buf: BytesMut::with_capacity(32_768),
                codec: h1::Codec::default(),
            })
        })?
        .start();

    println!("Started http server: 127.0.0.1:8080");
    sys.run()
}
