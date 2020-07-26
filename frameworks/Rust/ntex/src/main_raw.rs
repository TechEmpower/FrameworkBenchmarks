#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

use std::future::Future;
use std::io;
use std::pin::Pin;
use std::task::{Context, Poll};

use bytes::{Buf, BytesMut};
use ntex::codec::{AsyncRead, AsyncWrite, Decoder};
use ntex::fn_service;
use ntex::http::{h1, Request};
use ntex::rt::net::TcpStream;
use yarte::Serialize;

mod utils;

const JSON: &[u8] = b"HTTP/1.1 200 OK\r\nServer: N\r\nContent-Type: application/json\r\nContent-Length: 27\r\n";
const PLAIN: &[u8] = b"HTTP/1.1 200 OK\r\nServer: N\r\nContent-Type: text/plain\r\nContent-Length: 13\r\n";
const HTTPNFOUND: &[u8] = b"HTTP/1.1 400 OK\r\n";
const HDR_SERVER: &[u8] = b"Server: N\r\n";
const BODY: &[u8] = b"Hello, World!";

#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

struct App {
    io: TcpStream,
    read_buf: BytesMut,
    write_buf: BytesMut,
    codec: h1::Codec,
}

impl App {
    fn handle_request(&mut self, req: Request) {
        match req.path() {
            "/json" => {
                self.write_buf.extend_from_slice(JSON);
                self.codec.set_date_header(&mut self.write_buf);
                Message {
                    message: "Hello, World!",
                }
                .to_bytes_mut(&mut self.write_buf);
            }
            "/plaintext" => {
                self.write_buf.extend_from_slice(PLAIN);
                self.codec.set_date_header(&mut self.write_buf);
                self.write_buf.extend_from_slice(BODY);
            }
            _ => {
                self.write_buf.extend_from_slice(HTTPNFOUND);
                self.write_buf.extend_from_slice(HDR_SERVER);
            }
        }
    }
}

impl Future for App {
    type Output = Result<(), ()>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.get_mut();

        loop {
            if this.read_buf.capacity() - this.read_buf.len() < 512 {
                this.read_buf.reserve(32_768);
            }
            let read = Pin::new(&mut this.io).poll_read_buf(cx, &mut this.read_buf);
            match read {
                Poll::Pending => break,
                Poll::Ready(Ok(n)) => {
                    if n == 0 {
                        return Poll::Ready(Ok(()));
                    }
                }
                Poll::Ready(Err(_)) => return Poll::Ready(Err(())),
            }
        }

        if this.write_buf.capacity() - this.write_buf.len() <= 512 {
            this.write_buf.reserve(32_768);
        }

        loop {
            match this.codec.decode(&mut this.read_buf) {
                Ok(Some(h1::Message::Item(req))) => this.handle_request(req),
                Ok(None) => break,
                _ => return Poll::Ready(Err(())),
            }
        }

        if !this.write_buf.is_empty() {
            let len = this.write_buf.len();
            let mut written = 0;
            while written < len {
                match Pin::new(&mut this.io).poll_write(cx, &this.write_buf[written..]) {
                    Poll::Pending => {
                        break;
                    }
                    Poll::Ready(Ok(n)) => {
                        if n == 0 {
                            return Poll::Ready(Ok(()));
                        } else {
                            written += n;
                        }
                    }
                    Poll::Ready(Err(_)) => return Poll::Ready(Err(())),
                }
            }
            if written == len {
                unsafe { this.write_buf.set_len(0) }
            } else if written > 0 {
                this.write_buf.advance(written);
            }
        }
        Poll::Pending
    }
}

#[ntex::main]
async fn main() -> io::Result<()> {
    println!("Started http server: 127.0.0.1:8080");

    // start http server
    ntex::server::build()
        .backlog(1024)
        .bind("techempower", "0.0.0.0:8080", || {
            fn_service(|io: TcpStream| App {
                io,
                read_buf: BytesMut::with_capacity(32_768),
                write_buf: BytesMut::with_capacity(32_768),
                codec: h1::Codec::default(),
            })
        })?
        .start()
        .await
}
