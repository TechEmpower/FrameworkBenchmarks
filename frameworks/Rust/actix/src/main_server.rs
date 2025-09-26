#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

use std::future::Future;
use std::io;
use std::pin::Pin;
use std::task::{Context, Poll};

use actix_codec::{AsyncWrite, Decoder};
use actix_http::{h1, Request};
use actix_rt::net::TcpStream;
use actix_server::Server;
use actix_service::fn_service;
use bytes::{Buf, BufMut, BytesMut};
use simd_json_derive::Serialize;
use tokio::io::{AsyncBufRead, BufReader};

mod models;
mod utils;

use crate::utils::Writer;

const HEAD_JSON: &[u8] =
    b"HTTP/1.1 200 OK\r\nServer: A\r\nContent-Type: application/json\r\nContent-Length: 27\r\n";
const HEAD_PLAIN: &[u8] =
    b"HTTP/1.1 200 OK\r\nServer: A\r\nContent-Type: text/plain\r\nContent-Length: 13\r\n";
const HEAD_NOT_FOUND: &[u8] = b"HTTP/1.1 204 OK\r\nServer: A\r\n";
const BODY_PLAIN: &[u8] = b"Hello, World!";
const HDR_END: &[u8] = b"\r\n";

#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

struct App {
    io: BufReader<TcpStream>,
    read_buf: BytesMut,
    write_buf: BytesMut,
    codec: h1::Codec,
}

impl App {
    fn handle_request(&mut self, req: Request) {
        match req.path() {
            "/json" => {
                let message = Message {
                    message: "Hello, World!",
                };
                self.write_buf.put_slice(HEAD_JSON);
                self.codec
                    .config()
                    .write_date_header(&mut self.write_buf, false);
                self.write_buf.put_slice(HDR_END);
                message
                    .json_write(&mut Writer(&mut self.write_buf))
                    .unwrap();
            }

            "/plaintext" => {
                self.write_buf.put_slice(HEAD_PLAIN);
                self.codec
                    .config()
                    .write_date_header(&mut self.write_buf, false);
                self.write_buf.put_slice(HDR_END);
                self.write_buf.put_slice(BODY_PLAIN);
            }

            _ => {
                self.write_buf.put_slice(HEAD_NOT_FOUND);
                self.write_buf.put_slice(HDR_END);
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

            let n = match Pin::new(&mut this.io).poll_fill_buf(cx) {
                Poll::Pending => break,
                Poll::Ready(Ok(filled)) => {
                    if filled.is_empty() {
                        return Poll::Ready(Ok(()));
                    }

                    this.read_buf.extend_from_slice(filled);
                    filled.len()
                }
                Poll::Ready(Err(_)) => return Poll::Ready(Err(())),
            };

            Pin::new(&mut this.io).consume(n);
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

#[actix_web::main]
async fn main() -> io::Result<()> {
    println!("Started HTTP server: 127.0.0.1:8080");

    // start http server
    Server::build()
        .backlog(1024)
        .bind("tfb-actix-server", "0.0.0.0:8080", || {
            fn_service(|io: TcpStream| App {
                io: BufReader::new(io),
                read_buf: BytesMut::with_capacity(32_768),
                write_buf: BytesMut::with_capacity(32_768),
                codec: h1::Codec::default(),
            })
        })?
        .run()
        .await
}
