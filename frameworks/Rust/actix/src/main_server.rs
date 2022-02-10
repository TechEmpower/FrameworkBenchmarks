#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

use std::{
    future::Future,
    io,
    pin::Pin,
    task::{Context, Poll},
};

use actix_codec::{AsyncWrite, Decoder};
use actix_http::{h1, Request};
use actix_server::Server;
use actix_service::fn_service;
use bytes::{Buf, BufMut, BytesMut};
use simd_json_derive::Serialize;
use tokio::net::TcpStream;

mod models;
mod utils;

use crate::utils::Writer;

const JSON_HEAD: &[u8] =
    b"HTTP/1.1 200 OK\r\nServer: A\r\nContent-Type: application/json\r\nContent-Length: 27\r\n";
const PLAIN_HEAD: &[u8] =
    b"HTTP/1.1 200 OK\r\nServer: A\r\nContent-Type: text/plain\r\nContent-Length: 13\r\n";
const NOT_FOUND_HEAD: &[u8] = b"HTTP/1.1 204 OK\r\nServer: A\r\n";
const END_HEADERS: &[u8] = b"\r\n";
const PLAIN_BODY: &[u8] = b"Hello, World!";

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
                let message = Message {
                    message: "Hello, World!",
                };
                self.write_buf.put_slice(JSON_HEAD);
                self.codec.config().write_date_header(&mut self.write_buf, false);
                self.write_buf.put_slice(END_HEADERS);

                message
                    .json_write(&mut Writer(&mut self.write_buf))
                    .unwrap();
            }

            "/plaintext" => {
                self.write_buf.put_slice(PLAIN_HEAD);
                self.codec.config().write_date_header(&mut self.write_buf, false);
                self.write_buf.put_slice(END_HEADERS);

                self.write_buf.put_slice(PLAIN_BODY);
            }
            
            _ => {
                self.write_buf.put_slice(NOT_FOUND_HEAD);
                self.write_buf.put_slice(END_HEADERS);
            }
        }
    }
}

impl Future for App {
    type Output = Result<(), ()>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.get_mut();

        loop {
            if this.io.poll_read_ready(cx).is_pending() {
                break;
            }
            
            if this.read_buf.capacity() - this.read_buf.len() < 512 {
                this.read_buf.reserve(32_768);
            }

            let io = Pin::new(&mut this.io);

            match io.try_read_buf(&mut this.read_buf) {
                Ok(0) => return Poll::Ready(Ok(())),

                Ok(_) => {}
                Err(ref err) if err.kind() == io::ErrorKind::WouldBlock => break,
                
                Err(_) => return Poll::Ready(Err(())),
            }
        }

        if this.write_buf.capacity() - this.write_buf.len() <= 512 {
            this.write_buf.reserve(32_768);
        }

        loop {
            match this.codec.decode(&mut Pin::new(&mut this.read_buf)) {
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
                    Poll::Pending => break,
                    Poll::Ready(Ok(0)) => return Poll::Ready(Ok(())),
                    Poll::Ready(Ok(n)) => {
                        written += n;
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

        match Pin::new(&mut this.io).poll_flush(cx) {
            Poll::Pending => {}
            Poll::Ready(Ok(())) => {}
            Poll::Ready(Err(_)) => return Poll::Ready(Err(())),
        }

        Poll::Pending
    }
}

#[actix_web::main]
async fn main() -> io::Result<()> {
    eprintln!("Started http server: 127.0.0.1:8080");

    // start http server
    Server::build()
        .backlog(1024)
        .bind("tfb-actix-server", "0.0.0.0:8080", || {
            fn_service(|io: TcpStream| App {
                io,
                read_buf: BytesMut::with_capacity(32_768),
                write_buf: BytesMut::with_capacity(32_768),
                codec: h1::Codec::default(),
            })
        })?
        .run()
        .await
}
