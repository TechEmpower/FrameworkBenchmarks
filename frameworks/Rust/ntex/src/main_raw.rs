#[global_allocator]
static GLOBAL: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

use std::{cell::RefCell, future::Future, io, pin::Pin, rc::Rc, task::Context, task::Poll};

use ntex::fn_service;
use ntex::framed::{ReadTask, State, WriteTask};
use ntex::http::h1;
use ntex::rt::net::TcpStream;

mod utils;

const JSON: &[u8] =
    b"HTTP/1.1 200 OK\r\nServer: N\r\nContent-Type: application/json\r\nContent-Length: 27\r\n";
const PLAIN: &[u8] =
    b"HTTP/1.1 200 OK\r\nServer: N\r\nContent-Type: text/plain\r\nContent-Length: 13\r\n";
const HTTPNFOUND: &[u8] = b"HTTP/1.1 400 OK\r\n";
const HDR_SERVER: &[u8] = b"Server: N\r\n";
const BODY: &[u8] = b"Hello, World!";

#[derive(serde::Serialize)]
pub struct Message {
    pub message: &'static str,
}

struct App {
    state: State,
    codec: h1::Codec,
}

impl Future for App {
    type Output = Result<(), ()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().get_mut();
        if !this.state.is_open() {
            this.state.close();
            return Poll::Ready(Ok(()));
        }

        let read = this.state.read();
        let write = this.state.write();
        loop {
            match read.decode(&this.codec) {
                Ok(Some((req, _))) => {
                    write.with_buf(|buf| {
                        // make sure we've got room
                        let remaining = buf.capacity() - buf.len();
                        if remaining < 1024 {
                            buf.reserve(65535 - remaining);
                        }

                        match req.path() {
                            "/json" => {
                                buf.extend_from_slice(JSON);
                                this.codec.set_date_header(buf);
                                let _ = simd_json::to_writer(
                                    crate::utils::Writer(buf),
                                    &Message {
                                        message: "Hello, World!",
                                    },
                                );
                            }
                            "/plaintext" => {
                                buf.extend_from_slice(PLAIN);
                                this.codec.set_date_header(buf);
                                buf.extend_from_slice(BODY);
                            }
                            _ => {
                                buf.extend_from_slice(HTTPNFOUND);
                                buf.extend_from_slice(HDR_SERVER);
                            }
                        }
                    });
                }
                Ok(None) => break,
                _ => {
                    this.state.close();
                    return Poll::Ready(Err(()));
                }
            }
        }
        if read.is_ready() {
            this.state.register_dispatcher(cx.waker());
        } else {
            read.wake(cx.waker())
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
            fn_service(|io: TcpStream| {
                let state = State::with_params(65535, 65535, 1024, 0);
                let io = Rc::new(RefCell::new(io));
                ntex::rt::spawn(ReadTask::new(io.clone(), state.clone()));
                ntex::rt::spawn(WriteTask::new(io, state.clone()));

                App {
                    state,
                    codec: h1::Codec::default(),
                }
            })
        })?
        .start()
        .await
}
