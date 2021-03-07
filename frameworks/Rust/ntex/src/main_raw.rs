#[global_allocator]
static GLOBAL: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

use std::{cell::RefCell, future::Future, io, pin::Pin, rc::Rc, task::Context, task::Poll};

use ntex::fn_service;
use ntex::framed::{ReadTask, State, WriteTask};
use ntex::http::h1;
use ntex::rt::net::TcpStream;
use yarte::Serialize;

mod utils;

const JSON: &[u8] =
    b"HTTP/1.1 200 OK\r\nServer: N\r\nContent-Type: application/json\r\nContent-Length: 27\r\n";
const PLAIN: &[u8] =
    b"HTTP/1.1 200 OK\r\nServer: N\r\nContent-Type: text/plain\r\nContent-Length: 13\r\n";
const HTTPNFOUND: &[u8] = b"HTTP/1.1 400 OK\r\n";
const HDR_SERVER: &[u8] = b"Server: N\r\n";
const BODY: &[u8] = b"Hello, World!";

#[derive(Serialize)]
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

        let mut updated = false;
        loop {
            match this.state.decode_item(&this.codec) {
                Ok(Some((req, _))) => {
                    match req.path() {
                        "/json" => this.state.with_write_buf(|buf| {
                            buf.extend_from_slice(JSON);
                            this.codec.set_date_header(buf);
                            Message {
                                message: "Hello, World!",
                            }
                            .to_bytes_mut(buf);
                        }),
                        "/plaintext" => this.state.with_write_buf(|buf| {
                            buf.extend_from_slice(PLAIN);
                            this.codec.set_date_header(buf);
                            buf.extend_from_slice(BODY);
                        }),
                        _ => this.state.with_write_buf(|buf| {
                            buf.extend_from_slice(HTTPNFOUND);
                            buf.extend_from_slice(HDR_SERVER);
                        }),
                    }
                    updated = true;
                }
                Ok(None) => break,
                _ => {
                    this.state.close();
                    return Poll::Ready(Err(()));
                }
            }
        }
        if updated {
            this.state.dsp_restart_write_task();
        }
        if !this.state.is_read_ready() {
            this.state.dsp_read_more_data(cx.waker());
        } else {
            this.state.dsp_register_task(cx.waker());
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
                let state = State::new().disconnect_timeout(0);
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
