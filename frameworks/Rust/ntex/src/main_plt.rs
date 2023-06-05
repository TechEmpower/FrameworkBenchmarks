#[global_allocator]
static GLOBAL: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;
use std::{future::Future, io, pin::Pin, sync::Arc, sync::Mutex, task::Context, task::Poll};

use ntex::{fn_service, http::h1, io::Io, io::RecvError, util::ready, util::PoolId};
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
    io: Io,
    codec: h1::Codec,
}

impl Future for App {
    type Output = Result<(), ()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().get_mut();
        loop {
            match ready!(this.io.poll_recv(&this.codec, cx)) {
                Ok((req, _)) => {
                    let _ = this.io.with_write_buf(|buf| {
                        buf.with_bytes_mut(|buf| {
                            utils::reserve(buf);
                            match req.path() {
                                "/json" => {
                                    buf.extend_from_slice(JSON);
                                    this.codec.set_date_header(buf);

                                    Message {
                                        message: "Hello, World!",
                                    }
                                    .to_bytes_mut(buf);
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
                        })
                    });
                }
                Err(RecvError::WriteBackpressure) => {
                    let _ = ready!(this.io.poll_flush(cx, false));
                }
                Err(_) => {
                    return Poll::Ready(Ok(()));
                }
            }
        }
    }
}

#[ntex::main]
async fn main() -> io::Result<()> {
    println!("Started http server: 127.0.0.1:8080");

    let cores = core_affinity::get_core_ids().unwrap();
    let total_cores = cores.len();
    let cores = Arc::new(Mutex::new(cores));

    // start http server
    ntex::server::build()
        .backlog(1024)
        .configure(move |cfg| {
            let cores = cores.clone();
            cfg.on_worker_start(move |_| {
                if let Some(core) = cores.lock().unwrap().pop() {
                    // Pin this worker to a single CPU core.
                    core_affinity::set_for_current(core);
                }
                std::future::ready(Ok::<_, &'static str>(()))
            })
        })?
        .bind("techempower", "0.0.0.0:8080", |cfg| {
            cfg.memory_pool(PoolId::P1);
            PoolId::P1.set_read_params(65535, 2048);
            PoolId::P1.set_write_params(65535, 2048);

            fn_service(|io| App {
                io,
                codec: h1::Codec::default(),
            })
        })?
        .workers(total_cores)
        .run()
        .await
}
