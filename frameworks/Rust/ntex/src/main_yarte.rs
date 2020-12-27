use std::borrow::Cow;
use std::future::Future;
use std::io;
use std::pin::Pin;
use std::task::{Context, Poll};

use bytes::BytesMut;
use futures::{FutureExt, StreamExt};
use ntex::codec::{AsyncRead, AsyncWrite, Decoder};
use ntex::http::h1;
use ntex::rt::net::TcpStream;
use ntex::{fn_factory, fn_service};
use smallvec::SmallVec;
use std::convert::TryFrom;
use tokio_postgres::{connect, Client, NoTls, Statement};
use yarte::ywrite_html;

use crate::utils::write_u16_reverse;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

mod utils;

// u16 Content-length
// https://tools.ietf.org/html/rfc7230#section-3.2
const HEAD: &[u8] = b"HTTP/1.1 200 OK\r\nServer: N\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length:     0\r\n";
const HTTPNFOUND: &[u8] = b"HTTP/1.1 400 OK\r\n";
const HTTPSERR: &[u8] = b"HTTP/1.1 500 OK\r\n";
const HDR_SERVER: &[u8] = b"Server: N\r\n";

#[derive(serde::Serialize, Debug)]
pub struct Fortune {
    pub id: i32,
    pub message: Cow<'static, str>,
}

struct App {
    io: TcpStream,
    read_buf: BytesMut,
    write_buf: BytesMut,
    write_pos: usize,
    codec: h1::Codec,
    db: PgConnection,
    // TODO: should be abstract with FnOnce |Output, &mut Writer|
    call: Option<
        Pin<Box<dyn Future<Output = Result<SmallVec<[Fortune; 32]>, io::Error>>>>,
    >,
}

impl Future for App {
    type Output = Result<(), ()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().get_mut();

        if !this.write_buf.is_empty() {
            let len = this.write_buf.len();
            let mut written = this.write_pos;
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
                this.write_pos = 0;
                unsafe { this.write_buf.set_len(0) }
            } else if written > 0 {
                this.write_pos = written;
                return Poll::Pending;
            }
        }

        if let Some(call) = this.call.as_mut() {
            match call.as_mut().poll(cx) {
                Poll::Pending => {
                    return Poll::Pending;
                }
                Poll::Ready(res) => {
                    this.call = None;
                    match res {
                        Ok(fortunes) => {
                            this.write_buf.extend_from_slice(HEAD);
                            // 0 position in buffer
                            let n = this.write_buf.len() - 3;
                            this.codec.set_date_header(&mut this.write_buf);
                            // Body init
                            let init = this.write_buf.len();
                            ywrite_html!(this.write_buf, "{{> fortune }}");

                            // Write Content-Length
                            let size = u16::try_from(this.write_buf.len() - init)
                                .expect("Overflow u16");
                            // SAFETY: previous reverse SIZE as OWS in this pointer
                            unsafe {
                                let zero_ptr = this.write_buf.as_mut_ptr().add(n);
                                write_u16_reverse(size, zero_ptr);
                            }
                        }
                        Err(_) => {
                            this.write_buf.extend_from_slice(HTTPSERR);
                            this.write_buf.extend_from_slice(HDR_SERVER);
                        }
                    }
                    return self.poll(cx);
                }
            }
        }

        if this.read_buf.capacity() - this.read_buf.len() < 4096 {
            this.read_buf.reserve(32_768);
        }

        loop {
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

        loop {
            match this.codec.decode(&mut this.read_buf) {
                Ok(Some(h1::Message::Item(req))) => match req.path() {
                    "/fortunes" => {
                        this.call = Some(Box::pin(this.db.tell_fortune()));
                    }
                    _ => {
                        this.write_buf.extend_from_slice(HTTPNFOUND);
                        this.write_buf.extend_from_slice(HDR_SERVER);
                    }
                },
                Ok(None) => break,
                _ => return Poll::Ready(Err(())),
            }
        }

        if !this.write_buf.is_empty() {
            self.poll(cx)
        } else {
            Poll::Pending
        }
    }
}

/// Postgres interface
#[derive(Clone)]
pub struct PgConnection {
    cl: Client,
    fortune: Statement,
}

impl PgConnection {
    async fn connect(db_url: &str) -> PgConnection {
        let (cl, conn) = connect(db_url, NoTls)
            .await
            .expect("can not connect to postgresql");
        ntex::rt::spawn(conn.map(|_| ()));

        let fortune = cl.prepare("SELECT * FROM fortune").await.unwrap();

        PgConnection { cl, fortune }
    }
    fn tell_fortune(
        &self,
    ) -> impl Future<Output = Result<SmallVec<[Fortune; 32]>, io::Error>> {
        let fut = self.cl.query_raw(&self.fortune, &[]);

        async move {
            let mut stream = fut
                .await
                .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("{:?}", e)))?;

            let mut fortunes: SmallVec<[_; 32]> = smallvec::smallvec![Fortune {
                id: 0,
                message: Cow::Borrowed("Additional fortune added at request time."),
            }];

            while let Some(row) = stream.next().await {
                let row = row.map_err(|e| {
                    io::Error::new(io::ErrorKind::Other, format!("{:?}", e))
                })?;
                fortunes.push(Fortune {
                    id: row.get(0),
                    message: Cow::Owned(row.get(1)),
                });
            }

            fortunes.sort_by(|it, next| it.message.cmp(&next.message));

            Ok(fortunes)
        }
    }
}

#[ntex::main]
async fn main() -> io::Result<()> {
    const DB_URL: &str =
        "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";
    println!("Started http server: 127.0.0.1:8080");

    // start http server
    ntex::server::build()
        .backlog(1024)
        .bind("techempower", "0.0.0.0:8080", || {
            fn_factory(|| async {
                let db = PgConnection::connect(DB_URL).await;
                Ok::<_, io::Error>(fn_service(move |io: TcpStream| App {
                    io,
                    db: db.clone(),
                    read_buf: BytesMut::with_capacity(32_768),
                    write_buf: BytesMut::with_capacity(32_768),
                    write_pos: 0,
                    codec: h1::Codec::default(),
                    call: None,
                }))
            })
        })?
        .start()
        .await
}
