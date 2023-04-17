// used as reference of if/how moving from epoll to io-uring(or mixture of the two) make sense for
// network io.

#![allow(dead_code)]
#![feature(type_alias_impl_trait)]

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

mod db;
mod ser;
mod util;

use std::{
    cell::RefCell,
    convert::Infallible,
    fmt,
    future::{poll_fn, Future},
    io,
    pin::pin,
};

use futures_util::stream::Stream;
use xitca_http::{
    body::Once,
    date::{DateTime, DateTimeService},
    h1::proto::context::Context,
    http::{
        self,
        const_header_value::{TEXT, TEXT_HTML_UTF8},
        header::{CONTENT_TYPE, SERVER},
        IntoResponse, RequestExt, StatusCode,
    },
    util::service::context::{Context as Ctx, ContextBuilder},
};
use xitca_io::{
    bytes::{Buf, Bytes, BytesMut, PagedBytesMut},
    net::TcpStream,
};
use xitca_service::{fn_service, middleware::UncheckedReady, Service, ServiceExt};

use self::{
    db::Client,
    ser::{json_response, Message},
    util::{QueryParse, DB_URL, SERVER_HEADER_VALUE},
};

fn main() -> io::Result<()> {
    xitca_server::Builder::new()
        .bind("xitca-iou", "0.0.0.0:8080", || {
            Http1IOU::new(
                ContextBuilder::new(|| async {
                    db::create(DB_URL).await.map(|client| State {
                        client,
                        write_buf: RefCell::new(BytesMut::new()),
                    })
                })
                .service(fn_service(handler)),
            )
            .enclosed(UncheckedReady)
        })?
        .build()
        .wait()
}

async fn handler(ctx: Ctx<'_, Request, State>) -> Result<Response, Infallible> {
    let (req, state) = ctx.into_parts();
    let mut res = match req.uri().path() {
        "/plaintext" => {
            let mut res = req.into_response(Bytes::from_static(b"Hello, World!"));
            res.headers_mut().insert(CONTENT_TYPE, TEXT);
            res
        }
        "/json" => json_response(req, &mut state.write_buf.borrow_mut(), &Message::new()).unwrap(),
        "/db" => {
            let world = state.client.get_world().await.unwrap();
            json_response(req, &mut state.write_buf.borrow_mut(), &world).unwrap()
        }
        "/queries" => {
            let num = req.uri().query().parse_query();
            let worlds = state.client.get_worlds(num).await.unwrap();
            json_response(req, &mut state.write_buf.borrow_mut(), worlds.as_slice()).unwrap()
        }
        "/updates" => {
            let num = req.uri().query().parse_query();
            let worlds = state.client.update(num).await.unwrap();
            json_response(req, &mut state.write_buf.borrow_mut(), worlds.as_slice()).unwrap()
        }
        "/fortunes" => {
            use sailfish::TemplateOnce;
            let fortunes = state
                .client
                .tell_fortune()
                .await
                .unwrap()
                .render_once()
                .unwrap();
            let mut res = req.into_response(Bytes::from(fortunes));
            res.headers_mut().append(CONTENT_TYPE, TEXT_HTML_UTF8);
            res
        }
        _ => {
            let mut res = req.into_response(Bytes::new());
            *res.status_mut() = StatusCode::NOT_FOUND;
            res
        }
    };
    res.headers_mut().insert(SERVER, SERVER_HEADER_VALUE);
    Ok(res)
}

struct Http1IOU<S> {
    service: S,
}

impl<S> Http1IOU<S> {
    fn new(service: S) -> Self {
        Self { service }
    }
}

// builder for http service.
impl<S> Service for Http1IOU<S>
where
    S: Service,
{
    type Response = Http1IOUService<S::Response>;
    type Error = S::Error;
    type Future<'f> = impl Future<Output = Result<Self::Response, Self::Error>> + 'f where Self: 'f, (): 'f ;

    fn call<'s>(&'s self, _: ()) -> Self::Future<'s>
    where
        (): 's,
    {
        async {
            let service = self.service.call(()).await?;
            Ok(Http1IOUService {
                service,
                date: DateTimeService::new(),
            })
        }
    }
}

struct Http1IOUService<S> {
    service: S,
    date: DateTimeService,
}

// runner for http service.
impl<S> Service<TcpStream> for Http1IOUService<S>
where
    S: Service<Request, Response = Response>,
    S::Error: fmt::Debug,
{
    type Response = ();
    type Error = io::Error;
    type Future<'f> = impl Future<Output = Result<Self::Response, Self::Error>> + 'f where Self: 'f, TcpStream: 'f ;

    fn call<'s>(&'s self, stream: TcpStream) -> Self::Future<'s>
    where
        TcpStream: 's,
    {
        async {
            let mut ctx = Context::<_, 8>::new(self.date.get());
            let mut paged = PagedBytesMut::new();
            let mut write_buf = BytesMut::with_capacity(4096);

            #[cfg(feature = "io-uring")]
            {
                let std = stream.into_std()?;
                let stream = tokio_uring::net::TcpStream::from_std(std);

                let mut read_buf = vec![0; 4096];

                'io: loop {
                    let (res, buf) = stream.read(read_buf).await;
                    let n = res?;
                    if n == 0 {
                        break;
                    }
                    read_buf = buf;
                    paged.get_mut().extend_from_slice(&read_buf[..n]);

                    request_handler(&mut ctx, &self.service, &mut paged, &mut write_buf).await;

                    while !write_buf.is_empty() {
                        let (res, mut w) = stream.write(write_buf).await;
                        let n = res?;
                        if n == 0 {
                            break 'io;
                        }
                        w.advance(n);
                        write_buf = w;
                    }
                }

                stream.shutdown(std::net::Shutdown::Both)
            }

            #[cfg(not(feature = "io-uring"))]
            {
                use xitca_io::{
                    bytes::BufRead,
                    io::{AsyncIo, Interest},
                };

                let mut stream = stream;

                'io: loop {
                    let interest = if write_buf.is_empty() {
                        Interest::READABLE
                    } else {
                        Interest::READABLE | Interest::WRITABLE
                    };

                    let ready = stream.ready(interest).await?;

                    if ready.is_readable() {
                        paged.do_io(&mut stream)?;
                        request_handler(&mut ctx, &self.service, &mut paged, &mut write_buf).await;
                    }

                    if ready.is_writable() {
                        'write: loop {
                            match io::Write::write(&mut stream, &write_buf) {
                                Ok(0) => break 'io,
                                Ok(n) => {
                                    write_buf.advance(n);
                                    if write_buf.is_empty() {
                                        break 'write;
                                    }
                                }
                                Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => break 'write,
                                Err(e) => return Err(e),
                            }
                        }
                    }
                }

                Ok(())
            }
        }
    }
}

async fn request_handler<D, S, const L: usize>(
    ctx: &mut Context<'_, D, L>,
    service: &S,
    paged: &mut PagedBytesMut<4096>,
    write_buf: &mut BytesMut,
) where
    D: DateTime,
    S: Service<Request, Response = Response>,
    S::Error: fmt::Debug,
{
    while let Some((req, _)) = ctx.decode_head::<{ usize::MAX }>(paged).unwrap() {
        let (parts, body) = service.call(req).await.unwrap().into_parts();
        let mut encoder = ctx.encode_head(parts, &body, write_buf).unwrap();
        let mut body = pin!(body);
        while let Some(chunk) = poll_fn(|cx| body.as_mut().poll_next(cx)).await {
            let chunk = chunk.unwrap();
            encoder.encode(chunk, write_buf);
        }
        encoder.encode_eof(write_buf);
    }
}

type Request = http::Request<RequestExt<()>>;
type Response = http::Response<Once<Bytes>>;

struct State {
    client: Client,
    write_buf: RefCell<BytesMut>,
}
