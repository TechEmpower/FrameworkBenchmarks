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
    convert::Infallible,
    fmt,
    future::{poll_fn, Future},
    io,
};

use futures_util::stream::Stream;
use tracing::{span, Level};
use xitca_http::http::const_header_value::TEXT_HTML_UTF8;
use xitca_http::{
    body::{BodySize, Once},
    date::DateTimeService,
    h1::proto::context::Context,
    http::{
        const_header_value::TEXT,
        header::{CONTENT_TYPE, SERVER},
        IntoResponse, Response, StatusCode,
    },
    util::{
        middleware::Logger,
        service::context::{Context as Ctx, ContextBuilder},
    },
    Request,
};
use xitca_io::{
    bytes::{Buf, Bytes, BytesMut},
    net::TcpStream,
};
use xitca_service::{fn_service, ready::ReadyService, Service, ServiceExt};
use xitca_unsafe_collection::pin;

use self::{
    db::Client,
    util::{DB_URL, SERVER_HEADER_VALUE},
};

fn main() -> io::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter("[xitca-iou]=trace")
        .init();
    xitca_server::Builder::new()
        .bind("xitca-iou", "0.0.0.0:8080", || {
            Http1IOU::new(ContextBuilder::new(|| db::create(DB_URL)).service(fn_service(handler)))
                .enclosed(Logger::with_span(span!(Level::ERROR, "xitca-iou")))
        })?
        .build()
        .wait()
}

async fn handler<B>(ctx: Ctx<'_, Request<B>, Client>) -> Result<Response<Once<Bytes>>, Infallible> {
    let (req, cli) = ctx.into_parts();
    let mut res = match req.uri().path() {
        "/plaintext" => {
            let mut res = req.into_response(Bytes::from_static(b"Hello, World!"));
            res.headers_mut().insert(CONTENT_TYPE, TEXT);
            res
        }
        "/fortunes" => {
            use sailfish::TemplateOnce;
            let fortunes = cli.tell_fortune().await.unwrap().render_once().unwrap();
            let mut res = req.into_response(Bytes::from(fortunes));
            res.headers_mut().append(CONTENT_TYPE, TEXT_HTML_UTF8);
            res
        }
        _ => {
            let mut res = req.into_response(Once::default());
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

// delegate to inner service's ready state
impl<S> ReadyService for Http1IOUService<S>
where
    S: ReadyService,
{
    type Ready = S::Ready;
    type ReadyFuture<'f> = S::ReadyFuture<'f> where Self: 'f ;

    fn ready(&self) -> Self::ReadyFuture<'_> {
        self.service.ready()
    }
}

// runner for http service.
impl<S> Service<TcpStream> for Http1IOUService<S>
where
    S: Service<Request<()>, Response = Response<Once<Bytes>>>,
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
            let std = stream.into_std()?;
            let stream = tokio_uring::net::TcpStream::from_std(std);

            let mut read_buf = BytesMut::with_capacity(4096);
            let mut write_buf = BytesMut::with_capacity(4096);

            let mut ctx = Context::<_, 8>::new(self.date.get());

            loop {
                let (res, buf) = stream.read(read_buf).await;
                let n = res?;

                if n == 0 {
                    break;
                }

                read_buf = buf;

                while let Some((req, _)) = ctx.decode_head::<65535>(&mut read_buf).unwrap() {
                    let (parts, body) = self.service.call(req).await.unwrap().into_parts();
                    let size = BodySize::from_stream(&body);
                    let mut encoder = ctx.encode_head(parts, size, &mut write_buf).unwrap();
                    pin!(body);
                    while let Some(chunk) = poll_fn(|cx| body.as_mut().poll_next(cx)).await {
                        let chunk = chunk.unwrap();
                        encoder.encode(chunk, &mut write_buf);
                    }
                    encoder.encode_eof(&mut write_buf);
                }

                if !write_buf.is_empty() {
                    let (res, mut w) = stream.write(write_buf).await;
                    let n = res?;
                    if n == 0 {
                        break;
                    }

                    w.advance(n);
                    write_buf = w;
                }

                read_buf.reserve(4096 - read_buf.capacity());
            }

            Ok(())
        }
    }
}
