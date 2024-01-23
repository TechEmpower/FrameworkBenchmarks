// used as reference of if/how moving from epoll to io-uring(or mixture of the two) make sense for
// network io.

mod db;
mod ser;
mod util;

use std::{convert::Infallible, fmt, future::poll_fn, io, pin::pin};

use futures_core::stream::Stream;
use xitca_http::{
    body::Once,
    date::DateTimeService,
    h1::proto::context::Context,
    http::{
        self,
        const_header_value::{TEXT, TEXT_HTML_UTF8},
        header::{CONTENT_TYPE, SERVER},
        IntoResponse, RequestExt, StatusCode,
    },
};
use xitca_io::{
    bytes::{Bytes, BytesMut},
    io_uring::IoBuf,
    net::{io_uring::TcpStream as IOUTcpStream, TcpStream},
};
use xitca_service::{fn_build, fn_service, middleware::UncheckedReady, Service, ServiceExt};

use self::{
    ser::{json_response, Message},
    util::{context_mw, Ctx, QueryParse, SERVER_HEADER_VALUE},
};

type Request = http::Request<RequestExt<()>>;
type Response = http::Response<Once<Bytes>>;

fn main() -> io::Result<()> {
    let service = fn_service(handler)
        .enclosed(context_mw())
        .enclosed(fn_build(|res: Result<_, _>| async {
            res.map(|service| Http1IOU {
                service,
                date: DateTimeService::new(),
            })
        }))
        .enclosed(UncheckedReady);
    xitca_server::Builder::new()
        .bind("xitca-iou", "0.0.0.0:8080", service)?
        .build()
        .wait()
}

async fn handler(ctx: Ctx<'_, Request>) -> Result<Response, Infallible> {
    let (req, state) = ctx.into_parts();
    let mut res = match req.uri().path() {
        "/plaintext" => {
            const HELLO: Bytes = Bytes::from_static(b"Hello, World!");
            let mut res = req.into_response(HELLO);
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
    date: DateTimeService,
}

// runner for http service.
impl<S> Service<TcpStream> for Http1IOU<S>
where
    S: Service<Request, Response = Response>,
    S::Error: fmt::Debug,
{
    type Response = ();
    type Error = io::Error;

    async fn call(&self, stream: TcpStream) -> Result<Self::Response, Self::Error> {
        let std = stream.into_std()?;
        let stream = IOUTcpStream::from_std(std);

        let mut ctx = Context::<_, 8>::new(self.date.get());
        let mut read_buf = BytesMut::new();
        let mut write_buf = BytesMut::with_capacity(4096);

        loop {
            let len = read_buf.len();
            let rem = read_buf.capacity() - len;
            if rem < 4096 {
                read_buf.reserve(4096 - rem);
            }

            let (res, buf) = stream.read(read_buf.slice(len..)).await;
            read_buf = buf.into_inner();
            if res? == 0 {
                break;
            }

            while let Some((req, _)) = ctx.decode_head::<{ usize::MAX }>(&mut read_buf).unwrap() {
                let (parts, body) = self.service.call(req).await.unwrap().into_parts();
                let mut encoder = ctx.encode_head(parts, &body, &mut write_buf).unwrap();
                let mut body = pin!(body);
                let chunk = poll_fn(|cx| body.as_mut().poll_next(cx))
                    .await
                    .unwrap()
                    .unwrap();
                encoder.encode(chunk, &mut write_buf);
                encoder.encode_eof(&mut write_buf);
            }

            let (res, b) = stream.write_all(write_buf).await;
            write_buf = b;
            write_buf.clear();
            res?;
        }

        stream.shutdown(std::net::Shutdown::Both)
    }
}
