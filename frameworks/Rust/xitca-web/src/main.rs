#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

mod db;
mod ser;

use std::{cell::RefCell, cmp, convert::Infallible, error::Error, io};

use bytes::{Bytes, BytesMut};
use serde::Serialize;
use xitca_http::http::{
    header::{HeaderValue, CONTENT_TYPE, SERVER},
    Method, StatusCode,
};
use xitca_web::{
    dev::fn_service,
    request::WebRequest,
    response::{WebResponse, WebResponseBuilder},
    App, HttpServer,
};

use self::db::Client;
use self::ser::{Message, Writer};

#[tokio::main(flavor = "current_thread")]
async fn main() -> io::Result<()> {
    let config = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

    HttpServer::new(move || {
        App::with_async_state(move || AppState::init(config)).service(fn_service(handle))
    })
    .force_flat_buf()
    .max_request_headers::<8>()
    .bind("0.0.0.0:8080")?
    .run()
    .await
}

type HandleResult = Result<WebResponse, Infallible>;

async fn handle(mut req: WebRequest<'_, AppState>) -> HandleResult {
    let inner = req.request_mut();

    match (inner.method(), inner.uri().path()) {
        (&Method::GET, "/plaintext") => plain_text(req),
        (&Method::GET, "/json") => json(req),
        (&Method::GET, "/db") => db(req).await,
        (&Method::GET, "/fortunes") => fortunes(req).await,
        (&Method::GET, "/queries") => queries(req).await,
        (&Method::GET, "/updates") => updates(req).await,
        _ => not_found(),
    }
}

fn plain_text(req: WebRequest<'_, AppState>) -> HandleResult {
    let mut res = req.into_response(Bytes::from_static(b"Hello, World!"));

    res.headers_mut()
        .append(SERVER, HeaderValue::from_static("TFB"));
    res.headers_mut()
        .append(CONTENT_TYPE, HeaderValue::from_static("text/plain"));

    Ok(res)
}

#[inline(always)]
fn json(req: WebRequest<'_, AppState>) -> HandleResult {
    json_response(req, &Message::new())
}

async fn db(req: WebRequest<'_, AppState>) -> HandleResult {
    match req.state().client().get_world().await {
        Ok(ref world) => json_response(req, world),
        Err(_) => internal(),
    }
}

async fn fortunes(req: WebRequest<'_, AppState>) -> HandleResult {
    match _fortunes(req.state().client()).await {
        Ok(body) => {
            let mut res = req.into_response(body);

            res.headers_mut()
                .append(SERVER, HeaderValue::from_static("TFB"));
            res.headers_mut().append(
                CONTENT_TYPE,
                HeaderValue::from_static("text/html; charset=utf-8"),
            );

            Ok(res)
        }
        Err(_) => internal(),
    }
}

async fn queries(mut req: WebRequest<'_, AppState>) -> HandleResult {
    let num = req.request_mut().uri().query().parse_query();

    match req.state().client().get_worlds(num).await {
        Ok(worlds) => json_response(req, worlds.as_slice()),
        Err(_) => internal(),
    }
}

async fn updates(mut req: WebRequest<'_, AppState>) -> HandleResult {
    let num = req.request_mut().uri().query().parse_query();

    match req.state().client().update(num).await {
        Ok(worlds) => json_response(req, worlds.as_slice()),
        Err(_) => internal(),
    }
}

trait QueryParse {
    fn parse_query(self) -> u16;
}

impl QueryParse for Option<&str> {
    fn parse_query(self) -> u16 {
        let num = self
            .and_then(|this| {
                use atoi::FromRadix10;
                this.find('q')
                    .map(|pos| u16::from_radix_10(this.split_at(pos + 2).1.as_ref()).0)
            })
            .unwrap_or(1);

        cmp::min(500, cmp::max(1, num))
    }
}

#[inline]
async fn _fortunes(client: &Client) -> Result<Bytes, Box<dyn Error>> {
    use sailfish::TemplateOnce;
    let fortunes = client.tell_fortune().await?.render_once()?;
    Ok(fortunes.into())
}

#[inline]
fn json_response<S>(req: WebRequest<'_, AppState>, value: &S) -> HandleResult
where
    S: ?Sized + Serialize,
{
    let mut writer = req.state().writer();
    simd_json::to_writer(&mut writer, value).unwrap();
    let body = writer.take();

    let mut res = req.into_response(body);
    res.headers_mut()
        .append(SERVER, HeaderValue::from_static("TFB"));
    res.headers_mut()
        .append(CONTENT_TYPE, HeaderValue::from_static("application/json"));

    Ok(res)
}

struct AppState {
    // postgres client
    client: Client,
    // a re-usable buffer for write response data.
    write_buf: RefCell<BytesMut>,
}

impl AppState {
    async fn init(config: &str) -> Self {
        let client = db::create(config).await;
        let write_buf = RefCell::new(BytesMut::new());

        Self { client, write_buf }
    }

    #[inline]
    fn writer(&self) -> Writer<'_> {
        Writer(self.write_buf.borrow_mut())
    }

    #[inline]
    fn client(&self) -> &Client {
        &self.client
    }
}

macro_rules! error {
    ($error: ident, $code: path) => {
        fn $error() -> HandleResult {
            Ok(WebResponseBuilder::new()
                .status($code)
                .header(SERVER, HeaderValue::from_static("TFB"))
                .body(Bytes::new().into())
                .unwrap())
        }
    };
}

error!(not_found, StatusCode::NOT_FOUND);
error!(internal, StatusCode::INTERNAL_SERVER_ERROR);
