#![feature(generic_associated_types, type_alias_impl_trait)]

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

mod db;
mod ser;
mod util;

use std::{
    convert::Infallible,
    error::Error,
    future::ready,
    io,
    sync::{Arc, Mutex},
};

use serde::Serialize;
use xitca_http::{
    body::ResponseBody,
    bytes::Bytes,
    config::HttpServiceConfig,
    h1::RequestBody,
    http::{
        self,
        header::{CONTENT_TYPE, SERVER},
        IntoResponse, Method,
    },
    util::service::Route,
    HttpServiceBuilder,
};
use xitca_server::Builder;

use self::db::Client;
use self::ser::Message;
use self::util::{
    internal, not_found, AppState, QueryParse, JSON_HEADER_VALUE, SERVER_HEADER_VALUE,
    TEXT_HEADER_VALUE,
};

type Request = xitca_http::Request<RequestBody>;

type Response = http::Response<ResponseBody>;

#[tokio::main(flavor = "current_thread")]
async fn main() -> io::Result<()> {
    let cores = core_affinity::get_core_ids().unwrap_or_else(Vec::new);
    let cores = Arc::new(Mutex::new(cores));

    let factory = || {
        let http = Http {
            config: "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world",
        };

        let config = HttpServiceConfig::new()
            .disable_vectored_write()
            .max_request_headers::<8>();

        let route = Route::new(http).methods([Method::GET]);

        HttpServiceBuilder::h1(route).config(config)
    };

    Builder::new()
        .on_worker_start(move || {
            if let Some(core) = cores.lock().unwrap().pop() {
                core_affinity::set_for_current(core);
            }
            ready(())
        })
        .bind("xitca-web", "0.0.0.0:8080", factory)?
        .build()
        .await
}

#[derive(Clone)]
struct Http {
    config: &'static str,
}

struct HttpService {
    state: AppState<Client>,
}

#[xitca_http_codegen::service_impl]
impl HttpService {
    async fn new_service(http: &Http, _: ()) -> Result<Self, ()> {
        let client = db::create(http.config).await;

        Ok(HttpService {
            state: AppState::new(client),
        })
    }

    async fn ready(&self) -> Result<(), Infallible> {
        Ok(())
    }

    async fn call(&self, req: Request) -> Result<Response, Infallible> {
        match req.uri().path() {
            "/plaintext" => self.plain_text(req),
            "/json" => self.json(req),
            "/db" => self.db(req).await,
            "/fortunes" => self.fortunes(req).await,
            "/queries" => self.queries(req).await,
            "/updates" => self.updates(req).await,
            _ => not_found(),
        }
    }
}

impl HttpService {
    fn plain_text(&self, req: Request) -> Result<Response, Infallible> {
        let mut res = req.into_response("Hello, World!");

        res.headers_mut().append(SERVER, SERVER_HEADER_VALUE);
        res.headers_mut().append(CONTENT_TYPE, TEXT_HEADER_VALUE);

        Ok(res)
    }

    #[inline]
    fn json(&self, req: Request) -> Result<Response, Infallible> {
        self._json(req, &Message::new())
    }

    async fn db(&self, req: Request) -> Result<Response, Infallible> {
        match self.state.client().get_world().await {
            Ok(ref world) => self._json(req, world),
            Err(_) => internal(),
        }
    }

    async fn fortunes(&self, req: Request) -> Result<Response, Infallible> {
        match self._fortunes().await {
            Ok(body) => {
                let mut res = req.into_response(body);

                res.headers_mut().append(SERVER, util::SERVER_HEADER_VALUE);
                res.headers_mut()
                    .append(CONTENT_TYPE, util::HTML_HEADER_VALUE);

                Ok(res)
            }
            Err(_) => internal(),
        }
    }

    async fn queries(&self, req: Request) -> Result<Response, Infallible> {
        let num = req.uri().query().parse_query();
        match self.state.client().get_worlds(num).await {
            Ok(worlds) => self._json(req, worlds.as_slice()),
            Err(_) => internal(),
        }
    }

    async fn updates(&self, req: Request) -> Result<Response, Infallible> {
        let num = req.uri().query().parse_query();
        match self.state.client().update(num).await {
            Ok(worlds) => self._json(req, worlds.as_slice()),
            Err(_) => internal(),
        }
    }

    async fn _fortunes(&self) -> Result<Bytes, Box<dyn Error>> {
        use sailfish::TemplateOnce;
        let fortunes = self.state.client().tell_fortune().await?.render_once()?;
        Ok(fortunes.into())
    }

    fn _json<S>(&self, req: Request, value: &S) -> Result<Response, Infallible>
    where
        S: ?Sized + Serialize,
    {
        let mut writer = self.state.writer();
        simd_json::to_writer(&mut writer, value).unwrap();
        let body = writer.take();

        let mut res = req.into_response(body);
        res.headers_mut().append(SERVER, SERVER_HEADER_VALUE);
        res.headers_mut().append(CONTENT_TYPE, JSON_HEADER_VALUE);

        Ok(res)
    }
}
