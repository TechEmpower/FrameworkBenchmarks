// this is a parity xitca-web implementation of "pure" io_uring runtime comparing performance with
// tokio_uring's epoll + io_uring runtime.

#![feature(async_iterator)]

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

mod db;
mod db_pool;
mod ser;
mod util;

use std::{cell::RefCell, io, net::SocketAddr, rc::Rc, time::Duration};

use compio::net::TcpOpts;
use xitca_http::{
    body::Once,
    bytes::Bytes,
    date::{DateTime, DateTimeState},
    h1::{RequestBody, dispatcher_compio::Dispatcher},
    http::{
        self, HeaderValue, IntoResponse as _, RequestExt, StatusCode,
        const_header_value::{JSON, TEXT_HTML_UTF8, TEXT_UTF8},
        header::{CONTENT_TYPE, SERVER},
    },
    util::{
        middleware::context::{Context, ContextBuilder},
        service::{
            route::get,
            router::{Router, RouterError},
        },
    },
};
use xitca_service::{Service, ServiceExt, fn_service};

use self::{
    ser::{HELLO, Message},
    util::{HandleResult, QueryParse},
};

type Request<B> = http::Request<RequestExt<B>>;

type Response = http::Response<Once<Bytes>>;

type Ctx<'a> = Context<'a, Request<RequestBody>, db_pool::Client>;

fn main() -> io::Result<()> {
    let cores = std::thread::available_parallelism().map(|num| num.get()).unwrap_or(56);

    let mut ids = core_affinity::get_core_ids().unwrap();

    let addr = "0.0.0.0:8080".parse::<SocketAddr>().unwrap();

    let worker = move |id: Option<core_affinity::CoreId>| {
        if let Some(id) = id {
            let _ = core_affinity::set_for_current(id);
        }

        compio::runtime::RuntimeBuilder::new().build().unwrap().block_on(async {
            let listener =
                compio::net::TcpListener::bind_with_options(addr, TcpOpts::new().reuse_address(true).reuse_port(true))
                    .await?;

            let service = Router::new()
                .insert(
                    "/plaintext",
                    get(fn_service(async |ctx: Ctx| {
                        let (req, _) = ctx.into_parts();
                        let mut res = req.into_response(const { Bytes::from_static(HELLO.as_bytes()) });
                        res.headers_mut().insert(CONTENT_TYPE, TEXT_UTF8);
                        Ok(res)
                    })),
                )
                .insert(
                    "/json",
                    get(fn_service(async |ctx: Ctx| {
                        let (req, _) = ctx.into_parts();
                        json_response(req, Message::HELLO)
                    })),
                )
                .insert(
                    "/db",
                    get(fn_service(async |ctx: Ctx| {
                        let (req, cli) = ctx.into_parts();
                        cli.db().await.and_then(|w| json_response(req, &w))
                    })),
                )
                .insert(
                    "/fortunes",
                    get(fn_service(async |ctx: Ctx| {
                        let (req, cli) = ctx.into_parts();
                        let fortunes = cli.fortunes().await?.render_once()?;
                        let mut res = req.into_response(Bytes::from(fortunes));
                        res.headers_mut().insert(CONTENT_TYPE, TEXT_HTML_UTF8);
                        Ok(res)
                    })),
                )
                .insert(
                    "/queries",
                    get(fn_service(async |ctx: Ctx| {
                        let (req, cli) = ctx.into_parts();
                        let num = req.uri().query().parse_query();
                        cli.queries(num).await.and_then(|w| json_response(req, &w))
                    })),
                )
                .insert(
                    "/updates",
                    get(fn_service(async |ctx: Ctx| {
                        let (req, cli) = ctx.into_parts();
                        let num = req.uri().query().parse_query();
                        cli.updates(num).await.and_then(|w| json_response(req, &w))
                    })),
                )
                .enclosed(ContextBuilder::new(db_pool::Client::create))
                .enclosed_fn(async |service, req| {
                    let mut res = service.call(req).await.unwrap_or_else(error_handler);
                    res.headers_mut().insert(SERVER, HeaderValue::from_static("x"));
                    Ok::<_, core::convert::Infallible>(res)
                });

            let router = service.call(()).await.unwrap();

            let service = Rc::new((router, Time::new()));

            loop {
                match listener.accept().await {
                    Ok((stream, addr)) => {
                        let service = service.clone();
                        compio::runtime::spawn(async move {
                            let _ = Dispatcher::<_, _, _, 64, { usize::MAX }, { usize::MAX }>::run(
                                stream, addr, &service.0, &service.1,
                            )
                            .await;
                        })
                        .detach();
                    }
                    Err(e) => return Err(e),
                };
            }
        })
    };

    let handle = core::iter::repeat_with(|| {
        let id = ids.pop();
        std::thread::spawn(move || worker(id))
    })
    .take(cores - 1)
    .collect::<Vec<_>>();

    // unrealistic due to no signal handling, not shutdown handling. when killing this process all resources that
    // need clean async shutdown will be leaked.
    worker(ids.pop())?;
    for handle in handle {
        let _ = handle.join().unwrap();
    }

    Ok(())
}

#[cold]
#[inline(never)]
fn error_handler(e: RouterError<util::Error>) -> Response {
    let status = match e {
        RouterError::Match(_) => StatusCode::NOT_FOUND,
        RouterError::NotAllowed(_) => StatusCode::METHOD_NOT_ALLOWED,
        RouterError::Service(e) => {
            eprintln!("Internal Error: {e}");
            StatusCode::INTERNAL_SERVER_ERROR
        }
    };
    http::Response::builder()
        .status(status)
        .body(Once::new(Bytes::new()))
        .unwrap()
}

fn json_response<Ext>(req: Request<Ext>, val: &impl serde_core::Serialize) -> HandleResult<Response> {
    let buf = ser::json_serialize(val)?;
    let mut res = req.into_response(Bytes::from(buf));
    res.headers_mut().insert(CONTENT_TYPE, JSON);
    Ok(res)
}

struct Time(Rc<RefCell<DateTimeState>>);

impl Time {
    fn new() -> Self {
        let state = Rc::new(RefCell::new(DateTimeState::default()));
        let state2 = state.clone();
        compio::runtime::spawn(async move {
            let mut interval = compio::runtime::time::interval(Duration::from_secs(1));
            loop {
                let _ = interval.tick().await;
                *state2.borrow_mut() = DateTimeState::default();
            }
        })
        .detach();
        Self(state)
    }
}

impl DateTime for Time {
    fn with_date<F, O>(&self, f: F) -> O
    where
        F: FnOnce(&[u8]) -> O,
    {
        let date = self.0.borrow();
        f(&date.date[..])
    }

    fn now(&self) -> tokio::time::Instant {
        self.0.borrow().now
    }
}

struct CompIoConnector;

impl xitca_postgres::pool::Connect for CompIoConnector {
    async fn connect(&self, cfg: xitca_postgres::Config) -> Result<xitca_postgres::Client, xitca_postgres::Error> {
        let (cli, drv) = compio::runtime::spawn_blocking(|| {
            tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap()
                .block_on(xitca_postgres::Postgres::new(cfg).connect())
        })
        .await
        .unwrap()?;

        let drv = drv.try_into_tcp().expect("raw tcp is used for database connection");
        let drv = xitca_postgres::CompIoDriver::from_tcp(drv)?;

        compio::runtime::spawn(async move {
            use core::{async_iter::AsyncIterator, future::poll_fn, pin::pin};

            let mut drv = pin!(drv.into_async_iter());
            while poll_fn(|cx| drv.as_mut().poll_next(cx)).await.is_some() {}
        })
        .detach();
        Ok(cli)
    }
}
