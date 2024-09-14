//! show case of axum running on proper thread per core server with io-uring enabled.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

mod db;
mod ser;
mod util;

use std::sync::Arc;

use axum::{
    extract::{Json, Query, State},
    http::{
        header::{HeaderValue, SERVER},
        StatusCode,
    },
    response::{Html, IntoResponse, Response},
    routing::{get, Router},
};
use tower_http::set_header::SetResponseHeaderLayer;

use crate::{db::Client, ser::Num, tower_compat::TowerHttp};

fn main() -> std::io::Result<()> {
    let service = TowerHttp::service(|| async {
        let cli = db::create().await?;
        let service = Router::new()
            .route("/plaintext", get(plain_text))
            .route("/json", get(json))
            .route("/db", get(db))
            .route("/fortunes", get(fortunes))
            .route("/queries", get(queries))
            .route("/updates", get(updates))
            .with_state(Arc::new(cli))
            .layer(SetResponseHeaderLayer::if_not_present(
                SERVER,
                HeaderValue::from_static("A"),
            ));
        Ok(service)
    });
    xitca_server::Builder::new()
        .bind("xitca-axum", "0.0.0.0:8080", service)?
        .build()
        .wait()
}

async fn plain_text() -> &'static str {
    "Hello, World!"
}

async fn json() -> impl IntoResponse {
    Json(ser::Message::new())
}

async fn db(State(cli): State<Arc<Client>>) -> impl IntoResponse {
    cli.get_world().await.map(Json).map_err(Error)
}

async fn fortunes(State(cli): State<Arc<Client>>) -> impl IntoResponse {
    use sailfish::TemplateOnce;
    cli.tell_fortune()
        .await
        .map_err(Error)?
        .render_once()
        .map(Html)
        .map_err(|e| Error(Box::new(e)))
}

async fn queries(State(cli): State<Arc<Client>>, Query(Num(num)): Query<Num>) -> impl IntoResponse {
    cli.get_worlds(num).await.map(Json).map_err(Error)
}

async fn updates(State(cli): State<Arc<Client>>, Query(Num(num)): Query<Num>) -> impl IntoResponse {
    cli.update(num).await.map(Json).map_err(Error)
}

struct Error(util::Error);

impl IntoResponse for Error {
    fn into_response(self) -> Response {
        let mut res = self.0.to_string().into_response();
        *res.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
        res
    }
}

// compat module between xitca-http and axum.
mod tower_compat {
    use core::{cell::RefCell, fmt, future::Future, marker::PhantomData};

    use std::net::SocketAddr;

    use http_body::Body;
    use xitca_http::{
        bytes::Bytes,
        h1::RequestBody,
        http::{Request, RequestExt, Response},
        HttpServiceBuilder,
    };
    use xitca_io::net::io_uring::TcpStream;
    use xitca_service::{fn_build, middleware::UncheckedReady, ready::ReadyService, Service, ServiceExt};
    use xitca_web::service::tower_http_compat::{CompatReqBody, CompatResBody};

    pub struct TowerHttp<S, B> {
        service: RefCell<S>,
        _p: PhantomData<fn(B)>,
    }

    impl<S, B> TowerHttp<S, B> {
        pub fn service<F, Fut>(
            func: F,
        ) -> impl Service<Response = impl ReadyService + Service<(TcpStream, SocketAddr)>, Error = impl fmt::Debug>
        where
            F: Fn() -> Fut + Send + Sync + Clone,
            Fut: Future<Output = Result<S, crate::util::Error>>,
            S: tower::Service<Request<CompatReqBody<RequestExt<RequestBody>, ()>>, Response = Response<B>>,
            S::Error: fmt::Debug,
            B: Body<Data = Bytes> + Send + 'static,
        {
            fn_build(move |_| {
                let func = func.clone();
                async move {
                    func().await.map(|service| TowerHttp {
                        service: RefCell::new(service),
                        _p: PhantomData,
                    })
                }
            })
            .enclosed(UncheckedReady)
            .enclosed(HttpServiceBuilder::h1().io_uring())
        }
    }

    impl<S, B> Service<Request<RequestExt<RequestBody>>> for TowerHttp<S, B>
    where
        S: tower::Service<Request<CompatReqBody<RequestExt<RequestBody>, ()>>, Response = Response<B>>,
    {
        type Response = Response<CompatResBody<B>>;
        type Error = S::Error;

        async fn call(&self, req: Request<RequestExt<RequestBody>>) -> Result<Self::Response, Self::Error> {
            let (parts, ext) = req.into_parts();
            let req = Request::from_parts(parts, CompatReqBody::new(ext, ()));
            let fut = self.service.borrow_mut().call(req);
            let (parts, body) = fut.await?.into_parts();
            Ok(Response::from_parts(parts, CompatResBody::new(body)))
        }
    }
}
