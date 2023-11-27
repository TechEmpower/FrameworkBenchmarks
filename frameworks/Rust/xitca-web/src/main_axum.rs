//! show case of axum running on proper thread per core server with io-uring enabled.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

mod ser;
mod util;

use axum::{
    http::header::{HeaderValue, SERVER},
    response::IntoResponse,
    routing::{get, Router},
    Json,
};
use tower_http::set_header::SetResponseHeaderLayer;

use crate::tower_compat::TowerHttp;

fn main() -> std::io::Result<()> {
    let service = TowerHttp::service(|| async {
        Router::new()
            .route("/plaintext", get(plain_text))
            .route("/json", get(json))
            .layer(SetResponseHeaderLayer::if_not_present(
                SERVER,
                HeaderValue::from_static("A"),
            ))
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

mod tower_compat {
    use std::{
        cell::RefCell,
        convert::Infallible,
        error, fmt,
        future::Future,
        io,
        marker::PhantomData,
        net::SocketAddr,
        pin::Pin,
        task::{Context, Poll},
    };

    use axum::extract::ConnectInfo;
    use futures_core::stream::Stream;
    use http_body::Body;
    use pin_project_lite::pin_project;
    use xitca_http::{
        body::none_body_hint,
        bytes::Bytes,
        h1::RequestBody,
        http::{HeaderMap, Request, RequestExt, Response},
        BodyError, HttpServiceBuilder,
    };
    use xitca_io::net::io_uring::TcpStream;
    use xitca_service::{
        fn_build, middleware::UncheckedReady, ready::ReadyService, Service, ServiceExt,
    };
    use xitca_unsafe_collection::fake_send_sync::FakeSend;

    pub struct TowerHttp<S, B> {
        service: RefCell<S>,
        _p: PhantomData<fn(B)>,
    }

    impl<S, B> TowerHttp<S, B> {
        pub fn service<F, Fut>(
            service: F,
        ) -> impl Service<
            Response = impl ReadyService + Service<(TcpStream, SocketAddr)>,
            Error = impl fmt::Debug,
        >
        where
            F: Fn() -> Fut + Send + Sync + Clone,
            Fut: Future<Output = S>,
            S: tower::Service<Request<_RequestBody>, Response = Response<B>>,
            S::Error: fmt::Debug,
            B: Body<Data = Bytes> + Send + 'static,
            B::Error: error::Error + Send + Sync,
        {
            fn_build(move |_| {
                let service = service.clone();
                async move {
                    let service = service().await;
                    Ok::<_, Infallible>(TowerHttp {
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
        S: tower::Service<Request<_RequestBody>, Response = Response<B>>,
        B: Body<Data = Bytes> + Send + 'static,
        B::Error: error::Error + Send + Sync,
    {
        type Response = Response<ResponseBody<B>>;
        type Error = S::Error;

        async fn call(
            &self,
            req: Request<RequestExt<RequestBody>>,
        ) -> Result<Self::Response, Self::Error> {
            let (parts, ext) = req.into_parts();
            let (ext, body) = ext.replace_body(());
            let body = _RequestBody {
                body: FakeSend::new(body),
            };
            let mut req = Request::from_parts(parts, body);
            let _ = req.extensions_mut().insert(ConnectInfo(*ext.socket_addr()));
            let fut = self.service.borrow_mut().call(req);
            let (parts, body) = fut.await?.into_parts();
            let body = ResponseBody { body };
            let res = Response::from_parts(parts, body);
            Ok(res)
        }
    }

    pub struct _RequestBody {
        body: FakeSend<RequestBody>,
    }

    impl Body for _RequestBody {
        type Data = Bytes;
        type Error = io::Error;

        fn poll_data(
            self: Pin<&mut Self>,
            cx: &mut Context<'_>,
        ) -> Poll<Option<Result<Self::Data, Self::Error>>> {
            Pin::new(&mut *self.get_mut().body).poll_next(cx)
        }

        fn poll_trailers(
            self: Pin<&mut Self>,
            _: &mut Context<'_>,
        ) -> Poll<Result<Option<HeaderMap>, Self::Error>> {
            Poll::Ready(Ok(None))
        }
    }

    pin_project! {
        pub struct ResponseBody<B> {
            #[pin]
            body: B
        }
    }

    impl<B> Stream for ResponseBody<B>
    where
        B: Body<Data = Bytes>,
        B::Error: error::Error + Send + Sync + 'static,
    {
        type Item = Result<Bytes, BodyError>;

        fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
            self.project()
                .body
                .poll_data(cx)
                .map_err(|e| BodyError::from(Box::new(e) as Box<dyn error::Error + Send + Sync>))
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            if Body::is_end_stream(&self.body) {
                return none_body_hint();
            }
            let hint = Body::size_hint(&self.body);
            (hint.lower() as _, hint.upper().map(|u| u as _))
        }
    }
}
