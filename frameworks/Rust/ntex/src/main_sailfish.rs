#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::borrow::Cow;
use std::future::Future;
use std::io;
use std::pin::Pin;
use std::task::{Context, Poll};

use bytes::Bytes;
use futures::future::{ok, FutureExt};
use futures::stream::StreamExt;
use ntex::http::body::Body;
use ntex::http::header::{HeaderValue, CONTENT_TYPE, SERVER};
use ntex::http::{HttpService, KeepAlive, Request, Response, StatusCode};
use ntex::service::{Service, ServiceFactory};
use ntex::web::Error;
use sailfish::TemplateOnce;
use smallvec::{smallvec, SmallVec};
use tokio_postgres::{connect, Client, NoTls, Statement};

struct Fortune {
    id: i32,
    message: Cow<'static, str>,
}

#[derive(TemplateOnce)]
#[template(path = "fortune.stpl", rm_whitespace = true)]
struct Fortunes {
    items: SmallVec<[Fortune; 32]>,
}

struct App {
    hdr_srv: HeaderValue,
    hdr_cthtml: HeaderValue,
    db: Client,
    fortune: Statement,
}

impl Service for App {
    type Request = Request;
    type Response = Response;
    type Error = Error;
    type Future = Pin<Box<dyn Future<Output = Result<Response, Error>>>>;

    #[inline]
    fn poll_ready(&self, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn call(&self, req: Request) -> Self::Future {
        let path = req.path();
        match path {
            "/fortunes" => {
                let h_srv = self.hdr_srv.clone();
                let h_ct = self.hdr_cthtml.clone();
                let fut = self.db.query_raw(&self.fortune, &[]);

                Box::pin(async move {
                    let mut stream = fut.await.map_err(|e| {
                        io::Error::new(io::ErrorKind::Other, format!("{:?}", e))
                    })?;

                    let mut items: SmallVec<[_; 32]> = smallvec![Fortune {
                        id: 0,
                        message: Cow::Borrowed(
                            "Additional fortune added at request time."
                        ),
                    }];

                    while let Some(row) = stream.next().await {
                        let row = row.map_err(|e| {
                            io::Error::new(io::ErrorKind::Other, format!("{:?}", e))
                        })?;
                        items.push(Fortune {
                            id: row.get(0),
                            message: Cow::Owned(row.get(1)),
                        });
                    }

                    items.sort_by(|it, next| it.message.cmp(&next.message));

                    let body = match (Fortunes { items }).render_once() {
                        Ok(body) => Ok(Bytes::from(body)),
                        Err(e) => {
                            Err(io::Error::new(io::ErrorKind::Other, e.to_string()))
                        }
                    }?;

                    let mut res = Response::with_body(StatusCode::OK, Body::Bytes(body));
                    let hdrs = res.headers_mut();
                    hdrs.insert(SERVER, h_srv);
                    hdrs.insert(CONTENT_TYPE, h_ct);
                    Ok(res)
                })
            }
            _ => Box::pin(ok(Response::new(http::StatusCode::NOT_FOUND))),
        }
    }
}

#[derive(Clone)]
struct AppFactory;

impl ServiceFactory for AppFactory {
    type Config = ();
    type Request = Request;
    type Response = Response;
    type Error = Error;
    type Service = App;
    type InitError = ();
    type Future = Pin<Box<dyn Future<Output = Result<Self::Service, Self::InitError>>>>;

    fn new_service(&self, _: ()) -> Self::Future {
        const DB_URL: &str =
            "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

        Box::pin(async move {
            let (db, conn) = connect(DB_URL, NoTls)
                .await
                .expect("can not connect to postgresql");
            ntex::rt::spawn(conn.map(|_| ()));

            let fortune = db.prepare("SELECT * FROM fortune").await.unwrap();

            Ok(App {
                hdr_srv: HeaderValue::from_static("N"),
                hdr_cthtml: HeaderValue::from_static("text/html; charset=utf-8"),
                db,
                fortune,
            })
        })
    }
}

#[ntex::main]
async fn main() -> std::io::Result<()> {
    println!("Starting http server: 127.0.0.1:8080");

    ntex::server::build()
        .backlog(1024)
        .bind("techempower", "0.0.0.0:8080", || {
            HttpService::build()
                .keep_alive(KeepAlive::Os)
                .client_timeout(0)
                .h1(AppFactory)
                .tcp()
        })?
        .start()
        .await
}
