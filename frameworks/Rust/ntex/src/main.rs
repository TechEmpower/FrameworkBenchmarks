#[global_allocator]
static GLOBAL: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

use std::sync::{Arc, Mutex};

use ntex::http::header::{CONTENT_TYPE, SERVER};
use ntex::{http, time::Seconds, util::BytesMut, util::PoolId, web};
use yarte::Serialize;

mod utils;

#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

#[web::get("/json")]
async fn json() -> web::HttpResponse {
    let mut body = BytesMut::with_capacity(utils::SIZE);
    Message {
        message: "Hello, World!",
    }
    .to_bytes_mut(&mut body);

    let mut response = web::HttpResponse::with_body(http::StatusCode::OK, body.into());
    response.headers_mut().insert(SERVER, utils::HDR_SERVER);
    response
        .headers_mut()
        .insert(CONTENT_TYPE, utils::HDR_JSON_CONTENT_TYPE);
    response
}

#[web::get("/plaintext")]
async fn plaintext() -> web::HttpResponse {
    let mut response = web::HttpResponse::with_body(
        http::StatusCode::OK,
        http::body::Body::Bytes(utils::BODY_PLAIN_TEXT),
    );
    response.headers_mut().insert(SERVER, utils::HDR_SERVER);
    response
        .headers_mut()
        .insert(CONTENT_TYPE, utils::HDR_TEXT_CONTENT_TYPE);
    response
}

#[ntex::main]
async fn main() -> std::io::Result<()> {
    println!("Started http server: 127.0.0.1:8080");

    let cores = core_affinity::get_core_ids().unwrap();
    let total_cores = cores.len();
    let cores = Arc::new(Mutex::new(cores));

    // start http server
    ntex::server::build()
        .backlog(1024)
        .configure(move |cfg| {
            let cores = cores.clone();
            cfg.on_worker_start(move |_| {
                if let Some(core) = cores.lock().unwrap().pop() {
                    // Pin this worker to a single CPU core.
                    core_affinity::set_for_current(core);
                }
                std::future::ready(Ok::<_, &'static str>(()))
            })
        })?
        .bind("techempower", "0.0.0.0:8080", |cfg| {
            cfg.memory_pool(PoolId::P1);
            PoolId::P1.set_read_params(65535, 2048);
            PoolId::P1.set_write_params(65535, 2048);

            http::HttpService::build()
                .keep_alive(http::KeepAlive::Os)
                .client_timeout(Seconds(0))
                .h1(web::App::new().service(json).service(plaintext).finish())
        })?
        .workers(total_cores)
        .run()
        .await
}
