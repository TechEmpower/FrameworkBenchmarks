#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use ntex::http::header::{CONTENT_TYPE, SERVER};
use ntex::{http, time::Seconds, util::BytesMut, util::PoolId, util::Ready, web};
use sonic_rs::Serialize;

mod utils;

#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

#[web::get("/json")]
async fn json() -> web::HttpResponse {
    let mut body = BytesMut::with_capacity(utils::SIZE);
    sonic_rs::to_writer(
        utils::BytesWriter(&mut body),
        &Message {
            message: "Hello, World!",
        },
    )
    .unwrap();

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
    let cores = std::sync::Arc::new(std::sync::Mutex::new(cores));

    // start http server
    ntex::server::build()
        .backlog(1024)
        .bind("techempower", "0.0.0.0:8080", |cfg| {
            cfg.memory_pool(PoolId::P1);
            PoolId::P1.set_read_params(65535, 2048);
            PoolId::P1.set_write_params(65535, 2048);

            http::HttpService::build()
                .keep_alive(http::KeepAlive::Os)
                .client_timeout(Seconds::ZERO)
                .headers_read_rate(Seconds::ZERO, Seconds::ZERO, 0)
                .payload_read_rate(Seconds::ZERO, Seconds::ZERO, 0)
                .h1(web::App::new().service(json).service(plaintext).finish())
        })?
        .configure(move |cfg| {
            let cores = cores.clone();
            cfg.on_worker_start(move |_| {
                if let Some(core) = cores.lock().unwrap().pop() {
                    core_affinity::set_for_current(core);
                }
                Ready::<_, &str>::Ok(())
            });
            Ok(())
        })?
        .workers(total_cores)
        .run()
        .await
}
