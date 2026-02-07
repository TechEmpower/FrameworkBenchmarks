use std::convert::Infallible;
use std::net::{Ipv4Addr, SocketAddr};
use std::{io, thread};

use clap::{Parser, ValueEnum};
use http_body_util::combinators::BoxBody;
use http_body_util::{BodyExt, Empty, Full};
use hyper::body::{Bytes, Incoming};
use hyper::header::{HeaderValue, SERVER};
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::{Request, Response, StatusCode};
use hyper_util::rt::TokioIo;
use socket2::{Domain, SockAddr, Socket};
use strum::Display;
use thiserror::Error;
use tokio::net::TcpListener;
use tokio::runtime;
use tracing::{error, info};

mod db;
mod fortunes;
mod json;
mod multiple_queries;
mod plaintext;
mod single_query;

static SERVER_HEADER: HeaderValue = HeaderValue::from_static("hyper");
static APPLICATION_JSON: HeaderValue = HeaderValue::from_static("application/json");
static TEXT_HTML: HeaderValue = HeaderValue::from_static("text/html; charset=utf-8");
static TEXT_PLAIN: HeaderValue = HeaderValue::from_static("text/plain");

type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, Error)]
enum Error {
    #[error("I/O error: {0}")]
    Io(#[from] io::Error),
    #[error("Hyper error: {0}")]
    Hyper(#[from] hyper::Error),
    #[error("Database error: {0}")]
    TokioPostgres(#[from] tokio_postgres::Error),
    #[error("Http error: {0}")]
    Http(#[from] http::Error),
    #[error("Database pool error: {0}")]
    DbPool(#[from] deadpool_postgres::PoolError),
    #[error("Serde error: {0}")]
    Serde(#[from] serde_json::Error),
}

#[derive(Debug, Parser)]
struct Args {
    /// The runtime to use.
    #[arg(short, long, default_value_t = Runtime::default())]
    runtime: Runtime,

    /// The number of threads to use.
    ///
    /// Defaults to the number of logical CPUs cores available on the system.
    ///
    /// - For the current thread runtime, this is the number of threads to spawn in addition to the
    ///   main thread.
    /// - For the multi-thread runtime, this is the number of worker threads to configure the
    ///   runtime to use.
    #[arg(short, long, default_value_t = num_cpus::get())]
    threads: usize,
}

#[derive(Clone, Debug, Default, Display, ValueEnum)]
#[strum(serialize_all = "kebab-case")]
enum Runtime {
    #[default]
    CurrentThread,
    MultiThread,
}

fn main() -> Result<()> {
    // Note: this is only here to capture logs outside of the hot path code. Avoid logging messages
    // in the hot path code.
    tracing_subscriber::fmt().with_thread_ids(true).init();

    let args = Args::parse();
    match args.runtime {
        Runtime::CurrentThread => run_current_thread(args.threads)?,
        Runtime::MultiThread => run_multi_thread(args.threads)?,
    }

    Ok(())
}

/// Runs the server using multiple current thread runtimes.
fn run_current_thread(threads: usize) -> Result<()> {
    info!("Running with {} threads", threads);

    // Spawn a new runtime on each thread.
    for _ in 1..threads {
        let runtime = runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;
        thread::spawn(|| run_server(runtime));
    }
    // Run the server on the main thread.
    let runtime = runtime::Builder::new_current_thread()
        .enable_all()
        .build()?;
    run_server(runtime)
}

/// Runs the server using a single multi-thread runtime.
fn run_multi_thread(threads: usize) -> Result<()> {
    let runtime = runtime::Builder::new_multi_thread()
        .enable_all()
        .worker_threads(threads)
        .build()?;
    run_server(runtime)
}

fn run_server(runtime: runtime::Runtime) -> Result<()> {
    // It's important to use [`Runtime::block_on()`] here and not [`handle::block_on()`] as
    // otherwise the runtime will not drive I/O operations. See the [`Handle::block_on`]
    // documentation for more information.
    runtime.block_on(serve(runtime.handle()))
}

async fn serve(handle: &runtime::Handle) -> Result<()> {
    let addr = SocketAddr::from((Ipv4Addr::UNSPECIFIED, 8080));
    let socket = create_socket(addr)?;

    let listener = TcpListener::from_std(socket.into())?;
    let addr = listener.local_addr()?;
    info!("Listening on: {}", addr);

    // spawn accept loop into a task so it is scheduled on the runtime with all the other tasks.
    let accept_loop = accept_loop(handle.clone(), listener);
    handle.spawn(accept_loop).await.unwrap()
}

/// Create a socket that allows reuse of the address and port.
///
/// This makes it possible for multiple instances of the server task to bind to the same address and
/// port.
fn create_socket(addr: SocketAddr) -> Result<Socket> {
    let domain = match addr {
        SocketAddr::V4(_) => Domain::IPV4,
        SocketAddr::V6(_) => Domain::IPV6,
    };
    let addr = SockAddr::from(addr);
    let socket = Socket::new(domain, socket2::Type::STREAM, None)?;
    let backlog = 4096; // maximum number of pending connections
    #[cfg(unix)]
    socket.set_reuse_port(true)?;
    socket.set_reuse_address(true)?;
    socket.set_nodelay(true)?;
    socket.set_nonblocking(true)?; // required for tokio
    socket.bind(&addr)?;
    socket.listen(backlog)?;

    Ok(socket)
}

/// Accept loop that accepts incoming connections and spawns a new task to handle each connection.
async fn accept_loop(handle: runtime::Handle, listener: TcpListener) -> Result<()> {
    let mut http = http1::Builder::new();
    http.pipeline_flush(true);

    let service = service_fn(router);
    loop {
        let (stream, _) = listener.accept().await?;
        let http = http.clone();
        handle.spawn(async move {
            let io = TokioIo::new(stream);
            if let Err(_e) = http.serve_connection(io, service).await {
                // ignore errors until https://github.com/hyperium/hyper/pull/3863/ is merged
                // This PR will allow us to filter out shutdown errors which are expected.
                // warn!("Connection error (this may be normal during shutdown): {e}");
            }
        });
    }
}

/// Routes requests to the appropriate handler.
async fn router(request: Request<Incoming>) -> Result<Response<BoxBody<Bytes, Infallible>>> {
    // The method is always GET, so we don't check it.
    match request.uri().path() {
        "/ping" => ping(),
        "/json" => json::get(),
        "/db" => single_query::get().await,
        "/queries" => multiple_queries::get(request.uri().query()).await,
        "/fortunes" => fortunes::get().await,
        "/plaintext" => plaintext::get(),
        _ => not_found_error(),
    }
}

/// A handler that returns a "pong" response.
///
/// This handler is used to verify that the server is running and can respond to requests. It is
/// used by the docker health check command.
fn ping() -> Result<Response<BoxBody<Bytes, Infallible>>> {
    Response::builder()
        .body(Full::from("pong").boxed())
        .map_err(Error::from)
}

/// A handler that returns a 404 response.
fn not_found_error() -> Result<Response<BoxBody<Bytes, Infallible>>> {
    Response::builder()
        .status(StatusCode::NOT_FOUND)
        .header(SERVER, SERVER_HEADER.clone())
        .body(Empty::new().boxed())
        .map_err(Error::from)
}
