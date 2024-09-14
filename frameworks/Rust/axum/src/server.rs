use std::{
    io,
    net::{Ipv4Addr, SocketAddr, TcpListener},
};

use axum::{
    http::{header, HeaderValue},
    Router,
};

use hyper::body::Incoming;
use hyper::Request;
use hyper_util::rt::{TokioExecutor, TokioIo};
use tower::Service;
use tower_http::set_header::SetResponseHeaderLayer;

use socket2::{Domain, Socket, Type};

/// Reuse an existing listener, ensuring that the socket `backlog``
/// is set to enable a higher number of pending connections.
fn set_socket_options(addr: SocketAddr) -> io::Result<tokio::net::TcpListener> {
    let socket = match addr {
        SocketAddr::V4(_) => Socket::new(Domain::IPV4, Type::STREAM, None)?,
        SocketAddr::V6(_) => Socket::new(Domain::IPV6, Type::STREAM, None)?,
    };

    socket.set_reuse_port(true)?;
    socket.set_reuse_address(true)?;
    socket.set_nonblocking(true)?;
    socket.set_nodelay(true)?;
    socket.bind(&addr.into())?;
    socket.listen(4096)?;

    let listener: TcpListener = socket.into();
    tokio::net::TcpListener::from_std(listener)
}

/// Build an Axum server with consistent configuration, using the high-level API exposed
/// by Axum 0.7. This is intended for convenience and intentionally does not provide much
/// customisability.
#[allow(dead_code)]
pub async fn serve(app: Router<()>, port: Option<u16>) {
    let port = port.unwrap_or(8000);
    let addr = SocketAddr::from((Ipv4Addr::UNSPECIFIED, port));
    let listener = set_socket_options(addr).expect("couldn't bind to address");
    println!("started axum server on port {port}.");

    let server_header_value = HeaderValue::from_static("Axum");
    let app = app.layer(SetResponseHeaderLayer::overriding(
        header::SERVER,
        server_header_value,
    ));

    axum::serve(listener, app.into_make_service())
        .await
        .unwrap();
}

/// Build an Axum server using the lower-level Hyper APIs for more
/// configurability. This has a few optimisations, including:
/// * Serving HTTP/1 only.
/// * Disabling connection upgrades (websockets are not needed).
/// * Setting TCP_NODELAY on the input stream.
/// * Aggregating flushes to better support pipelined responses.
///
/// See for more details:
/// * https://github.com/tokio-rs/axum/blob/1ac617a1b540e8523347f5ee889d65cad9a45ec4/examples/serve-with-hyper/src/main.rs
#[allow(dead_code)]
pub async fn serve_hyper(app: Router<()>, port: Option<u16>) {
    let port = port.unwrap_or(8000);
    let addr = SocketAddr::from((Ipv4Addr::UNSPECIFIED, port));
    let listener = set_socket_options(addr).expect("couldn't bind to address");
    println!("started axum server on port {port}.");

    let server_header_value = HeaderValue::from_static("Axum");
    let app = app.layer(SetResponseHeaderLayer::overriding(
        header::SERVER,
        server_header_value,
    ));

    // Continuously accept new connections.
    loop {
        let (socket, _remote_addr) = listener.accept().await.unwrap();
        socket
            .set_nodelay(true)
            .expect("could not set TCP_NODELAY!");

        let tower_service = app.clone();
        tokio::spawn(async move {
            let socket = TokioIo::new(socket);

            let hyper_service =
                hyper::service::service_fn(move |request: Request<Incoming>| {
                    tower_service.clone().call(request)
                });

            if (hyper_util::server::conn::auto::Builder::new(TokioExecutor::new())
                .http1()
                .pipeline_flush(true)
                .serve_connection(socket, hyper_service)
                .await)
                .is_err()
            {}
        });
    }
}
