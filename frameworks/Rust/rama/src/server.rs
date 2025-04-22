use std::{
    future::Future,
    io,
    net::{Ipv4Addr, SocketAddr},
};

use rama::{
    Layer,
    http::{
        layer::required_header::AddRequiredResponseHeadersLayer, server::HttpServer,
        service::web::Router,
    },
    net::socket::{Domain, Socket, Type},
    rt::Executor,
    tcp::server::TcpListener,
};

/// Reuse an existing listener, ensuring that the socket `backlog`
/// is set to enable a higher number of pending connections.
fn set_socket_options(addr: SocketAddr) -> io::Result<TcpListener<()>> {
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

    socket.try_into()
}

/// Build a Rama server with consistent configuration, using the high-level API exposed
/// by rama 0.2. This is intended for convenience and intentionally does not provide much
/// customisability.
#[allow(dead_code)]
pub async fn serve<State: Clone + Send + Sync + 'static>(
    state: State,
    app: Router<State>,
    port: Option<u16>,
) {
    let port = port.unwrap_or(8000);
    let addr = SocketAddr::from((Ipv4Addr::UNSPECIFIED, port));
    let listener = set_socket_options(addr).expect("couldn't bind to address");
    println!("started rama server on port: {port}");

    let app = AddRequiredResponseHeadersLayer::default().layer(app);
    let http_service = HttpServer::auto(Executor::default()).service(app);

    listener.with_state(state).serve(http_service).await;
}

/// Start a single-threaded tokio runtime on multiple threads.
#[allow(dead_code)]
pub fn start_tokio<Fut>(f: fn() -> Fut)
where
    Fut: Future<Output = ()> + 'static,
{
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();

    for _ in 1..num_cpus::get() {
        std::thread::spawn(move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();
            rt.block_on(f());
        });
    }
    rt.block_on(f());
}
