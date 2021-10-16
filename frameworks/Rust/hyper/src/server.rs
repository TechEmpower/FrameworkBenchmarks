use std::io;
use std::net::SocketAddr;
use std::thread;

use futures::{Future, Stream};
use hyper::server::conn::Http;
use tokio_core::net::{TcpListener, TcpStream};
use tokio_core::reactor::{Core, Handle};

pub(crate) fn run<F>(per_connection: F)
where
    F: Fn(TcpStream, &mut Http, &Handle) + Clone + Send + 'static,
{
    // Spawn a thread for each available core, minus one, since we'll
    // reuse the main thread as a server thread as well.
    for _ in 1..num_cpus::get() {
        let per_connection = per_connection.clone();
        thread::spawn(move || {
            server_thread(per_connection);
        });
    }
    server_thread(per_connection);
}

fn server_thread<F>(per_connection: F)
where
    F: Fn(TcpStream, &mut Http, &Handle) + Send + 'static,
{
    let mut http = Http::new();
    http.http1_only(true);

    // Our event loop...
    let mut core = Core::new().expect("core");
    let handle = core.handle();

    // Bind to 0.0.0.0:8080
    let addr = SocketAddr::from(([0, 0, 0, 0], 8080));
    let tcp = reuse_listener(&addr, &handle).expect("couldn't bind to addr");

    // For every accepted connection, spawn an HTTP task
    let server = tcp
        .incoming()
        .for_each(move |(sock, _addr)| {
            per_connection(sock, &mut http, &handle);
            Ok(())
        })
        .map_err(|e| eprintln!("accept error: {}", e));

    core.run(server).expect("server");
}

fn reuse_listener(addr: &SocketAddr, handle: &Handle) -> io::Result<TcpListener> {
    let socket = match *addr {
        SocketAddr::V4(_) => socket2::Socket::new(socket2::Domain::IPV4, socket2::Type::STREAM, None)?,
        SocketAddr::V6(_) => socket2::Socket::new(socket2::Domain::IPV6, socket2::Type::STREAM, None)?,
    };

    socket.set_reuse_address(true)?;
    socket.set_reuse_port(true)?;
    // Accepted socket can inherit TCP_NODELAY form listening socket.
    // https://github.com/h2o/h2o/pull/1568
    socket.set_nodelay(true)?;
    socket.bind(&socket2::SockAddr::from(*addr))?;
    socket.listen(4096)?;
    TcpListener::from_listener(socket.into(), addr, handle)
}
