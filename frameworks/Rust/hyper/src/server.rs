use std::io;
use std::net::SocketAddr;
use std::thread;

use hyper::server::conn::Http;
use tokio::net::{TcpListener, TcpStream};
use tokio::runtime::{Builder as RuntimeBuilder, Handle};

pub(crate) fn run<F>(per_connection: F)
where
    F: Fn(TcpStream, &mut Http, Handle) + Clone + Send + 'static,
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
    F: Fn(TcpStream, &mut Http, Handle) + Send + 'static,
{
    let mut http = Http::new();
    http.http1_only(true);

    // Our event loop...
    let core = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let handle = core.handle();

    // Bind to 0.0.0.0:8080
    let addr = SocketAddr::from(([0, 0, 0, 0], 8080));

    // For every accepted connection, spawn an HTTP task
    let server = async move {
        let tcp = reuse_listener(&addr).expect("couldn't bind to addr");
        loop {
            match tcp.accept().await {
                Ok((sock, _)) => {
                    let _ = sock.set_nodelay(true);
                    per_connection(sock, &mut http, handle.clone());
                }
                Err(e) => {
                    log::warn!("accept error: {}", e)
                }
            }
        }
    };

    core.block_on(server);
}

fn reuse_listener(addr: &SocketAddr) -> io::Result<TcpListener> {
    let builder = match *addr {
        SocketAddr::V4(_) => net2::TcpBuilder::new_v4()?,
        SocketAddr::V6(_) => net2::TcpBuilder::new_v6()?,
    };

    #[cfg(unix)]
    {
        use net2::unix::UnixTcpBuilderExt;
        if let Err(e) = builder.reuse_port(true) {
            eprintln!("error setting SO_REUSEPORT: {}", e);
        }
    }

    builder.reuse_address(true)?;
    builder.bind(addr)?;
    let listener = builder.listen(1024)?;
    listener.set_nonblocking(true)?;
    TcpListener::from_std(listener)
}
