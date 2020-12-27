use hyper::server::conn::Http;
use std::io;
use std::net::SocketAddr;
use std::thread;
use tokio::net::{TcpListener, TcpStream};
use tokio::runtime::{Builder, Handle};

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
    let rt = Builder::new_current_thread().build().expect("runtime");
    let handle = rt.handle();

    // Bind to 0.0.0.0:8080
    let addr = SocketAddr::from(([0, 0, 0, 0], 8080));
    let tcp = reuse_listener(&addr).expect("couldn't bind to addr");

    rt.block_on(async move {
        // For every accepted connection, spawn an HTTP task
        loop {
            match tcp.accept().await {
                Ok((sock, _addr)) => {
                    let _ = sock.set_nodelay(true);
                    per_connection(sock, &mut http, &handle);
                }
                Err(e) => eprintln!("accept error: {}", e),
            }
        }
    });
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
    builder.listen(1024).and_then(|l| TcpListener::from_std(l))
}
