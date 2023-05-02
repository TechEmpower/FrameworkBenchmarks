use std::io;
use std::net::{Ipv4Addr, SocketAddr};
use std::{env, fmt::Debug, str::FromStr};

use tokio::net::{TcpListener, TcpSocket};

#[allow(dead_code)]
pub fn get_env_var<T: FromStr>(key: &str) -> T
where
    <T as FromStr>::Err: Debug,
{
    env::var(key)
        .unwrap_or_else(|_| panic!("{key} environment variable was not set"))
        .parse::<T>()
        .unwrap_or_else(|_| panic!("could not parse {key}"))
}

#[allow(dead_code)]
pub fn reuse_listener() -> io::Result<TcpListener> {
    let addr = SocketAddr::from((Ipv4Addr::UNSPECIFIED, 8080));
    let socket = match addr {
        SocketAddr::V4(_) => TcpSocket::new_v4()?,
        SocketAddr::V6(_) => TcpSocket::new_v6()?,
    };

    #[cfg(unix)]
    {
        if let Err(e) = socket.set_reuseport(true) {
            eprintln!("error setting SO_REUSEPORT: {e}");
        }
    }

    socket.set_reuseaddr(true)?;
    socket.bind(addr)?;
    socket.listen(1024)
}
