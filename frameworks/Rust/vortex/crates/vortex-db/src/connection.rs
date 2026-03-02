//! PostgreSQL connection — connect, authenticate, prepare statements, execute queries.
//!
//! Uses blocking TCP with BufReader for efficient reads. All hot-path query
//! methods batch writes and reuse buffers to eliminate per-request allocations.

use crate::wire;
use std::io::{self, BufReader, Write};
use std::net::TcpStream;
use std::os::fd::{IntoRawFd, RawFd};
use std::time::Duration;

/// PostgreSQL OID for int4.
const OID_INT4: u32 = 23;
/// PostgreSQL OID for int4[].
const OID_INT4_ARRAY: u32 = 1007;

/// Pre-defined prepared statement names.
pub const STMT_WORLD: &str = "w";
pub const STMT_FORTUNE: &str = "f";
pub const STMT_UPDATE: &str = "u";
pub const STMT_UPDATE_BATCH: &str = "ub";

/// A single PostgreSQL connection with prepared statements and reusable buffers.
pub struct PgConnection {
    reader: BufReader<TcpStream>,
    wbuf: Vec<u8>,
    rbuf: Vec<u8>,
}

/// Database configuration.
pub struct DbConfig {
    pub host: String,
    pub port: u16,
    pub user: String,
    pub password: String,
    pub database: String,
}

impl DbConfig {
    /// Read config from environment variables with TechEmpower defaults.
    pub fn from_env() -> Self {
        Self {
            host: std::env::var("DB_HOST").unwrap_or_else(|_| "tfb-database".to_string()),
            port: std::env::var("DB_PORT")
                .ok()
                .and_then(|s| s.parse().ok())
                .unwrap_or(5432),
            user: std::env::var("DB_USER").unwrap_or_else(|_| "benchmarkdbuser".to_string()),
            password: std::env::var("DB_PASS").unwrap_or_else(|_| "benchmarkdbpass".to_string()),
            database: std::env::var("DB_NAME").unwrap_or_else(|_| "hello_world".to_string()),
        }
    }
}

impl PgConnection {
    /// Resolve the DB host once. Call this on the main thread before spawning workers.
    pub fn resolve_host(config: &DbConfig) -> io::Result<std::net::SocketAddr> {
        let addr = format!("{}:{}", config.host, config.port);
        resolve_addr(&addr)
    }

    /// Connect to PostgreSQL using a pre-resolved address.
    /// Retries up to 10 times with exponential backoff.
    pub fn connect_resolved(addr: std::net::SocketAddr, config: &DbConfig) -> io::Result<Self> {
        let mut last_err = io::Error::new(io::ErrorKind::TimedOut, "failed to connect");

        for attempt in 0..10 {
            if attempt > 0 {
                std::thread::sleep(Duration::from_millis(50 * (1 << attempt.min(5))));
            }

            let stream = match TcpStream::connect_timeout(&addr, Duration::from_secs(5)) {
                Ok(s) => s,
                Err(e) => { last_err = e; continue; }
            };

            if let Err(e) = stream.set_nodelay(true) { last_err = e; continue; }
            stream.set_read_timeout(Some(Duration::from_secs(10)))?;
            stream.set_write_timeout(Some(Duration::from_secs(10)))?;

            let mut conn = Self {
                reader: BufReader::with_capacity(8192, stream),
                wbuf: Vec::with_capacity(32768),
                rbuf: Vec::with_capacity(4096),
            };

            match conn.startup(config) {
                Ok(()) => {}
                Err(e) => { last_err = e; continue; }
            }

            match conn.prepare_statements() {
                Ok(()) => {}
                Err(e) => { last_err = e; continue; }
            }

            conn.reader.get_mut().set_read_timeout(None)?;
            conn.reader.get_mut().set_write_timeout(None)?;
            return Ok(conn);
        }

        Err(last_err)
    }

    /// Consume this connection and return the raw socket fd.
    ///
    /// The fd is in blocking mode; the caller should set `O_NONBLOCK` if needed.
    /// Prepared statements survive — they are server-side state, not tied to fd flags.
    pub fn into_raw_fd(self) -> RawFd {
        self.reader.into_inner().into_raw_fd()
    }

    /// Connect using hostname (resolves DNS each time — use connect_resolved for perf).
    pub fn connect(config: &DbConfig) -> io::Result<Self> {
        let addr = format!("{}:{}", config.host, config.port);
        let sock_addr = resolve_addr(&addr)?;
        Self::connect_resolved(sock_addr, config)
    }

    /// Send startup message and handle authentication.
    fn startup(&mut self, config: &DbConfig) -> io::Result<()> {
        wire::write_startup(self.reader.get_mut(), &config.user, &config.database)?;

        loop {
            match wire::read_message(&mut self.reader)? {
                wire::BackendMessage::AuthenticationOk => {}
                wire::BackendMessage::AuthenticationMd5Password(salt) => {
                    wire::write_md5_password(
                        self.reader.get_mut(),
                        &config.user,
                        &config.password,
                        &salt,
                    )?;
                }
                wire::BackendMessage::AuthenticationCleartextPassword => {
                    let body_len = 4 + config.password.len() + 1;
                    let mut buf = Vec::with_capacity(1 + body_len);
                    buf.push(b'p');
                    buf.extend_from_slice(&(body_len as i32).to_be_bytes());
                    buf.extend_from_slice(config.password.as_bytes());
                    buf.push(0);
                    self.reader.get_mut().write_all(&buf)?;
                }
                wire::BackendMessage::AuthenticationSASL(sasl_body) => {
                    crate::scram::authenticate_bufreader(
                        &mut self.reader,
                        &config.user,
                        &config.password,
                        &sasl_body,
                    )?;
                }
                wire::BackendMessage::ReadyForQuery => return Ok(()),
                wire::BackendMessage::ErrorResponse(msg) => {
                    return Err(io::Error::new(io::ErrorKind::ConnectionRefused, msg));
                }
                _ => {} // Skip ParameterStatus, BackendKeyData, etc.
            }
        }
    }

    /// Prepare the TechEmpower benchmark statements.
    fn prepare_statements(&mut self) -> io::Result<()> {
        wire::write_parse(
            self.reader.get_mut(),
            STMT_WORLD,
            "SELECT id, randomNumber FROM World WHERE id = $1",
            &[OID_INT4],
        )?;
        wire::write_parse(
            self.reader.get_mut(),
            STMT_FORTUNE,
            "SELECT id, message FROM Fortune",
            &[],
        )?;
        wire::write_parse(
            self.reader.get_mut(),
            STMT_UPDATE,
            "UPDATE World SET randomNumber = $1 WHERE id = $2",
            &[OID_INT4, OID_INT4],
        )?;
        wire::write_parse(
            self.reader.get_mut(),
            STMT_UPDATE_BATCH,
            "UPDATE world SET randomNumber = w.r FROM (SELECT unnest($1::int[]) AS i, unnest($2::int[]) AS r) AS w WHERE world.id = w.i",
            &[OID_INT4_ARRAY, OID_INT4_ARRAY],
        )?;
        wire::write_sync(self.reader.get_mut())?;
        wire::drain_until_ready(&mut self.reader)?;
        Ok(())
    }

    /// Execute a single-row query: SELECT id, randomNumber FROM World WHERE id = $1
    /// Returns (id, randomNumber). Zero allocation in hot path.
    #[inline]
    pub fn query_world(&mut self, id: i32) -> io::Result<(i32, i32)> {
        self.wbuf.clear();
        wire::buf_bind_i32(&mut self.wbuf, STMT_WORLD, &[id]);
        wire::buf_execute(&mut self.wbuf);
        wire::buf_sync(&mut self.wbuf);
        self.reader.get_mut().write_all(&self.wbuf)?;
        wire::read_single_world(&mut self.reader, &mut self.rbuf)
    }

    /// Execute N world queries pipelined. Clears and fills `out`.
    #[inline]
    pub fn query_worlds(&mut self, ids: &[i32], out: &mut Vec<(i32, i32)>) -> io::Result<()> {
        self.wbuf.clear();
        for &id in ids {
            wire::buf_bind_i32(&mut self.wbuf, STMT_WORLD, &[id]);
            wire::buf_execute(&mut self.wbuf);
        }
        wire::buf_sync(&mut self.wbuf);
        self.reader.get_mut().write_all(&self.wbuf)?;
        out.clear();
        wire::read_world_rows(&mut self.reader, &mut self.rbuf, out)
    }

    /// Execute the fortune query. Clears and fills `out`.
    #[inline]
    pub fn query_fortunes(&mut self, out: &mut Vec<(i32, String)>) -> io::Result<()> {
        self.wbuf.clear();
        wire::buf_bind_no_params(&mut self.wbuf, STMT_FORTUNE, &[1, 0]);
        wire::buf_execute(&mut self.wbuf);
        wire::buf_sync(&mut self.wbuf);
        self.reader.get_mut().write_all(&self.wbuf)?;
        out.clear();
        wire::read_fortune_rows(&mut self.reader, &mut self.rbuf, out)
    }

    /// Pipeline N updates: UPDATE World SET randomNumber = $1 WHERE id = $2
    #[inline]
    pub fn update_worlds(&mut self, updates: &[(i32, i32)]) -> io::Result<()> {
        self.wbuf.clear();
        for &(random_number, id) in updates {
            wire::buf_bind_i32(&mut self.wbuf, STMT_UPDATE, &[random_number, id]);
            wire::buf_execute(&mut self.wbuf);
        }
        wire::buf_sync(&mut self.wbuf);
        self.reader.get_mut().write_all(&self.wbuf)?;
        wire::drain_until_ready_buf(&mut self.reader, &mut self.rbuf)
    }

    /// Batch update N worlds using unnest() — single SQL statement.
    #[inline]
    pub fn update_worlds_batch(&mut self, ids: &[i32], random_numbers: &[i32]) -> io::Result<()> {
        self.wbuf.clear();
        wire::buf_bind_i32_arrays(&mut self.wbuf, STMT_UPDATE_BATCH, ids, random_numbers);
        wire::buf_execute(&mut self.wbuf);
        wire::buf_sync(&mut self.wbuf);
        self.reader.get_mut().write_all(&self.wbuf)?;
        wire::drain_until_ready_buf(&mut self.reader, &mut self.rbuf)
    }
}

/// Resolve a "host:port" string to a SocketAddr.
fn resolve_addr(addr: &str) -> io::Result<std::net::SocketAddr> {
    if let Ok(sa) = addr.parse::<std::net::SocketAddr>() {
        return Ok(sa);
    }
    use std::net::ToSocketAddrs;
    addr.to_socket_addrs()?
        .next()
        .ok_or_else(|| io::Error::new(io::ErrorKind::AddrNotAvailable, "DNS resolution failed"))
}
