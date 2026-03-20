//! TCP server — accept loop with per-connection topology execution.
//!
//! Whip-snap concurrency model:
//!   - N threads, each with its own accept loop (SO_REUSEPORT)
//!   - Kernel distributes connections across threads
//!   - Each thread walks the topology synchronously per-connection
//!   - Keep-alive: loop read → execute → respond until close
//!
//! The topology IS the concurrency model:
//!   (listener: Accept)-[:FORK { threads: N }]->(thread: Worker)
//!   (thread: Worker)-[:FORK]->(conn: Connection)
//!   (conn)-[:PROCESS]->(req/res loop)
//!
//! On Linux: will use io_uring for accept + read + write.
//! On macOS: uses blocking I/O with SO_REUSEPORT threads (dev mode).

use std::os::raw::c_int;
use std::net::TcpListener;

use crate::executor::{Executor, ConnContext};

/// Create a TcpListener with SO_REUSEPORT set BEFORE bind.
/// This is required on macOS where SO_REUSEPORT must be set pre-bind.
fn bind_reuseport(port: u16) -> TcpListener {
    unsafe {
        let fd = libc::socket(libc::AF_INET, libc::SOCK_STREAM, 0);
        assert!(fd >= 0, "socket() failed");

        let one: c_int = 1;
        libc::setsockopt(
            fd, libc::SOL_SOCKET, libc::SO_REUSEPORT,
            &one as *const _ as *const libc::c_void,
            std::mem::size_of::<c_int>() as u32,
        );
        libc::setsockopt(
            fd, libc::SOL_SOCKET, libc::SO_REUSEADDR,
            &one as *const _ as *const libc::c_void,
            std::mem::size_of::<c_int>() as u32,
        );

        let mut addr: libc::sockaddr_in = std::mem::zeroed();
        addr.sin_family = libc::AF_INET as libc::sa_family_t;
        addr.sin_port = port.to_be();
        addr.sin_addr.s_addr = libc::INADDR_ANY;

        let ret = libc::bind(
            fd,
            &addr as *const _ as *const libc::sockaddr,
            std::mem::size_of::<libc::sockaddr_in>() as u32,
        );
        assert!(ret == 0, "bind() failed: {}", std::io::Error::last_os_error());

        libc::listen(fd, 1024);

        use std::os::fd::FromRawFd;
        TcpListener::from_raw_fd(fd)
    }
}

/// Run the server on the given port (single thread).
pub fn run(executor: &mut Executor) {
    let listener = bind_reuseport(executor.port);

    eprintln!("Using built-in TechEmpower route table (12 routes)");
    eprintln!("gnosis-uring listening on 0.0.0.0:{}", executor.port);
    eprintln!("  GET /plaintext       -- TechEmpower plaintext");
    eprintln!("  GET /json            -- TechEmpower JSON");
    eprintln!("  GET /db              -- TechEmpower single query");
    eprintln!("  GET /queries         -- TechEmpower multiple queries");
    eprintln!("  GET /updates         -- TechEmpower updates");
    eprintln!("  GET /fortunes        -- TechEmpower fortunes");
    eprintln!("  GET /cached-queries  -- TechEmpower cached queries");
    eprintln!("  GET /*               -- static files from {}", executor.root);

    // ── FORK: accept connections ─────────────────────────────────
    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                let socket_fd = {
                    use std::os::fd::AsRawFd;
                    stream.as_raw_fd()
                };

                // TCP_NODELAY — reduce latency
                unsafe {
                    let one: c_int = 1;
                    libc::setsockopt(
                        socket_fd,
                        libc::IPPROTO_TCP,
                        libc::TCP_NODELAY,
                        &one as *const _ as *const libc::c_void,
                        std::mem::size_of::<c_int>() as u32,
                    );
                }

                // Handle the connection with keep-alive loop
                handle_connection(executor, socket_fd);

                // Prevent Rust's TcpStream destructor from double-closing
                std::mem::forget(stream);
            }
            Err(_) => {}
        }
    }
}

/// Handle a connection: keep-alive loop reading requests.
///
/// This is the per-connection topology walk:
///   loop {
///     read → ParseHTTP → FORK(cache, mmap, disk) → RACE → respond
///     if !keep_alive → break
///   }
fn handle_connection(executor: &mut Executor, socket_fd: c_int) {
    let mut ctx = ConnContext::new(socket_fd);

    loop {
        // Read request
        ctx.read_len = 0;
        let n = unsafe {
            libc::read(
                socket_fd,
                ctx.read_buf.as_mut_ptr() as *mut libc::c_void,
                ctx.read_buf.len(),
            )
        };

        if n <= 0 {
            break; // Connection closed or error → VENT
        }
        ctx.read_len = n as usize;

        // Execute topology for this request
        let keep_alive = executor.handle_request(&mut ctx);

        if !keep_alive {
            break;
        }
    }

    // Close socket
    unsafe { libc::close(socket_fd); }
}

/// Whip-snap: N threads, each with its own accept loop.
///
/// The kernel's SO_REUSEPORT distributes connections across threads.
/// This is the Worthington Whip pattern: each thread is a "whip"
/// that independently processes its share of connections.
///
/// No shared state between threads (each has its own Executor + cache).
/// No mutex contention. No cross-thread signaling.
pub fn run_threaded(root: String, port: u16, threads: usize) {
    eprintln!("gnosis-uring: {} whip-snap threads on :{}", threads, port);

    let mut handles = Vec::new();

    for i in 0..threads {
        let root = root.clone();
        let handle = std::thread::spawn(move || {
            let mut executor = Executor::new(root, port);
            let listener = bind_reuseport(port);

            if i == 0 {
                eprintln!("gnosis-uring listening on 0.0.0.0:{}", port);
            }

            for stream in listener.incoming() {
                if let Ok(stream) = stream {
                    let socket_fd = {
                        use std::os::fd::AsRawFd;
                        stream.as_raw_fd()
                    };
                    unsafe {
                        let one: c_int = 1;
                        libc::setsockopt(
                            socket_fd,
                            libc::IPPROTO_TCP,
                            libc::TCP_NODELAY,
                            &one as *const _ as *const libc::c_void,
                            std::mem::size_of::<c_int>() as u32,
                        );
                    }
                    handle_connection(&mut executor, socket_fd);
                    std::mem::forget(stream);
                }
            }
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }
}
