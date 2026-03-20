//! gnosis-uring: topology-driven HTTP server
//!
//! Four primitives. One binary. Zero GC.
//!
//!   FORK  — fan out (accept connections, resolve files, race codecs)
//!   RACE  — first wins (cache vs mmap vs disk, smallest codec wins)
//!   FOLD  — gather (headers + body, multiple frames)
//!   VENT  — cancel (losers discarded, errors propagated)
//!
//! The topology IS the server. Betty compiles .gg to a static DAG.
//! The executor walks edges and submits io_uring operations.
//! No event loop. No callbacks. No GC. No JIT.
//!
//! On macOS: kqueue + direct syscalls (development).
//! On Linux: io_uring multishot accept + batched submissions (production).
//!
//! Usage:
//!   gnosis-uring                           # serve ./www on :8080
//!   gnosis-uring --port 9090 --root /var/www
//!   gnosis-uring --threads 0              # auto-detect CPU count

mod topology;
mod laminar;
mod http;
mod executor;
mod cache;
mod server;
mod uring;
mod laminar_mux;
mod pgwire;
mod db;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let mut port: u16 = 8080;
    let mut flow_port: u16 = 9082;
    let mut root = String::from("./www");
    let mut threads: usize = 1;
    let mut use_uring = false;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--port" | "-p" => {
                i += 1;
                if i < args.len() { port = args[i].parse().unwrap_or(8080); }
            }
            "--flow-port" => {
                i += 1;
                if i < args.len() { flow_port = args[i].parse().unwrap_or(9082); }
            }
            "--root" | "-r" => {
                i += 1;
                if i < args.len() { root = args[i].clone(); }
            }
            "--threads" | "-t" => {
                i += 1;
                if i < args.len() {
                    threads = args[i].parse().unwrap_or(1);
                    if threads == 0 {
                        threads = std::thread::available_parallelism()
                            .map(|n| n.get())
                            .unwrap_or(4);
                    }
                }
            }
            "--uring" => {
                use_uring = true;
            }
            "--help" | "-h" => {
                eprintln!("gnosis-uring: topology-driven HTTP server");
                eprintln!();
                eprintln!("Usage: gnosis-uring [OPTIONS]");
                eprintln!();
                eprintln!("Options:");
                eprintln!("  -p, --port <PORT>       TCP listen port (default: 8080)");
                eprintln!("  --flow-port <PORT>      UDP Aeon Flow port (default: 9082)");
                eprintln!("  -r, --root <PATH>       Document root (default: ./www)");
                eprintln!("  -t, --threads <N>       Worker threads (0 = auto, default: 1)");
                eprintln!("  --uring                 Use io_uring event loop (Linux only)");
                eprintln!("  -h, --help              Show this help");
                eprintln!();
                eprintln!("Topology:");
                eprintln!("  (accept)-[:FORK]->(conn)");
                eprintln!("  (conn)-[:PROCESS]->(req: ParseHTTP)");
                eprintln!("  (req)-[:FORK]->(cache, mmap, disk)");
                eprintln!("  (cache | mmap | disk)-[:RACE]->(resolved)");
                eprintln!("  (resolved)-[:FORK]->(id, gz, br, df)");
                eprintln!("  (id | gz | br | df)-[:RACE {{ take: 'smallest' }}]->(compressed)");
                eprintln!("  (compressed)-[:PROCESS]->(send)");
                eprintln!();
                eprintln!("Dual protocol:");
                eprintln!("  TCP :{{port}}       HTTP/1.1 for browsers");
                eprintln!("  UDP :{{flow-port}} Aeon Flow for topology clients");
                std::process::exit(0);
            }
            _ => {
                eprintln!("Unknown option: {}", args[i]);
                std::process::exit(1);
            }
        }
        i += 1;
    }

    // ── io_uring path (Linux) ────────────────────────────────────
    #[cfg(target_os = "linux")]
    if use_uring {
        match uring::linux::UringExecutor::new(root.clone(), port, flow_port) {
            Ok(mut uring_exec) => {
                uring_exec.run().expect("io_uring executor failed");
                return;
            }
            Err(e) => {
                eprintln!("WARNING: io_uring unavailable ({}), falling back to blocking I/O", e);
                eprintln!("  Hint: run with --privileged or --security-opt seccomp=unconfined");
            }
        }
    }

    #[cfg(not(target_os = "linux"))]
    if use_uring {
        eprintln!("ERROR: --uring requires Linux (io_uring not available on this platform)");
        eprintln!("Using blocking I/O fallback (development mode)");
    }

    // ── Fallback path (macOS / any OS) ───────────────────────────
    if threads > 1 {
        server::run_threaded(root, port, threads);
    } else {
        let mut executor = executor::Executor::new(root, port);
        server::run(&mut executor);
    }
}
