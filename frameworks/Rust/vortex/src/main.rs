//! TechEmpower benchmark entry point for Vortex.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn main() {
    eprintln!("=== Vortex TechEmpower Benchmark Server ===");

    if let Err(e) = vortex_server::Server::builder()
        .addr("0.0.0.0")
        .port(8080)
        .workers(0) // auto-detect (override with VORTEX_WORKERS env)
        .backlog(4096)
        .sqpoll(false) // override with VORTEX_SQPOLL=1 env
        .run()
    {
        eprintln!("[vortex] Fatal error: {}", e);
        std::process::exit(1);
    }
}
