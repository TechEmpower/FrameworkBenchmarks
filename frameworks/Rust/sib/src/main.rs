use bytes::Bytes;
use sib::network::http::{
    message::Status,
    server::{H1ServiceFactory, HttpService},
    session::Session,
};
use std::fs;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

struct H1Server<T>(pub T);

struct H1Service;

impl HttpService for H1Service {
    fn call(&mut self, session: &mut Session) -> std::io::Result<()> {
        session
            .status_code(Status::Ok)
            .header("Content-Type", "text/plain")?
            .header("Content-Length", "13")?
            .body(&Bytes::from_static(b"Hello, World!"))
            .eom();
        Ok(())
    }
}

impl H1ServiceFactory for H1Server<H1Service> {
    type Service = H1Service;

    fn service(&self, _id: usize) -> H1Service {
        H1Service
    }
}

fn main() {
    // Print number of CPU cores
    let cpus = num_cpus::get();
    println!("CPU cores: {}", cpus);

    // Print total RAM in MB
    if let Ok(meminfo) = fs::read_to_string("/proc/meminfo") {
        for line in meminfo.lines() {
            if line.starts_with("MemTotal:") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 2 {
                    if let Ok(kb) = parts[1].parse::<u64>() {
                        let mb = kb / 1024;
                        println!("Total RAM: {} MB", mb);
                    }
                }
                break;
            }
        }
    }

    // Pick a port and start the server
    let addr = "0.0.0.0:8080";
    H1Server(H1Service)
        .start(addr)
        .expect("h1 server failed to start")
        .join()
        .expect("h1 failed on joining thread");
}
