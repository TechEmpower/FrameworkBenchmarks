use bytes::Bytes;
use sib::network::http::{
    h1::{H1Service, H1ServiceFactory},
    message::Status,
    session::Session,
};
use std::{
    fs,
    io::{Read, Write},
};

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

struct H1Server<T>(pub T);

struct HService;

impl H1Service for HService {
    fn call<S: Read + Write>(&mut self, session: &mut Session<S>) -> std::io::Result<()> {
        session
            .status_code(Status::Ok)
            .header("Content-Type", "text/plain")?
            .header("Content-Length", "13")?
            .body(&Bytes::from_static(b"Hello, World!"))
            .eom();
        Ok(())
    }
}

impl H1ServiceFactory for H1Server<HService> {
    type Service = HService;

    fn service(&self, _id: usize) -> HService {
        HService
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
    H1Server(HService)
        .start(addr, cpus)
        .expect("h1 server failed to start")
        .join()
        .expect("h1 failed on joining thread");
}
