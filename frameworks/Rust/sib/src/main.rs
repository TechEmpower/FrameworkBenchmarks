use bytes::Bytes;
use sib::network::http::{
    h1::{H1Service, H1ServiceFactory},
    util::Status,
    session::Session,
};
use std::{
    fs,
    io::{Read, Write},
};

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[derive(serde::Serialize)]
struct JsonMessage<'a> {
    message: &'a str,
}

impl Default for JsonMessage<'_> {
    fn default() -> Self {
        JsonMessage {
            message: "Hello, World!",
        }
    }
}

struct H1Server<T>(pub T);

struct HService;

impl H1Service for HService {
    fn call<S: Read + Write>(&mut self, session: &mut Session<S>) -> std::io::Result<()> {
        if session.req_path() == Some("/json") {
            // Respond with JSON
            let json = serde_json::to_vec(&JsonMessage::default())?;
            session
                .status_code(Status::Ok)
                .header_str("Content-Type", "application/json")?
                .header_str("Content-Length", &json.len().to_string())?
                .body(&Bytes::from(json))
                .eom();
            return Ok(());
        }
        session
            .status_code(Status::Ok)
            .header_str("Content-Type", "text/plain")?
            .header_str("Content-Length", "13")?
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
    println!("CPU cores: {cpus}");

    // Print total RAM in MB
    if let Ok(meminfo) = fs::read_to_string("/proc/meminfo") {
        for line in meminfo.lines() {
            if line.starts_with("MemTotal:") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 2 {
                    if let Ok(kb) = parts[1].parse::<u64>() {
                        let mb = kb / 1024;
                        println!("Total RAM: {mb} MB");
                    }
                }
                break;
            }
        }
    }

    // Pick a port and start the server
    let addr = "0.0.0.0:8080";
    let mut threads = Vec::with_capacity(cpus);

    for _ in 0..cpus {
        let handle = std::thread::spawn(move || {
            let id = std::thread::current().id();
            println!("Listening {addr} on thread: {id:?}");
            H1Server(HService)
                .start(addr, cpus, 0)
                .unwrap_or_else(|_| panic!("h1 server failed to start for thread {id:?}"))
                .join()
                .unwrap_or_else(|_| panic!("h1 server failed to joining thread {id:?}"));
        });
        threads.push(handle);
    }

    // Wait for all threads to complete (they won’t unless crashed)
    for handle in threads {
        handle.join().expect("Thread panicked");
    }
}
