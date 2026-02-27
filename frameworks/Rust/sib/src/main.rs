// TechEmpower benchmark tests for Sib with the `net-h1-server` feature enabled

use sib::network::http::{
    server::{H1Config, HFactory},
    session::{HService, Session},
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

struct Server;

impl HService for Server {
    fn call<S: Session>(&mut self, session: &mut S) -> std::io::Result<()> {
        use core::fmt::Write;
        let time = sib::network::http::date::current_date_str();
        let mut res: heapless::String<256> = heapless::String::new();
        if session.req_path_bytes() == b"/json" {
            // Respond with JSON
            let json = serde_json::to_vec(&JsonMessage::default())?;
            write!(
                res,
                "HTTP/1.1 200 OK\r\n\
                Server: sib\r\n\
                Date: {}\r\n\
                Content-Type: application/json\r\n\
                Content-Length: {}\r\n\
                \r\n\
                    {}",
                time,
                &json.len().to_string(),
                String::from_utf8_lossy(&json)
            )
            .unwrap();
            session.write_all_eom(res.as_bytes())
        } else {
            write!(
                res,
                "HTTP/1.1 200 OK\r\n\
             Server: sib\r\n\
             Date: {}\r\n\
             Content-Type: text/plain\r\n\
             Content-Length: 13\r\n\
             \r\n\
             Hello, World!",
                time
            )
            .unwrap();
            session.write_all_eom(res.as_bytes())
        }
    }
}

impl HFactory for Server {
    type Service = Server;

    fn service(&self, _id: usize) -> Server {
        Server
    }
}

fn main() {
    let stack_size = 2 * 1024; // 2 KB stack
    let cpus = num_cpus::get();

    sib::init_global_poller(cpus, stack_size);

    // Pick a port and start the server
    let addr = "0.0.0.0:8080";
    let mut threads = Vec::with_capacity(cpus);

    for _ in 0..cpus {
        let handle = std::thread::spawn(move || {
            let id = std::thread::current().id();
            tracing::info!("Listening {addr} on thread: {id:?}");
            Server
                .start_h1(
                    addr,
                    H1Config {
                        io_timeout: std::time::Duration::from_secs(15),
                        stack_size,
                        ..Default::default()
                    },
                )
                .unwrap_or_else(|_| panic!("H1 server failed to start for thread {id:?}"))
                .join()
                .unwrap_or_else(|_| panic!("H1 server failed to join thread {id:?}"));
        });
        threads.push(handle);
    }

    // Wait for all threads to complete
    for handle in threads {
        handle.join().expect("Thread panicked");
    }
}
