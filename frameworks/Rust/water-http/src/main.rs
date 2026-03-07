pub mod server;
pub mod models;
pub mod db;
pub mod date;
pub mod buf;
// pub mod chop;

use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

 fn main() {
    server::run_server();
}

