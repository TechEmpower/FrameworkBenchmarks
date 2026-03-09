pub mod server;
pub mod models;
pub mod db;
pub mod date;
pub mod buf;

use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

 fn main() {

     #[cfg(feature = "mini")]
     {
         mini::run();
     }
     #[cfg(not(feature = "mini"))]
     server::run_server();
}

