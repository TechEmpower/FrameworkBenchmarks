mod application;
mod db;
mod routes;

use application::application;

fn main() {
    trillium_async_std::run(application())
}
