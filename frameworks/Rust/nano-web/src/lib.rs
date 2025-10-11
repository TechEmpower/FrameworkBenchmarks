pub mod cli;
pub mod compression;
pub mod mime_types;
pub mod path;
pub mod routes;
pub mod server;
pub mod template;

use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

pub fn init_logging(level: &str, format: &str) {
    let env_filter = match level {
        "debug" => "debug",
        "warn" => "warn",
        "error" => "error",
        _ => "info",
    };

    let subscriber =
        tracing_subscriber::registry().with(tracing_subscriber::EnvFilter::new(env_filter));

    if format == "json" {
        subscriber
            .with(tracing_subscriber::fmt::layer().json())
            .init();
    } else {
        subscriber
            .with(tracing_subscriber::fmt::layer().pretty())
            .init();
    }
}
