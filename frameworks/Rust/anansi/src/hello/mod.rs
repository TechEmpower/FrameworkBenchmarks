pub mod records;
pub mod migrations;

pub const APP_NAME: &'static str = "hello";
pub mod world;

#[cfg(feature = "raw")]
pub mod middleware;
