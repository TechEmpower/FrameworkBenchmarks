pub mod urls;
#[cfg(not(feature = "raw"))]
pub mod records;
pub mod migrations;

pub const APP_NAME: &'static str = "hello";
pub mod world;
