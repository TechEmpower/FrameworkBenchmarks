use super::*;

pub static REQUEST_CONFIG: Lazy<RequestConfigData> =
    Lazy::new(|| block_on(async { init_request_config().await.get_data().await }));
