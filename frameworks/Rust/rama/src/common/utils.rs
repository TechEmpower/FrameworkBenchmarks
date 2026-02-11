use rama::http::StatusCode;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct Params {
    q: Option<String>,
}

#[allow(dead_code)]
#[inline(always)]
pub fn parse_params(params: Params) -> usize {
    params
        .q
        .and_then(|q| q.parse().ok())
        .unwrap_or(1)
        .clamp(1, 500)
}

/// Utility function for mapping any error into a `500 Internal Server Error`
/// response.
#[allow(dead_code)]
pub fn internal_error<E>(err: E) -> (StatusCode, String)
where
    E: std::error::Error,
{
    (StatusCode::INTERNAL_SERVER_ERROR, err.to_string())
}
