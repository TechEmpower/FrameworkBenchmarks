use axum::http::{header, HeaderValue, StatusCode};
use axum::response::IntoResponse;
use axum::{
    body::{Bytes, Full},
    response::Response,
};
use rand::rngs::SmallRng;
use rand::Rng;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct Params {
    queries: Option<String>,
}

pub fn random_number(rng: &mut SmallRng) -> i32 {
    (rng.gen::<u32>() % 10_000 + 1) as i32
}

pub fn parse_params(params: Params) -> usize {
    num::clamp(
        params
            .queries
            .map(|queries| queries.parse::<i32>().unwrap_or(1))
            .unwrap_or(0),
        1,
        500,
    ) as usize
}

/// Utility function for mapping any error into a `500 Internal Server Error`
/// response.
pub fn internal_error<E>(err: E) -> (StatusCode, String)
where
    E: std::error::Error,
{
    (StatusCode::INTERNAL_SERVER_ERROR, err.to_string())
}

#[derive(Clone, Copy, Debug)]
pub struct Utf8Html<T>(pub T);

impl<T> IntoResponse for Utf8Html<T>
where
    T: Into<Full<Bytes>>,
{
    fn into_response(self) -> Response {
        let mut res = self.0.into().into_response();
        res.headers_mut().insert(
            header::CONTENT_TYPE,
            HeaderValue::from_static("text/html; charset=utf-8"),
        );
        res
    }
}

impl<T> From<T> for Utf8Html<T> {
    fn from(inner: T) -> Self {
        Self(inner)
    }
}
