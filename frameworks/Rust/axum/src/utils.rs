use std::{env, fmt::Debug, str::FromStr};

use axum::{
    body::{Bytes, Full},
    http::{header, HeaderValue, StatusCode},
    response::{IntoResponse, Response},
};
use rand::{rngs::SmallRng, Rng};
use serde::Deserialize;

pub fn get_environment_variable<T: FromStr>(key: &str) -> T
where
    <T as FromStr>::Err: Debug,
{
    env::var(key)
        .unwrap_or_else(|_| panic!("{key} environment variable was not set"))
        .parse::<T>()
        .unwrap_or_else(|_| panic!("could not parse {key}"))
}

#[derive(Debug, Deserialize)]
pub struct Params {
    queries: Option<String>,
}

#[allow(dead_code)]
pub fn random_number(rng: &mut SmallRng) -> i32 {
    (rng.gen::<u32>() % 10_000 + 1) as i32
}

#[allow(dead_code)]
pub fn parse_params(params: Params) -> i32 {
    params
        .queries
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

#[derive(Clone, Copy, Debug)]
pub struct Utf8Html<T>(pub T);

impl<T> IntoResponse for Utf8Html<T>
where
    T: Into<Full<Bytes>>,
{
    fn into_response(self) -> Response {
        let mut res = (StatusCode::OK, self.0.into()).into_response();
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
