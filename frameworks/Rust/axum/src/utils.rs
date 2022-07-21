use axum::body::{Bytes, Full};
use axum::http::{header, HeaderValue, StatusCode};
use axum::response::{IntoResponse, Response};
use rand::rngs::SmallRng;
use rand::Rng;
use serde::Deserialize;

use std::env;
use std::fmt::Debug;
use std::str::FromStr;

pub fn get_environment_variable<T: FromStr>(key: &str) -> T
where
    <T as FromStr>::Err: Debug,
{
    T::from_str(
        &*env::var(key).expect(&*format!("{} environment variable was not set", key)),
    )
    .expect(&*format!("could not parse {}", key))
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
