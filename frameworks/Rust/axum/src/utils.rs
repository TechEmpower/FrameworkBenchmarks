use axum::body::{Bytes, Full};
use axum::http::{header, HeaderValue, StatusCode};
use axum_core::response::IntoResponse;
use axum_core::response::Response;
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
        &*env::var(key)
            .ok()
            .expect(&*format!("{} environment variable was not set", key)),
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
    let mut q = 0;

    if params.queries.is_some() {
        let queries = params.queries.ok_or("could not get value").unwrap();

        let queries_as_int = queries.parse::<i32>();

        match queries_as_int {
            Ok(_ok) => q = queries_as_int.unwrap(),
            Err(_e) => q = 1,
        }
    }

    let q = if q == 0 {
        1
    } else if q > 500 {
        500
    } else {
        q
    };

    q
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
