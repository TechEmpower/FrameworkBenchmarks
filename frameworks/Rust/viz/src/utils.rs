use std::{cmp, ops::Range};

use atoi::FromRadix10;
use viz::header::HeaderValue;

#[allow(dead_code)]
pub const RANGE: Range<i32> = 1..10_001;

pub const HDR_SERVER: HeaderValue = HeaderValue::from_static("VIZ");

#[allow(dead_code)]
pub fn get_query_param(query: Option<&str>) -> u16 {
    let query = query.unwrap_or("");
    let q = if let Some(pos) = query.find('q') {
        u16::from_radix_10(query.split_at(pos + 2).1.as_ref()).0
    } else {
        1
    };
    cmp::min(500, cmp::max(1, q))
}
