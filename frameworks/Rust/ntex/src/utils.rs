#![allow(dead_code)]
use std::cmp;

use atoi::FromRadix10;

pub const SIZE: usize = 27;

pub fn get_query_param(query: Option<&str>) -> u16 {
    let query = query.unwrap_or("");
    let q = if let Some(pos) = query.find('q') {
        u16::from_radix_10(query.split_at(pos + 2).1.as_ref()).0
    } else {
        1
    };
    cmp::min(500, cmp::max(1, q))
}
