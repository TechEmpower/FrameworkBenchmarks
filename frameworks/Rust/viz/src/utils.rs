use std::ops::Range;

use atoi::FromRadix10;

#[allow(dead_code)]
pub const RANGE: Range<i32> = 1..10_001;

#[allow(dead_code)]
pub fn get_query_param(query: Option<&str>) -> u16 {
    query
        .and_then(|s| {
            s.find('q')
                .map(|p| u16::from_radix_10(s.split_at(p + 2).1.as_ref()).0)
        })
        .map(|n| n.clamp(1, 500))
        .unwrap_or(1)
}
