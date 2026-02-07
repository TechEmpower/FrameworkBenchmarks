use rand::{Rng, SeedableRng};
use rand::rngs::SmallRng;
use std::cell::RefCell;

pub const MAX_WORLD_ID: i32 = 10_000;
const MIN_QUERY_COUNT: usize = 1;
const MAX_QUERY_COUNT: usize = 500;

thread_local! {
    static RNG: RefCell<SmallRng> = RefCell::new(SmallRng::from_entropy());
}

pub fn random_id() -> i32 {
    RNG.with(|rng| rng.borrow_mut().gen_range(1..=MAX_WORLD_ID))
}

pub fn random_ids(count: usize) -> Vec<i32> {
    let mut ids = Vec::with_capacity(count);
    for _ in 0..count {
        ids.push(random_id());
    }
    ids
}

pub fn clamp_query_count(count: Option<u16>) -> usize {
    let mut value = count.unwrap_or(1) as usize;
    if value < MIN_QUERY_COUNT {
        value = MIN_QUERY_COUNT;
    } else if value > MAX_QUERY_COUNT {
        value = MAX_QUERY_COUNT;
    }
    value
}

pub fn parse_query_count(value: Option<&str>) -> usize {
    let parsed = value.and_then(|raw| raw.parse::<u16>().ok());
    clamp_query_count(parsed)
}

pub fn escape_html(input: &str) -> String {
    let mut output = String::with_capacity(input.len());
    for ch in input.chars() {
        match ch {
            '&' => output.push_str("&amp;"),
            '<' => output.push_str("&lt;"),
            '>' => output.push_str("&gt;"),
            '"' => output.push_str("&quot;"),
            '\'' => output.push_str("&#x27;"),
            _ => output.push(ch),
        }
    }
    output
}
