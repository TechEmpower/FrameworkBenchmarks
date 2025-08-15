use super::*;

pub fn get_thread_count() -> usize {
    num_cpus::get().max(1)
}

pub fn escape_html(input: &str) -> String {
    let mut result: String = String::new();
    for ch in input.chars() {
        match ch {
            '<' => result.push_str("&lt;"),
            '>' => result.push_str("&gt;"),
            '&' => result.push_str("&amp;"),
            '"' => result.push_str("&quot;"),
            '\'' => result.push_str("&#39;"),
            _ => result.push(ch),
        }
    }
    result
}

pub fn get_random_id() -> Queries {
    let mut rng: SmallRng = SmallRng::from_rng(&mut rng());
    let random_id: u32 = rng.random_range(1..RANDOM_MAX_ADD_ONE);
    random_id as Queries
}
