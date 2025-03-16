use crate::*;

#[inline]
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

#[inline]
pub fn get_random_id() -> i32 {
    let mut rand: WyRand = WyRand::new();
    let random_id: i32 = (rand.generate::<u32>() % 10_000 + 1) as i32;
    random_id
}
