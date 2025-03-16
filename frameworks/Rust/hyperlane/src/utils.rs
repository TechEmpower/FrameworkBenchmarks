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
pub fn get_random_id() -> Queries {
    let mut rand: WyRand = WyRand::new();
    let random_id: Queries = rand.generate::<Queries>() % (RANDOM_MAX as Queries + 1);
    random_id
}
