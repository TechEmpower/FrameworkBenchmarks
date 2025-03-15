use crate::*;

#[inline]
pub fn generate_rfc1123_timestamp() -> String {
    let now: DateTime<Utc> = SystemTime::now().into();
    now.format("%a, %d %b %Y %H:%M:%S GMT").to_string()
}

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
