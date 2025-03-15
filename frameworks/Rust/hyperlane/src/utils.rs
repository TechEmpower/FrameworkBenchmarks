use crate::*;

#[inline]
pub fn generate_rfc1123_timestamp() -> String {
    let now: DateTime<Utc> = SystemTime::now().into();
    now.format("%a, %d %b %Y %H:%M:%S GMT").to_string()
}
#[inline]
pub fn replace_html_entities(input: &str) -> String {
    input
        .replace("&lt;", "<")
        .replace("&gt;", ">")
        .replace("&quot;", "\"")
}
