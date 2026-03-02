//! vortex-template: HTML template rendering for Vortex.
//!
//! Hand-optimized Fortunes template with HTML entity escaping.

/// Escape HTML entities in a string, writing to the output buffer.
///
/// Escapes: & < > " '
#[inline]
pub fn escape_html(input: &[u8], output: &mut Vec<u8>) {
    for &byte in input {
        match byte {
            b'&' => output.extend_from_slice(b"&amp;"),
            b'<' => output.extend_from_slice(b"&lt;"),
            b'>' => output.extend_from_slice(b"&gt;"),
            b'"' => output.extend_from_slice(b"&quot;"),
            b'\'' => output.extend_from_slice(b"&#x27;"),
            _ => output.push(byte),
        }
    }
}

/// Render the Fortunes HTML template into the provided output buffer.
///
/// Takes raw DB fortunes, adds the extra fortune, sorts, and renders.
/// Clears output before writing.
pub fn render_fortunes(db_fortunes: &[(i32, String)], output: &mut Vec<u8>) {
    // Build full list with the extra fortune
    let mut fortunes: Vec<(i32, &str)> = db_fortunes
        .iter()
        .map(|(id, msg)| (*id, msg.as_str()))
        .collect();
    fortunes.push((0, "Additional fortune added at request time."));

    // Sort by message (TechEmpower requirement)
    fortunes.sort_by(|a, b| a.1.cmp(b.1));

    // Render HTML
    output.clear();
    output.extend_from_slice(
        b"<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>",
    );

    let mut id_buf = itoa::Buffer::new();
    for &(id, message) in &fortunes {
        output.extend_from_slice(b"<tr><td>");
        output.extend_from_slice(id_buf.format(id).as_bytes());
        output.extend_from_slice(b"</td><td>");
        escape_html(message.as_bytes(), output);
        output.extend_from_slice(b"</td></tr>");
    }

    output.extend_from_slice(b"</table></body></html>");
}
