//! vortex-json: Hand-optimized JSON serialization for Vortex.
//!
//! Avoids serde overhead by providing direct-to-buffer serialization
//! for known response types.

/// Write a JSON "World" object: {"id":N,"randomNumber":N}
///
/// Returns bytes written.
#[inline]
pub fn write_world(buf: &mut [u8], id: i32, random_number: i32) -> usize {
    let mut offset = 0;

    buf[offset..offset + 6].copy_from_slice(b"{\"id\":");
    offset += 6;

    offset += write_i32(&mut buf[offset..], id);

    buf[offset..offset + 16].copy_from_slice(b",\"randomNumber\":");
    offset += 16;

    offset += write_i32(&mut buf[offset..], random_number);

    buf[offset] = b'}';
    offset + 1
}

/// Write a JSON array of World objects.
#[inline]
pub fn write_worlds(buf: &mut [u8], worlds: &[(i32, i32)]) -> usize {
    let mut offset = 0;
    buf[offset] = b'[';
    offset += 1;

    for (i, (id, rn)) in worlds.iter().enumerate() {
        if i > 0 {
            buf[offset] = b',';
            offset += 1;
        }
        offset += write_world(&mut buf[offset..], *id, *rn);
    }

    buf[offset] = b']';
    offset + 1
}

/// Write an i32 as decimal ASCII. Returns bytes written.
#[inline]
fn write_i32(buf: &mut [u8], val: i32) -> usize {
    let mut n = itoa::Buffer::new();
    let s = n.format(val);
    let bytes = s.as_bytes();
    buf[..bytes.len()].copy_from_slice(bytes);
    bytes.len()
}
