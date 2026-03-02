//! Zero-copy HTTP request representation.
//!
//! The request borrows directly from the receive buffer,
//! avoiding any allocation for header access.

/// A zero-copy view into a received HTTP request.
pub struct Request<'buf> {
    pub method: &'buf [u8],
    pub path: &'buf [u8],
    pub version: u8,
}

impl<'buf> Request<'buf> {
    /// Check if this is a GET request.
    #[inline]
    pub fn is_get(&self) -> bool {
        self.method == b"GET"
    }

    /// Get the path as a string slice.
    #[inline]
    pub fn path_str(&self) -> &str {
        // Safety: HTTP paths are ASCII
        unsafe { std::str::from_utf8_unchecked(self.path) }
    }
}
