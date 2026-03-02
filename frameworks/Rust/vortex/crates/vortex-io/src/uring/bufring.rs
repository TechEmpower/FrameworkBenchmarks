//! Provided buffer rings for zero-allocation recv.
//!
//! The kernel selects a buffer from the ring for recv operations that
//! specify `IOSQE_BUFFER_SELECT`.  After processing, the app returns the
//! buffer via `return_buf()`.

use std::io;
use std::sync::atomic::{AtomicU16, Ordering};

use io_uring::types::BufRingEntry;

/// A ring-mapped provided buffer pool.
///
/// Each worker thread maintains one of these. Buffers are used by
/// recv operations without any per-request allocation.
pub struct ProvidedBufRing {
    /// Base pointer to the ring descriptor array (array of BufRingEntry).
    ring_ptr: *mut BufRingEntry,
    /// Pointer to the actual buffer data.
    bufs_ptr: *mut u8,
    /// Number of buffers (must be power of 2).
    buf_count: u16,
    /// Size of each buffer in bytes.
    buf_size: u32,
    /// Buffer group ID.
    bgid: u16,
    /// Mask for wrapping tail index (buf_count - 1).
    mask: u16,
    /// Layout used for ring descriptor allocation (for dealloc).
    ring_layout: std::alloc::Layout,
    /// Layout used for buffer data allocation (for dealloc).
    bufs_layout: std::alloc::Layout,
}

impl ProvidedBufRing {
    /// Default buffer count per ring (must be power of 2).
    pub const DEFAULT_BUF_COUNT: u16 = 1024;
    /// Default buffer size (sufficient for most HTTP/1.1 requests).
    pub const DEFAULT_BUF_SIZE: u32 = 4096;

    /// Create a new provided buffer ring and register it with io_uring.
    ///
    /// `buf_count` must be a power of 2.
    pub fn new(
        submitter: &io_uring::Submitter<'_>,
        bgid: u16,
        buf_count: u16,
        buf_size: u32,
    ) -> io::Result<Self> {
        assert!(buf_count.is_power_of_two(), "buf_count must be power of 2");

        let mask = buf_count - 1;

        // Allocate the ring descriptor array (page-aligned)
        let ring_size = buf_count as usize * std::mem::size_of::<BufRingEntry>();
        let ring_layout = std::alloc::Layout::from_size_align(ring_size, 4096)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
        let ring_ptr = unsafe { std::alloc::alloc_zeroed(ring_layout) } as *mut BufRingEntry;
        if ring_ptr.is_null() {
            return Err(io::Error::new(
                io::ErrorKind::OutOfMemory,
                "failed to allocate buffer ring descriptors",
            ));
        }

        // Allocate the actual buffer data (page-aligned)
        let total_buf_mem = buf_count as usize * buf_size as usize;
        let bufs_layout = std::alloc::Layout::from_size_align(total_buf_mem, 4096)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
        let bufs_ptr = unsafe { std::alloc::alloc_zeroed(bufs_layout) };
        if bufs_ptr.is_null() {
            unsafe { std::alloc::dealloc(ring_ptr as *mut u8, ring_layout); }
            return Err(io::Error::new(
                io::ErrorKind::OutOfMemory,
                "failed to allocate buffer ring memory",
            ));
        }

        // Initialize all ring entries to point to their buffers
        for i in 0..buf_count {
            let entry = unsafe { &mut *ring_ptr.add(i as usize) };
            let buf_addr = unsafe { bufs_ptr.add(i as usize * buf_size as usize) };
            entry.set_addr(buf_addr as u64);
            entry.set_len(buf_size);
            entry.set_bid(i);
        }

        // Set initial tail = buf_count (all buffers available)
        let tail_ptr = unsafe { BufRingEntry::tail(ring_ptr) } as *const AtomicU16;
        unsafe { (*tail_ptr).store(buf_count, Ordering::Release); }

        // Register with io_uring
        unsafe {
            submitter.register_buf_ring_with_flags(ring_ptr as u64, buf_count, bgid, 0)?;
        }

        Ok(Self {
            ring_ptr,
            bufs_ptr,
            buf_count,
            buf_size,
            bgid,
            mask,
            ring_layout,
            bufs_layout,
        })
    }

    /// Get the buffer group ID.
    #[inline]
    pub fn bgid(&self) -> u16 {
        self.bgid
    }

    /// Get a slice to a specific buffer by its buffer ID and data length.
    #[inline]
    pub fn get_buf(&self, buf_id: u16, len: usize) -> &[u8] {
        debug_assert!(buf_id < self.buf_count);
        debug_assert!(len <= self.buf_size as usize);
        unsafe {
            let ptr = self.bufs_ptr.add(buf_id as usize * self.buf_size as usize);
            std::slice::from_raw_parts(ptr, len)
        }
    }

    /// Return a buffer to the ring (make it available for kernel reuse).
    #[inline]
    pub fn return_buf(&self, buf_id: u16) {
        let tail_ptr = unsafe { BufRingEntry::tail(self.ring_ptr) } as *const AtomicU16;
        let tail = unsafe { (*tail_ptr).load(Ordering::Acquire) };
        let idx = tail & self.mask;

        let entry = unsafe { &mut *self.ring_ptr.add(idx as usize) };
        let buf_addr = unsafe { self.bufs_ptr.add(buf_id as usize * self.buf_size as usize) };
        entry.set_addr(buf_addr as u64);
        entry.set_len(self.buf_size);
        entry.set_bid(buf_id);

        unsafe { (*tail_ptr).store(tail.wrapping_add(1), Ordering::Release); }
    }

    /// Get the size of each buffer.
    #[inline]
    pub fn buf_size(&self) -> u32 {
        self.buf_size
    }
}

impl Drop for ProvidedBufRing {
    fn drop(&mut self) {
        unsafe {
            std::alloc::dealloc(self.ring_ptr as *mut u8, self.ring_layout);
            std::alloc::dealloc(self.bufs_ptr, self.bufs_layout);
        }
    }
}

// Safety: ProvidedBufRing is only accessed from a single worker thread.
unsafe impl Send for ProvidedBufRing {}
