//! Core io_uring ring management.
//!
//! Each worker thread gets its own Ring with:
//! - IORING_SETUP_SINGLE_ISSUER (kernel optimization for single-thread rings)
//! - IORING_SETUP_COOP_TASKRUN (cooperative completion delivery)
//! - IORING_SETUP_DEFER_TASKRUN (defer completions to submitting thread)

use io_uring::IoUring;
use std::io;

/// Configuration for an io_uring ring instance.
pub struct RingConfig {
    /// Submission queue depth (must be power of 2).
    pub sq_entries: u32,
    /// Whether to enable SQPOLL mode (kernel thread polls SQ).
    pub sqpoll: bool,
    /// SQPOLL idle timeout in milliseconds.
    pub sqpoll_idle_ms: u32,
}

impl Default for RingConfig {
    fn default() -> Self {
        Self {
            sq_entries: 4096,
            sqpoll: false,
            sqpoll_idle_ms: 1000,
        }
    }
}

/// Wrapper around io_uring::IoUring with Vortex-specific configuration.
pub struct Ring {
    inner: IoUring,
}

impl Ring {
    /// Create a new io_uring ring with the given configuration.
    ///
    /// Uses cascading fallbacks for maximum kernel compatibility:
    /// 1. SQPOLL (if requested, kernel 5.4+ with CAP_SYS_ADMIN)
    /// 2. DEFER_TASKRUN + SINGLE_ISSUER + COOP_TASKRUN (kernel 6.1+)
    /// 3. COOP_TASKRUN (kernel 5.19+)
    /// 4. Basic io_uring (kernel 5.4+)
    pub fn new(config: &RingConfig) -> io::Result<Self> {
        if config.sqpoll {
            let mut builder = IoUring::builder();
            builder.setup_single_issuer();
            builder.setup_coop_taskrun();
            builder.setup_sqpoll(config.sqpoll_idle_ms);
            match builder.build(config.sq_entries) {
                Ok(ring) => return Ok(Self { inner: ring }),
                Err(e) => eprintln!("[vortex] SQPOLL failed ({}), trying DEFER_TASKRUN", e),
            }
        }

        // Try DEFER_TASKRUN (best for single-thread rings, kernel 6.1+)
        {
            let mut builder = IoUring::builder();
            builder.setup_single_issuer();
            builder.setup_coop_taskrun();
            builder.setup_defer_taskrun();
            match builder.build(config.sq_entries) {
                Ok(ring) => return Ok(Self { inner: ring }),
                Err(e) => eprintln!("[vortex] DEFER_TASKRUN failed ({}), trying COOP_TASKRUN", e),
            }
        }

        // Try COOP_TASKRUN (kernel 5.19+)
        {
            let mut builder = IoUring::builder();
            builder.setup_coop_taskrun();
            match builder.build(config.sq_entries) {
                Ok(ring) => return Ok(Self { inner: ring }),
                Err(e) => eprintln!("[vortex] COOP_TASKRUN failed ({}), using basic mode", e),
            }
        }

        // Basic io_uring (kernel 5.4+)
        let ring = IoUring::builder().build(config.sq_entries)?;
        Ok(Self { inner: ring })
    }

    /// Push an SQE onto the submission queue.
    ///
    /// # Safety
    /// The caller must ensure the SQE references valid memory.
    #[inline(always)]
    pub unsafe fn push_sqe(&mut self, entry: &io_uring::squeue::Entry) -> Result<(), io_uring::squeue::PushError> {
        self.inner.submission().push(entry)
    }

    /// Iterate over available completion queue entries.
    /// The returned CompletionQueue implements Iterator.
    #[inline(always)]
    pub fn completions(&mut self) -> io_uring::cqueue::CompletionQueue<'_> {
        self.inner.completion()
    }

    /// Submit pending entries and wait for at least `want` completions.
    #[inline]
    pub fn submit_and_wait(&self, want: u32) -> io::Result<usize> {
        self.inner.submit_and_wait(want as usize)
    }

    /// Submit pending entries without waiting.
    #[inline]
    pub fn submit(&self) -> io::Result<usize> {
        self.inner.submit()
    }

    /// Get the raw submitter for advanced operations.
    #[inline]
    pub fn submitter(&self) -> io_uring::Submitter<'_> {
        self.inner.submitter()
    }

    /// Get mutable access to the inner IoUring (for split operations).
    #[inline]
    pub fn inner_mut(&mut self) -> &mut IoUring {
        &mut self.inner
    }
}
