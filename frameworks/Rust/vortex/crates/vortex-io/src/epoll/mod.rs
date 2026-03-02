//! epoll fallback for older kernels without io_uring support.
//!
//! This follows the faf pattern: raw epoll_create1, epoll_ctl, epoll_wait
//! via libc. Only compiled when feature "epoll-fallback" is enabled.

// TODO: Implement epoll fallback (Phase 2+)
