//! Multishot accept and recv operations.
//!
//! Multishot operations submit a single SQE that produces multiple CQEs,
//! eliminating the need to re-arm after each completion.

use io_uring::opcode;
use io_uring::types::{Fd, Fixed};
use std::os::fd::RawFd;

/// Prepare a multishot accept SQE.
///
/// A single submission produces one CQE per accepted connection.
/// The `IORING_CQE_F_MORE` flag indicates more completions will follow.
/// The accepted fd is returned in `cqe.result()`.
#[inline]
pub fn prep_multishot_accept(listener_fd: RawFd, user_data: u64) -> io_uring::squeue::Entry {
    opcode::AcceptMulti::new(Fd(listener_fd))
        .build()
        .user_data(user_data)
}

/// Prepare a multishot recv SQE with provided buffer group.
///
/// A single submission produces one CQE per incoming data chunk.
/// The buffer ID is extracted via `io_uring::cqueue::buffer_select(flags)`.
/// Data is in `provided_buffers[buffer_id]` with length `cqe.result()`.
#[inline]
pub fn prep_multishot_recv(
    conn_fd: RawFd,
    buf_group_id: u16,
    user_data: u64,
) -> io_uring::squeue::Entry {
    opcode::RecvMulti::new(Fd(conn_fd), buf_group_id)
        .build()
        .user_data(user_data)
}

/// Prepare a standard recv SQE (single-shot).
#[inline]
pub fn prep_recv(
    conn_fd: RawFd,
    buf: *mut u8,
    len: u32,
    user_data: u64,
) -> io_uring::squeue::Entry {
    opcode::Recv::new(Fd(conn_fd), buf, len)
        .build()
        .user_data(user_data)
}

/// Prepare a single-shot recv SQE using provided buffer group.
///
/// The kernel selects a buffer from the ring. On completion, extract
/// the buffer ID via `buffer_id(cqe.flags())`.
#[inline]
pub fn prep_recv_buf_select(
    conn_fd: RawFd,
    buf_size: u32,
    buf_group_id: u16,
    user_data: u64,
) -> io_uring::squeue::Entry {
    opcode::Recv::new(Fd(conn_fd), std::ptr::null_mut(), buf_size)
        .buf_group(buf_group_id)
        .build()
        .flags(io_uring::squeue::Flags::BUFFER_SELECT)
        .user_data(user_data)
}

/// Prepare a send SQE.
#[inline]
pub fn prep_send(
    conn_fd: RawFd,
    buf: *const u8,
    len: u32,
    user_data: u64,
) -> io_uring::squeue::Entry {
    opcode::Send::new(Fd(conn_fd), buf, len)
        .build()
        .user_data(user_data)
}

/// Prepare a close SQE.
#[inline]
pub fn prep_close(fd: RawFd, user_data: u64) -> io_uring::squeue::Entry {
    opcode::Close::new(Fd(fd)).build().user_data(user_data)
}

// ── Registered file (Fixed) variants ─────────────────────────────────

/// Prepare a single-shot recv SQE using a registered file slot + provided buffer group.
#[inline]
pub fn prep_recv_buf_select_fixed(
    slot: u32,
    buf_size: u32,
    buf_group_id: u16,
    user_data: u64,
) -> io_uring::squeue::Entry {
    opcode::Recv::new(Fixed(slot), std::ptr::null_mut(), buf_size)
        .buf_group(buf_group_id)
        .build()
        .flags(io_uring::squeue::Flags::BUFFER_SELECT)
        .user_data(user_data)
}

/// Prepare a recv SQE using a registered file slot.
#[inline]
pub fn prep_recv_fixed(
    slot: u32,
    buf: *mut u8,
    len: u32,
    user_data: u64,
) -> io_uring::squeue::Entry {
    opcode::Recv::new(Fixed(slot), buf, len)
        .build()
        .user_data(user_data)
}

/// Prepare a send SQE using a registered file slot.
#[inline]
pub fn prep_send_fixed(
    slot: u32,
    buf: *const u8,
    len: u32,
    user_data: u64,
) -> io_uring::squeue::Entry {
    opcode::Send::new(Fixed(slot), buf, len)
        .build()
        .user_data(user_data)
}

/// Prepare a close SQE for a registered file slot.
#[inline]
pub fn prep_close_fixed(slot: u32, user_data: u64) -> io_uring::squeue::Entry {
    opcode::Close::new(Fixed(slot)).build().user_data(user_data)
}

/// Check if a CQE has the MORE flag (multishot still active).
#[inline]
pub fn has_more(flags: u32) -> bool {
    io_uring::cqueue::more(flags)
}

/// Extract buffer ID from CQE flags (for provided buffer operations).
#[inline]
pub fn buffer_id(flags: u32) -> Option<u16> {
    io_uring::cqueue::buffer_select(flags)
}
