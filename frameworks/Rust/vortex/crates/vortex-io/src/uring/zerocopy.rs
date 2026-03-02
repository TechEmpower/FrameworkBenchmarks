//! Zero-copy send support via IORING_OP_SEND_ZC.
//!
//! Transmits data directly from user memory to NIC without CPU copies.
//! Produces two CQEs: one for send completion, one for buffer release.

use io_uring::opcode;
use io_uring::types::{Fd, Fixed};
use std::os::fd::RawFd;

/// Prepare a zero-copy send SQE.
///
/// Two CQEs will be produced:
/// 1. Send completed (data delivered to socket buffer) — flags has CQE_F_MORE
/// 2. Buffer notification (safe to reuse the buffer) — flags has CQE_F_NOTIF
#[inline]
pub fn prep_send_zc(
    conn_fd: RawFd,
    buf: *const u8,
    len: u32,
    user_data: u64,
) -> io_uring::squeue::Entry {
    opcode::SendZc::new(Fd(conn_fd), buf, len)
        .build()
        .user_data(user_data)
}

/// Prepare a zero-copy send SQE using a registered file slot.
#[inline]
pub fn prep_send_zc_fixed(
    slot: u32,
    buf: *const u8,
    len: u32,
    user_data: u64,
) -> io_uring::squeue::Entry {
    opcode::SendZc::new(Fixed(slot), buf, len)
        .build()
        .user_data(user_data)
}
