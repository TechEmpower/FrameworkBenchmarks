//! Registered buffers and fixed file descriptors.
//!
//! Pre-registering resources with io_uring avoids per-operation overhead
//! for page pinning (buffers) and fd lookup (file descriptors).

use std::io;
use std::os::fd::RawFd;

/// Register a set of buffers with the io_uring instance.
///
/// Once registered, operations can reference buffers by index instead
/// of pointer, avoiding page pinning overhead on every operation.
///
/// # Safety
/// The caller must ensure buffer pointers and lengths remain valid
/// until unregistration or ring destruction.
pub unsafe fn register_buffers(
    submitter: &io_uring::Submitter<'_>,
    bufs: &[libc::iovec],
) -> io::Result<()> {
    submitter.register_buffers(bufs)?;
    Ok(())
}

/// Register a set of file descriptors with the io_uring instance.
///
/// Once registered, operations can reference fds by index,
/// avoiding fd table lookup and atomic reference counting per operation.
pub fn register_files(
    submitter: &io_uring::Submitter<'_>,
    fds: &[i32],
) -> io::Result<()> {
    submitter.register_files(fds)?;
    Ok(())
}

/// Register a sparse (empty) file table with the io_uring instance.
///
/// Creates `nr` empty slots that can be populated later with `update_file()`.
/// Requires Linux kernel 5.19+.
pub fn register_files_sparse(
    submitter: &io_uring::Submitter<'_>,
    nr: u32,
) -> io::Result<()> {
    submitter.register_files_sparse(nr)?;
    Ok(())
}

/// Register a single file descriptor into a slot in the file table.
///
/// After this call the fd can be used via `Fixed(slot)` in io_uring opcodes.
/// The caller should close the raw fd afterwards — the kernel holds its own reference.
pub fn update_file(
    submitter: &io_uring::Submitter<'_>,
    slot: u32,
    fd: RawFd,
) -> io::Result<()> {
    submitter.register_files_update(slot, &[fd])?;
    Ok(())
}
