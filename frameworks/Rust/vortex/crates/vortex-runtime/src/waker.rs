//! Custom Waker implementation backed by io_uring CQE user_data.
//!
//! When a CQE arrives, we use its user_data to identify the task to wake.
//! The waker simply adds the task ID to the run queue.

use std::task::{RawWaker, RawWakerVTable, Waker};

/// Create a no-op waker. In our event loop model, tasks are woken
/// directly by CQE processing, not by the Waker mechanism.
/// The waker exists only to satisfy the Future polling API.
pub fn noop_waker() -> Waker {
    fn noop_clone(data: *const ()) -> RawWaker {
        RawWaker::new(data, &VTABLE)
    }
    fn noop(_data: *const ()) {}

    static VTABLE: RawWakerVTable = RawWakerVTable::new(noop_clone, noop, noop, noop);

    unsafe { Waker::from_raw(RawWaker::new(std::ptr::null(), &VTABLE)) }
}
