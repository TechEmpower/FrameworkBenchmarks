//! CPU pinning via sched_setaffinity.
//!
//! Pins threads to specific CPU cores for cache locality and
//! to avoid NUMA-related performance degradation.

use std::io;

/// Pin the current thread to a specific CPU core.
pub fn pin_to_core(core_id: usize) -> io::Result<()> {
    unsafe {
        let mut set: libc::cpu_set_t = std::mem::zeroed();
        libc::CPU_ZERO(&mut set);
        libc::CPU_SET(core_id, &mut set);

        let ret = libc::sched_setaffinity(0, std::mem::size_of::<libc::cpu_set_t>(), &set);
        if ret < 0 {
            Err(io::Error::last_os_error())
        } else {
            Ok(())
        }
    }
}

/// Get the number of available CPU cores.
pub fn available_cores() -> usize {
    unsafe { libc::sysconf(libc::_SC_NPROCESSORS_ONLN) as usize }
}
