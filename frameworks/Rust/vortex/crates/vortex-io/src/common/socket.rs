//! Socket creation and configuration.
//!
//! Applies all performance-critical socket options for maximum throughput.

use std::io;
use std::os::fd::RawFd;

/// Create a non-blocking TCP listener socket with all performance options.
pub fn create_listener(addr: &str, port: u16, backlog: i32) -> io::Result<RawFd> {
    unsafe {
        // Create socket
        let fd = libc::socket(
            libc::AF_INET,
            libc::SOCK_STREAM | libc::SOCK_NONBLOCK | libc::SOCK_CLOEXEC,
            0,
        );
        if fd < 0 {
            return Err(io::Error::last_os_error());
        }

        // SO_REUSEADDR — allow rebinding immediately
        let val: libc::c_int = 1;
        setsockopt(fd, libc::SOL_SOCKET, libc::SO_REUSEADDR, &val)?;

        // SO_REUSEPORT — kernel distributes connections across per-core listeners
        setsockopt(fd, libc::SOL_SOCKET, libc::SO_REUSEPORT, &val)?;

        // TCP_NODELAY — disable Nagle's algorithm (send immediately)
        setsockopt(fd, libc::IPPROTO_TCP, libc::TCP_NODELAY, &val)?;

        // TCP_DEFER_ACCEPT — only wake on data, not just SYN-ACK
        let defer_secs: libc::c_int = 1;
        setsockopt(fd, libc::IPPROTO_TCP, libc::TCP_DEFER_ACCEPT, &defer_secs)?;

        // TCP_QUICKACK — disable delayed ACK
        setsockopt(fd, libc::IPPROTO_TCP, libc::TCP_QUICKACK, &val)?;

        // Bind
        let ip = parse_ipv4(addr).unwrap_or(0); // 0.0.0.0
        let sockaddr = libc::sockaddr_in {
            sin_family: libc::AF_INET as u16,
            sin_port: port.to_be(),
            sin_addr: libc::in_addr { s_addr: ip },
            sin_zero: [0; 8],
        };
        let ret = libc::bind(
            fd,
            &sockaddr as *const libc::sockaddr_in as *const libc::sockaddr,
            std::mem::size_of::<libc::sockaddr_in>() as libc::socklen_t,
        );
        if ret < 0 {
            libc::close(fd);
            return Err(io::Error::last_os_error());
        }

        // Listen
        let ret = libc::listen(fd, backlog);
        if ret < 0 {
            libc::close(fd);
            return Err(io::Error::last_os_error());
        }

        Ok(fd)
    }
}

/// Apply TCP_NODELAY to an accepted connection fd.
#[inline]
pub fn configure_accepted(fd: RawFd) -> io::Result<()> {
    let val: libc::c_int = 1;
    unsafe {
        setsockopt(fd, libc::IPPROTO_TCP, libc::TCP_NODELAY, &val)?;
        setsockopt(fd, libc::IPPROTO_TCP, libc::TCP_QUICKACK, &val)?;

        // SO_BUSY_POLL — spin 50μs for arriving data instead of interrupt-driven wake
        const SO_BUSY_POLL: libc::c_int = 46;
        let busy_poll: libc::c_int = 50;
        let _ = setsockopt(fd, libc::SOL_SOCKET, SO_BUSY_POLL, &busy_poll);
    }
    Ok(())
}

/// Helper to call setsockopt with proper casting.
#[inline]
unsafe fn setsockopt<T>(fd: RawFd, level: libc::c_int, name: libc::c_int, val: &T) -> io::Result<()> {
    let ret = libc::setsockopt(
        fd,
        level,
        name,
        val as *const T as *const libc::c_void,
        std::mem::size_of::<T>() as libc::socklen_t,
    );
    if ret < 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(())
    }
}

/// Attach a BPF program to route connections by CPU id.
///
/// This 3-instruction cBPF program loads `SKF_AD_CPU`, takes modulo
/// `num_sockets`, and returns the result so the kernel hands each incoming
/// connection to the socket whose index matches the CPU.  Must be called
/// AFTER `listen()`.  Failure is non-fatal — the kernel falls back to
/// default round-robin distribution.
pub fn attach_reuseport_cbpf(fd: RawFd, num_sockets: usize) -> io::Result<()> {
    // linux/filter.h: struct sock_filter { __u16 code; __u8 jt; __u8 jf; __u32 k; }
    #[repr(C)]
    struct SockFilter {
        code: u16,
        jt: u8,
        jf: u8,
        k: u32,
    }

    #[repr(C)]
    struct SockFprog {
        len: u16,
        filter: *const SockFilter,
    }

    // BPF constants
    const BPF_LD: u16 = 0x00;
    const BPF_W: u16 = 0x00;
    const BPF_ABS: u16 = 0x20;
    const BPF_ALU: u16 = 0x04;
    const BPF_MOD: u16 = 0x90;
    const BPF_K: u16 = 0x00;
    const BPF_RET: u16 = 0x06;
    const BPF_A: u16 = 0x10;
    const SKF_AD_OFF: u32 = 0xfffff000;
    const SKF_AD_CPU: u32 = 36;

    const SO_ATTACH_REUSEPORT_CBPF: libc::c_int = 51;

    let prog = [
        // LD W ABS SKF_AD_OFF+SKF_AD_CPU  — load CPU id
        SockFilter { code: BPF_LD | BPF_W | BPF_ABS, jt: 0, jf: 0, k: SKF_AD_OFF + SKF_AD_CPU },
        // ALU MOD K num_sockets            — A %= num_sockets
        SockFilter { code: BPF_ALU | BPF_MOD | BPF_K, jt: 0, jf: 0, k: num_sockets as u32 },
        // RET A                            — return A
        SockFilter { code: BPF_RET | BPF_A, jt: 0, jf: 0, k: 0 },
    ];

    let fprog = SockFprog {
        len: 3,
        filter: prog.as_ptr(),
    };

    unsafe {
        let ret = libc::setsockopt(
            fd,
            libc::SOL_SOCKET,
            SO_ATTACH_REUSEPORT_CBPF,
            &fprog as *const SockFprog as *const libc::c_void,
            std::mem::size_of::<SockFprog>() as libc::socklen_t,
        );
        if ret < 0 {
            return Err(io::Error::last_os_error());
        }
    }

    Ok(())
}

/// Parse an IPv4 address string to network-byte-order u32.
fn parse_ipv4(addr: &str) -> Option<u32> {
    if addr == "0.0.0.0" {
        return Some(0);
    }
    let parts: Vec<&str> = addr.split('.').collect();
    if parts.len() != 4 {
        return None;
    }
    let a = parts[0].parse::<u8>().ok()?;
    let b = parts[1].parse::<u8>().ok()?;
    let c = parts[2].parse::<u8>().ok()?;
    let d = parts[3].parse::<u8>().ok()?;
    Some(u32::from_ne_bytes([a, b, c, d]))
}
