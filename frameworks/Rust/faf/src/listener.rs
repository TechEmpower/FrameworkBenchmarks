use crate::epoll_ds::{in_addr, linger, sockaddr_in};
use crate::sys_const::*;
use sys_call::sys_call;

extern "C" {
   fn htons(hostshort: u16) -> u16;
   fn htonl(hostlong: u32) -> u32;
}

const OPTVAL: isize = 1;
const OPTVAL_SOINCOMINGCPU: isize = 0;
const OPTVAL_BUSYPOLL: isize = 50;
const OPTVAL_TCPFASTOPEN_QUEUE_LEN: isize = 4096;
const OPTVAL_TCPDEFERACCEPT_TIMEOUT: isize = 3;
const OPTVAL_SOLINGER_TIMEOUT: linger = linger { l_onoff: 1, l_linger: 0 };

#[inline]
pub fn get_listener_fd(port: u16) -> (isize, sockaddr_in, u32) {
   pub const AF_INET: i32 = 2;
   unsafe {
      let fd_listener = sys_call!(SYS_SOCKET as isize, AF_INET as isize, SOCK_STREAM as isize, 0);
      let size_of_optval = std::mem::size_of_val(&OPTVAL) as u32;

      sys_call!(
         SYS_SETSOCKOPT as isize,
         fd_listener,
         SOL_SOCKET as isize,
         SO_REUSEADDR as isize,
         &OPTVAL as *const _ as _,
         size_of_optval as isize
      );

      sys_call!(
         SYS_SETSOCKOPT as isize,
         fd_listener,
         SOL_SOCKET as isize,
         SO_REUSEPORT as isize,
         &OPTVAL as *const isize as _,
         size_of_optval as isize
      );

      // sys_call!(SYS_SETSOCKOPT as isize,
      //    fd_listener,
      //    SOL_SOCKET,
      //    SO_BUSY_POLL,
      //    &OPTVAL_BUSYPOLL as *const _ as _,
      //    std::mem::size_of_val(&OPTVAL_BUSYPOLL) as u32
      // );

      //https://stackoverflow.com/a/49900878
      // sys_call!(
      //    SYS_SETSOCKOPT as isize,
      //    fd_listener,
      //    SOL_SOCKET as isize,
      //    SO_ZEROCOPY as isize,
      //    &OPTVAL as *const _ as _,
      //    std::mem::size_of_val(&OPTVAL) as isize
      // );

      sys_call!(
         SYS_SETSOCKOPT as isize,
         fd_listener,
         IPPROTO_TCP as isize,
         TCP_QUICKACK as isize,
         &OPTVAL as *const _ as _,
         std::mem::size_of_val(&OPTVAL) as isize
      );

      // sys_call!(SYS_SETSOCKOPT as isize,
      //    fd_listener,
      //    IPPROTO_TCP as isize,
      //    TCP_DEFER_ACCEPT as isize,
      //    &OPTVAL_TCPDEFERACCEPT_TIMEOUT as *const _ as _,
      //    std::mem::size_of_val(&OPTVAL_TCPDEFERACCEPT_TIMEOUT) as u32
      // ));

      sys_call!(
         SYS_SETSOCKOPT as isize,
         fd_listener,
         IPPROTO_TCP as isize,
         TCP_FASTOPEN as isize,
         &OPTVAL_TCPFASTOPEN_QUEUE_LEN as *const _ as _,
         std::mem::size_of_val(&OPTVAL_TCPFASTOPEN_QUEUE_LEN) as isize
      );

      let addr = sockaddr_in {
         sin_family: AF_INET as u16,
         sin_port: htons(port),
         sin_addr: in_addr { s_addr: htonl(INADDR_ANY) },
         sin_zero: std::mem::zeroed(),
      };

      sys_call!(SYS_BIND as isize, fd_listener, &addr as *const _ as _, std::mem::size_of_val(&addr) as isize);

      sys_call!(SYS_LISTEN as isize, fd_listener, OPTVAL_TCPFASTOPEN_QUEUE_LEN);

      let sock_len: u32 = std::mem::size_of::<sockaddr_in>() as u32;
      (fd_listener, addr, sock_len)
   }
}

#[inline]
pub fn setup_client_connection(fd: isize, core: i32) {
   pub const O_NONBLOCK: isize = 2048;
   pub const F_GETFL: isize = 3;
   pub const F_SETFL: isize = 4;

   unsafe {
      sys_call!(SYS_FCNTL as isize, fd as isize, F_SETFL, O_NONBLOCK);

      //Doesn't help with throughput, just latency per request
      // sys_call!(
      //    SYS_SETSOCKOPT as isize,
      //    fd,
      //    IPPROTO_TCP as isize,
      //    TCP_NODELAY as isize,
      //    &OPTVAL as *const _ as _,
      //    std::mem::size_of_val(&OPTVAL) as isize
      // );

      // sys_call!(
      //    SYS_SETSOCKOPT as isize,
      //    fd as isize,
      //    SOL_SOCKET as isize,
      //    SO_ZEROCOPY as isize,
      //    &OPTVAL as *const isize as _,
      //    std::mem::size_of_val(&OPTVAL) as isize
      // );

      sys_call!(
         SYS_SETSOCKOPT as isize,
         fd,
         IPPROTO_TCP as isize,
         TCP_QUICKACK as isize,
         &OPTVAL as *const _ as _,
         std::mem::size_of_val(&OPTVAL) as isize
      );

      // This can be disabled if we are passed, say, '-1' for times we don't want to assign a core affinity
      if core >= 0 {
         sys_call!(
            SYS_SETSOCKOPT as isize,
            fd,
            SOL_SOCKET as isize,
            SO_INCOMING_CPU as isize,
            &core as *const _ as _,
            std::mem::size_of_val(&core) as isize
         );
      }

      sys_call!(
         SYS_SETSOCKOPT as isize,
         fd,
         SOL_SOCKET as isize,
         SO_BUSY_POLL as isize,
         &OPTVAL_BUSYPOLL as *const _ as _,
         std::mem::size_of_val(&OPTVAL_BUSYPOLL) as isize
      );
   }
}

#[inline]
pub fn prepare_abort_connection(fd: isize) {
   unsafe {
      sys_call!(
         SYS_SETSOCKOPT as isize,
         fd,
         SOL_SOCKET as isize,
         SO_LINGER as isize,
         &OPTVAL_SOLINGER_TIMEOUT as *const _ as _,
         std::mem::size_of_val(&OPTVAL_SOLINGER_TIMEOUT) as isize
      );
   }
}

#[inline]
pub fn get_new_addr() -> sockaddr_in {
   unsafe { std::mem::zeroed() }
}
