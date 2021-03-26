#[repr(C)]
pub union epoll_data {
   pub ptr: isize,
   pub fd: i32,
   pub uint32_t: u32,
   pub uint64_t: u64,
}
#[repr(C, packed)]
pub struct epoll_event {
   pub events: u32,
   pub data: epoll_data,
}

#[repr(C)]
pub struct in_addr {
   pub s_addr: u32,
}
#[repr(C)]
pub struct sockaddr_in {
   pub sin_family: u16,
   pub sin_port: u16,
   pub sin_addr: in_addr,
   pub sin_zero: [u8; 8],
}
#[repr(C)]
pub struct linger {
   pub l_onoff: i32,
   pub l_linger: i32,
}
