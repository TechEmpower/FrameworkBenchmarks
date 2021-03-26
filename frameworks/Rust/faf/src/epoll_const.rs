pub const fn const_len<T>(con: &[T]) -> usize {
   con.len()
}

pub const EPOLL_CTL_ADD: i32 = 1;
pub const EPOLL_CTL_MOD: i32 = 3;
pub const EPOLL_CTL_DEL: i32 = 2;

pub const EPOLLIN: u32 = 0x1;
pub const EPOLLPRI: u32 = 0x2;
pub const EPOLLOUT: u32 = 0x4;
pub const EPOLLRDNORM: u32 = 0x40;
pub const EPOLLRDBAND: u32 = 0x80;
pub const EPOLLWRNORM: u32 = 0x100;
pub const EPOLLWRBAND: u32 = 0x200;
pub const EPOLLMSG: u32 = 0x400;
pub const EPOLLERR: u32 = 0x8;
pub const EPOLLHUP: u32 = 0x10;
pub const EPOLLET: u32 = 0x80000000;
pub const EPOLLRDHUP: u32 = 0x2000;
pub const EPOLLEXCLUSIVE: u32 = 0x10000000;
pub const EPOLLONESHOT: u32 = 0x40000000;

pub const EXAMPLE_HTTP_RESPONSE: &[u8] = b"HTTP/1.1 200 OK\r\nContent-Length: 15\r\nContent-Type: text/plain\r\nServer: F\r\nDate: Wed, 24 Feb 2021 12:00:00 GMT\r\n\r\nHello, World!";
pub const EXAMPLE_HTTP_RESPONSE_LEN: usize = const_len(EXAMPLE_HTTP_RESPONSE);
pub const HTTP_404_NOTFOUND: &[u8] = b"HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n";
pub const HTTP_404_NOTFOUND_LEN: usize = const_len(HTTP_404_NOTFOUND);

pub const HTTP_405_NOTALLOWED: &[u8] = b"HTTP/1.1 405 Method Not Allowed\r\nContent-Length: 0\r\n\r\n";
pub const HTTP_405_NOTALLOWED_LEN: usize = const_len(HTTP_405_NOTALLOWED);

pub const HTTP_500_NOTFOUND: &[u8] = b"HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n";
pub const HTTP_500_NOTFOUND_LEN: usize = const_len(HTTP_500_NOTFOUND);
