#![allow(clippy::missing_safety_doc)]
#![feature(const_fn, const_fn_union, const_raw_ptr_deref, const_size_of_val)]

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

mod epoll;
mod epoll_callback;
mod const_config;
mod extern_http_date;
mod const_http;
mod http_content_length;
mod listener;
mod const_sys;
mod util;

#[macro_export]
macro_rules! const_concat_bytes {
    () => {
        ""
    };
    ($a:expr) => {
        $a
    };
    ($a:expr, $b:expr) => {{
        let bytes: &'static [u8] = unsafe {
            &$crate::util::concat::<
                [u8; $a.len()],
                [u8; $b.len()],
                [u8; $a.len() + $b.len()],
            >($a, $b)
        };

        unsafe { $crate::util::transmute(bytes) }
    }};
    ($a:expr, $($rest:expr),*) => {{
        pub const TAIL: &[u8] = const_concat_bytes!($($rest),*);
        const_concat_bytes!($a, TAIL)
    }};
    ($a:expr, $($rest:expr),*,) => {
        const_concat_bytes!($a, $($rest),*)
    };
}

#[inline]
fn main() {
   epoll::go(8089);
}
