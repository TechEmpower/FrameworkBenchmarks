#![allow(unused_must_use)]
#![allow(deprecated)]
#![allow(dead_code)]
#![allow(clippy::comparison_chain, clippy::missing_safety_doc)]
#![feature(const_fn, const_fn_union, const_raw_ptr_deref, const_size_of_val)]

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

// #[global_allocator]
// static GLOBAL: jemallocator::Jemalloc = jemallocator::Jemalloc;

// #[global_allocator]
// static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

pub mod epoll;
mod epoll_callback;
pub mod epoll_config;
mod epoll_const;
pub mod epoll_ds;
pub mod extern_httpdate;
mod listener;
mod sys_const;
pub mod util;

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
   //    let mut buf = extern_httpdate::get_buff_with_date();
   //    extern_httpdate::get_http_date(&mut buf);
   //    let date = unsafe { std::str::from_utf8_unchecked(&buf[..]) };
   //    println!("{}", date);

   epoll::go(8089);
}
