#![allow(dead_code)]
use std::cmp;

use atoi::FromRadix10;

pub const SIZE: usize = 27;

pub fn get_query_param(query: &str) -> u16 {
    let q = if let Some(pos) = query.find('q') {
        u16::from_radix_10(query.split_at(pos + 2).1.as_ref()).0
    } else {
        1
    };
    cmp::min(500, cmp::max(1, q))
}

// Reverse u16 writer
static DIGITS_LUT: &[u8] = b"\
      0001020304050607080910111213141516171819\
      2021222324252627282930313233343536373839\
      4041424344454647484950515253545556575859\
      6061626364656667686970717273747576777879\
      8081828384858687888990919293949596979899";

#[inline(always)]
fn dig(n: usize) -> u8 {
    debug_assert!(n < DIGITS_LUT.len());
    unsafe { *DIGITS_LUT.as_ptr().add(n) }
}

#[inline(always)]
fn sum_0(n: u8) -> u8 {
    n.sum(b'0')
}

trait UnsafeInteger: Copy {
    fn sum(self, a: Self) -> Self;
    fn dib(self, a: Self) -> Self;
    fn ren(self, a: Self) -> Self;
    fn m2(self) -> usize;
}

macro_rules! impl_unsafe_integers {
    ($($t:tt)+) => {
    $(
    impl UnsafeInteger for $t {
        #[inline(always)]
        fn sum(self, b: Self) -> Self {
            debug_assert!(self.checked_add(b).is_some());
            self.wrapping_add(b)
        }

        #[inline(always)]
        fn dib(self, b: Self) -> Self {
            debug_assert!(self.checked_div(b).is_some());
            self.wrapping_div(b)
        }

        #[inline(always)]
        fn ren(self, b: Self) -> Self {
            debug_assert!(self.checked_rem(b).is_some());
            self.wrapping_rem(b)
        }

        #[inline(always)]
        fn m2(self) -> usize {
            debug_assert!(self.checked_shl(1).is_some());
            (self as usize).wrapping_shl(1)
        }
    }
    )+
    };
}

impl_unsafe_integers!(u8 u16 usize);

pub unsafe fn write_u16_reverse(value: u16, buf: *mut u8) -> usize {
    if value < 100 {
        if value < 10 {
            *buf = sum_0(value as u8);
            1
        } else {
            let d = value.m2();
            *buf.offset(-1) = dig(d);
            *buf = dig(d.sum(1));
            2
        }
    } else if value < 10_000 {
        let d2 = value.ren(100).m2();
        if value < 1_000 {
            *buf.offset(-2) = sum_0(value.dib(100) as u8);
            *buf.offset(-1) = dig(d2);
            *buf = dig(d2.sum(1));
            3
        } else {
            let d1 = value.dib(100).m2();
            *buf.offset(-3) = dig(d1);
            *buf.offset(-2) = dig(d1.sum(1));
            *buf.offset(-1) = dig(d2);
            *buf = dig(d2.sum(1));
            4
        }
    } else {
        let c = value.ren(10_000);

        let d1 = c.dib(100).m2();
        let d2 = c.ren(100).m2();

        *buf.offset(-4) = sum_0(value.dib(10_000) as u8);
        *buf.offset(-3) = dig(d1);
        *buf.offset(-2) = dig(d1.sum(1));
        *buf.offset(-1) = dig(d2);
        *buf = dig(d2.sum(1));
        5
    }
}
