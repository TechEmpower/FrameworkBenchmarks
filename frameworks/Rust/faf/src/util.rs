extern "C" {
   pub fn memcmp(s1: *const i8, s2: *const i8, n: usize) -> i32;
}

#[inline]
pub const unsafe fn transmute<From, To>(from: From) -> To {
   union Transmute<From, To> {
      from: std::mem::ManuallyDrop<From>,
      to: std::mem::ManuallyDrop<To>,
   }

   std::mem::ManuallyDrop::into_inner(Transmute { from: std::mem::ManuallyDrop::new(from) }.to)
}

#[inline]
pub const unsafe fn concat<First, Second, Out>(a: &[u8], b: &[u8]) -> Out
where
   First: Copy,
   Second: Copy,
   Out: Copy,
{
   #[repr(C)]
   #[derive(Copy, Clone)]
   struct Both<A, B>(A, B);

   let arr: Both<First, Second> =
      Both(*transmute::<_, *const First>(a.as_ptr()), *transmute::<_, *const Second>(b.as_ptr()));

   transmute(arr)
}

pub const fn const_len<T>(con: &[T]) -> usize {
   con.len()
}
