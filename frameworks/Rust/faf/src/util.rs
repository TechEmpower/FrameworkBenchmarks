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

// Basically, convert u64 to ascii string representation to bytes. This is useful for Content-Length

const DIGITS_LUT: [char; 200] = [
   '0', '0', '0', '1', '0', '2', '0', '3', '0', '4', '0', '5', '0', '6', '0', '7', '0', '8', '0', '9', '1', '0', '1',
   '1', '1', '2', '1', '3', '1', '4', '1', '5', '1', '6', '1', '7', '1', '8', '1', '9', '2', '0', '2', '1', '2', '2',
   '2', '3', '2', '4', '2', '5', '2', '6', '2', '7', '2', '8', '2', '9', '3', '0', '3', '1', '3', '2', '3', '3', '3',
   '4', '3', '5', '3', '6', '3', '7', '3', '8', '3', '9', '4', '0', '4', '1', '4', '2', '4', '3', '4', '4', '4', '5',
   '4', '6', '4', '7', '4', '8', '4', '9', '5', '0', '5', '1', '5', '2', '5', '3', '5', '4', '5', '5', '5', '6', '5',
   '7', '5', '8', '5', '9', '6', '0', '6', '1', '6', '2', '6', '3', '6', '4', '6', '5', '6', '6', '6', '7', '6', '8',
   '6', '9', '7', '0', '7', '1', '7', '2', '7', '3', '7', '4', '7', '5', '7', '6', '7', '7', '7', '8', '7', '9', '8',
   '0', '8', '1', '8', '2', '8', '3', '8', '4', '8', '5', '8', '6', '8', '7', '8', '8', '8', '9', '9', '0', '9', '1',
   '9', '2', '9', '3', '9', '4', '9', '5', '9', '6', '9', '7', '9', '8', '9', '9',
];

#[inline]
pub fn u64toa(buf: &mut [i8], value: u64) -> i64 {
   let mut index: usize = 0;
   //let b: *mut i8 = buf as *mut i8;
   if value < 100000000 {
      let v: u32 = (value) as u32;
      if v < 10000 {
         let d1: u32 = (v / 100) << 1;
         let d2: u32 = (v % 100) << 1;

         if v >= 1000 {
            buf[index] = DIGITS_LUT[d1 as usize] as i8;
            index += 1;
         }

         if v >= 100 {
            buf[index] = DIGITS_LUT[d1 as usize + 1] as i8;
            index += 1;
         }

         if v >= 10 {
            buf[index] = DIGITS_LUT[d2 as usize] as i8;
            index += 1;
         }

         buf[index] = DIGITS_LUT[d2 as usize + 1] as i8;
         index += 1;
      } else {
         let b: u32 = v / 10000;
         let c: u32 = v % 10000;

         let d1: u32 = (b / 100) << 1;
         let d2: u32 = (b % 100) << 1;

         let d3: u32 = (c / 100) << 1;
         let d4: u32 = (c % 100) << 1;

         if value >= 10000000 {
            buf[index] = DIGITS_LUT[d1 as usize] as i8;
            index += 1;
         }

         if value >= 1000000 {
            buf[index] = DIGITS_LUT[d1 as usize + 1] as i8;
            index += 1;
         }

         if value >= 100000 {
            buf[index] = DIGITS_LUT[d2 as usize] as i8;
            index += 1;
         }

         buf[index] = DIGITS_LUT[d2 as usize + 1] as i8;
         index += 1;

         buf[index] = DIGITS_LUT[d3 as usize] as i8;
         index += 1;

         buf[index] = DIGITS_LUT[d3 as usize + 1] as i8;
         index += 1;

         buf[index] = DIGITS_LUT[d4 as usize] as i8;
         index += 1;

         buf[index] = DIGITS_LUT[d4 as usize + 1] as i8;
         index += 1;
      }
   }

   index as i64
}
