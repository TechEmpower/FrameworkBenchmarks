//    let fd_epoll: libc::c_int =
//       syscall!(syscall(libc::SYS_epoll_create1, libc::EPOLL_CLOEXEC as libc::c_int)) as libc::c_int;

//    // Set up eventfd and timerfd.
//    let event_fd = syscall!(eventfd(0, libc::EFD_CLOEXEC | libc::EFD_NONBLOCK));
//    let timer_fd = syscall!(syscall(
//        libc::SYS_timerfd_create,
//        libc::CLOCK_MONOTONIC as libc::c_int,
//        (libc::TFD_CLOEXEC | libc::TFD_NONBLOCK) as libc::c_int,
//    ));

//listener.set_nonblocking(true);

//let acceptor = TcpListener::bind("127.0.0.1:6666").unwrap();

// let ring = rio::new().unwrap();
// loop {
//    let stream = ring.accept(&listener).wait().unwrap();
//    dbg!(proxy(&ring, &stream, &stream));
// }

// extreme::run(async {
//    // kernel 5.5 and later support TCP accept
//    loop {
//       let stream = ring.accept(&listener).await.unwrap();
//       dbg!(proxy(&ring, &stream, &stream).await);
//    }
// })

// fn proxy(ring: &rio::Rio, a: &TcpStream, b: &TcpStream) -> io::Result<()> {
//    let buf = vec![0_u8; 512];
//    loop {
//       let read_bytes = ring.read_at(a, &buf, 0).wait().unwrap();
//       let buf = &buf[..read_bytes];
//       ring.write_at(b, &buf, 0);
//    }
// }

// let listener: TcpListener = {

//    unsafe { TcpListener::from_raw_fd(fd_listener) }
// };

//let method_str = std::str::from_utf8_unchecked(std::slice::from_raw_parts(method as *const u8, method_len));

// std::ptr::copy_nonoverlapping(PLAINTEXT_DEBUG.as_ptr(), response_buffer.as_mut_ptr(), PLAINTEXT_DEBUG_LEN);
// return PLAINTEXT_DEBUG_LEN;

//let str_test = std::str::from_utf8_unchecked(response_buffer);
// println!("{}", str_test);

// let header1 =
//    std::str::from_utf8(std::slice::from_raw_parts(headers[0].name as *const u8, headers[0].name_len)).unwrap();
//println!("{}", header1);

// This unchecked version was slower than my original code
#[inline]
pub unsafe fn u64toa_unchecked(buf: &mut [i8], value: u64) -> i64 {
   let mut index: usize = 0;
   //let b: *mut i8 = buf as *mut i8;
   if value < 100000000 {
      let v: u32 = (value) as u32;
      if v < 10000 {
         let d1: u32 = (v / 100) << 1;
         let d2: u32 = (v % 100) << 1;

         if v >= 1000 {
            *buf.get_unchecked_mut(index) = *DIGITS_LUT.get_unchecked(d1 as usize) as i8;
            index += 1;
         }

         if v >= 100 {
            *buf.get_unchecked_mut(index) = *DIGITS_LUT.get_unchecked(d1 as usize + 1) as i8;
            index += 1;
         }

         if v >= 10 {
            *buf.get_unchecked_mut(index) = *DIGITS_LUT.get_unchecked(d2 as usize) as i8;
            index += 1;
         }

         *buf.get_unchecked_mut(index) = *DIGITS_LUT.get_unchecked(d2 as usize + 1) as i8;
         index += 1;
      } else {
         let b: u32 = v / 10000;
         let c: u32 = v % 10000;

         let d1: u32 = (b / 100) << 1;
         let d2: u32 = (b % 100) << 1;

         let d3: u32 = (c / 100) << 1;
         let d4: u32 = (c % 100) << 1;

         if value >= 10000000 {
            *buf.get_unchecked_mut(index) = *DIGITS_LUT.get_unchecked(d1 as usize) as i8;
            index += 1;
         }

         if value >= 1000000 {
            *buf.get_unchecked_mut(index) = *DIGITS_LUT.get_unchecked(d1 as usize + 1) as i8;
            index += 1;
         }

         if value >= 100000 {
            *buf.get_unchecked_mut(index) = *DIGITS_LUT.get_unchecked(d2 as usize) as i8;
            index += 1;
         }

         *buf.get_unchecked_mut(index) = *DIGITS_LUT.get_unchecked(d2 as usize + 1) as i8;
         index += 1;

         *buf.get_unchecked_mut(index) = *DIGITS_LUT.get_unchecked(d3 as usize) as i8;
         index += 1;

         *buf.get_unchecked_mut(index) = *DIGITS_LUT.get_unchecked(d3 as usize + 1) as i8;
         index += 1;

         *buf.get_unchecked_mut(index) = *DIGITS_LUT.get_unchecked(d4 as usize) as i8;
         index += 1;

         *buf.get_unchecked_mut(index) = *DIGITS_LUT.get_unchecked(d4 as usize + 1) as i8;
         index += 1;
      }
   }

   index as i64
}

// Get HTTP Header Date, SLOW AS FUCK compared to my new approach
extern "C" {
   pub fn gmtime_r(time_p: *const i64, result: *mut tm) -> *mut tm;
   pub fn time(time: *mut i64) -> i64;
   pub fn strftime(buff: *mut u8, max: usize, format: *const u8, tm: *const tm);
}

#[repr(C)]
pub struct tm {
   pub tm_sec: i32,
   pub tm_min: i32,
   pub tm_hour: i32,
   pub tm_mday: i32,
   pub tm_mon: i32,
   pub tm_year: i32,
   pub tm_wday: i32,
   pub tm_yday: i32,
   pub tm_isdst: i32,
   pub tm_gmtoff: i64,
   pub tm_zone: *const i8,
}

const DAYS: [&[u8]; 7] = [b"Sun", b"Mon", b"Tue", b"Wed", b"Thu", b"Fri", b"Sat"];
const MONTHS: [&[u8]; 12] =
   [b"Jan", b"Feb", b"Mar", b"Apr", b"May", b"Jun", b"Jul", b"Aug", b"Sep", b"Oct", b"Nov", b"Dec"];
const BASE_DATE_FORMAT: &[u8] = b"---, %d --- %Y %H:%M:%S GMT";

#[inline]
pub fn get_date(buff: &mut [u8; 30], tm: &mut tm) {
   let mut t: i64 = 0;

   unsafe {
      time(&mut t as *mut i64);
      gmtime_r(&t as *const i64, tm as *mut tm);
      strftime(buff.as_mut_ptr() as *mut i8 as _, 30, BASE_DATE_FORMAT.as_ptr(), tm as *const tm);
      std::ptr::copy_nonoverlapping(DAYS.get_unchecked(tm.tm_wday as usize).as_ptr(), buff.as_mut_ptr(), 3);
      std::ptr::copy_nonoverlapping(MONTHS.get_unchecked(tm.tm_mon as usize).as_ptr(), &mut buff[8], 3);
   }
}

// get_http_date without Date: at the first
pub fn get_buff() -> [u8; 29] {
   let buf: [u8; 29] = [
      // Writing as the following only gives us a reference: b"Thu, 01 Jan 1970 00:00:00 GMT"
      b' ', b' ', b' ', b',', b' ', b'0', b'0', b' ', b' ', b' ', b' ', b' ', b'0', b'0', b'0', b'0', b' ', b'0', b'0',
      b':', b'0', b'0', b':', b'0', b'0', b' ', b'G', b'M', b'T',
   ];

   buf
}

#[inline]
pub fn get_http_date(buf: &mut [u8; 29]) -> &str {
   let mut ts: timespec = unsafe { std::mem::zeroed() };
   unsafe { clock_gettime(CLOCK_REALTIME, &mut ts as *mut timespec) };

   let secs_since_epoch = ts.tv_sec;

   const LEAPOCH: i64 = 11017;
   const DAYS_PER_400Y: i64 = 365 * 400 + 97;
   const DAYS_PER_100Y: i64 = 365 * 100 + 24;
   const DAYS_PER_4Y: i64 = 365 * 4 + 1;

   let days = (secs_since_epoch / 86400) - LEAPOCH;
   let secs_of_day = secs_since_epoch % 86400;

   let mut qc_cycles = days / DAYS_PER_400Y;
   let mut remdays = days % DAYS_PER_400Y;

   if remdays < 0 {
      remdays += DAYS_PER_400Y;
      qc_cycles -= 1;
   }

   let mut c_cycles = remdays / DAYS_PER_100Y;
   if c_cycles == 4 {
      c_cycles -= 1;
   }
   remdays -= c_cycles * DAYS_PER_100Y;

   let mut q_cycles = remdays / DAYS_PER_4Y;
   if q_cycles == 25 {
      q_cycles -= 1;
   }
   remdays -= q_cycles * DAYS_PER_4Y;

   let mut remyears = remdays / 365;
   if remyears == 4 {
      remyears -= 1;
   }
   remdays -= remyears * 365;

   let mut year = 2000 + remyears + 4 * q_cycles + 100 * c_cycles + 400 * qc_cycles;

   let months = [31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 29];
   let mut mon = 0;
   for mon_len in months.iter() {
      mon += 1;
      if remdays < *mon_len {
         break;
      }
      remdays -= *mon_len;
   }
   let mday = remdays + 1;
   let mon = if mon + 2 > 12 {
      year += 1;
      mon - 10
   } else {
      mon + 2
   };

   let mut wday = (3 + days) % 7;
   if wday <= 0 {
      wday += 7
   };

   let sec = (secs_of_day % 60) as u8;
   let min = ((secs_of_day % 3600) / 60) as u8;
   let hour = (secs_of_day / 3600) as u8;
   let day = mday as u8;
   let mon = mon as u8;
   let year = year as u16;
   let wday = wday as u8;

   let wday = match wday {
      1 => b"Mon",
      2 => b"Tue",
      3 => b"Wed",
      4 => b"Thu",
      5 => b"Fri",
      6 => b"Sat",
      7 => b"Sun",
      _ => unsafe { std::hint::unreachable_unchecked() },
   };
   let mon = match mon {
      1 => b"Jan",
      2 => b"Feb",
      3 => b"Mar",
      4 => b"Apr",
      5 => b"May",
      6 => b"Jun",
      7 => b"Jul",
      8 => b"Aug",
      9 => b"Sep",
      10 => b"Oct",
      11 => b"Nov",
      12 => b"Dec",
      _ => unsafe { std::hint::unreachable_unchecked() },
   };

   buf[0] = wday[0];
   buf[1] = wday[1];
   buf[2] = wday[2];
   buf[5] = b'0' + (day / 10) as u8;
   buf[6] = b'0' + (day % 10) as u8;
   buf[8] = mon[0];
   buf[9] = mon[1];
   buf[10] = mon[2];
   buf[12] = b'0' + (year / 1000) as u8;
   buf[13] = b'0' + (year / 100 % 10) as u8;
   buf[14] = b'0' + (year / 10 % 10) as u8;
   buf[15] = b'0' + (year % 10) as u8;
   buf[17] = b'0' + (hour / 10) as u8;
   buf[18] = b'0' + (hour % 10) as u8;
   buf[20] = b'0' + (min / 10) as u8;
   buf[21] = b'0' + (min % 10) as u8;
   buf[23] = b'0' + (sec / 10) as u8;
   buf[24] = b'0' + (sec % 10) as u8;

   unsafe { std::str::from_utf8_unchecked(&buf[..]) }
}


// Epoll thread priority and isolation

let thread_id = thread_priority::thread_native_id();
         let thread_priority_was_successful = thread_priority::set_thread_priority_and_policy(
            thread_id,
            thread_priority::ThreadPriority::Min,
            thread_priority::ThreadSchedulePolicy::Realtime(thread_priority::RealtimeThreadSchedulePolicy::Fifo),
         )
         .is_ok();
         if !thread_priority_was_successful {
            panic!("failed to set thread priority");
         }
         let core_ids_2 = core_affinity::get_core_ids().unwrap();
         core_affinity::set_for_current(core_ids_2[worker_index + WORKER_AFFINITY_OFFSET]);

         thread_priority::set_current_thread_priority(thread_priority::ThreadPriority::Max).unwrap();


            let core_ids = core_affinity::get_core_ids().unwrap();
   let core0 = core_ids[0];
   core_affinity::set_for_current(core0);
   thread_priority::set_current_thread_priority(thread_priority::ThreadPriority::Max).unwrap();

   let thread_id = thread_priority::thread_native_id();
   let thread_priority_was_successful = thread_priority::set_thread_priority_and_policy(
      thread_id,
      thread_priority::ThreadPriority::Min,
      thread_priority::ThreadSchedulePolicy::Realtime(thread_priority::RealtimeThreadSchedulePolicy::Fifo),
   )
   .is_ok();
   if !thread_priority_was_successful {
            panic!("failed to set thread priority");
         }



static body_len: &[u8] =
   unsafe { std::slice::from_raw_parts(PLAINTEXT_BODY as *const [u8] as _, std::mem::size_of::<usize>()) };
const PLAINTEXT_BODY_LEN: usize = const_len(PLAINTEXT_BODY);

const DATE_DEBUG: &[u8] = b"Date: Wed, 24 Feb 2021 12:00:00 GMT";
const PLAINTEXT_DEBUG: &[u8] = const_concat_bytes!(
   HTTP11,
   CRLF,
   SERVER,
   CRLF,
   PLAINTEXT_CONTENT_TYPE,
   CRLF,
   CONTENT_LENGTH,
   PLAINTEXT_BODY_LEN,
   CRLF,
   DATE_DEBUG,
   CRLF,
   CRLF,
   PLAINTEXT_BODY
);

const PLAINTEXT_DEBUG_LEN: usize = const_len(PLAINTEXT_DEBUG);

const TEST: &[u8] = const_concat_usize_to_bytes!(PLAINTEXT_BASE, &PLAINTEXT_BODY_LEN);
const PLAINTEXT_BASE: &[u8] = const_concat_bytes!(HTTP11, PLAINTEXT_BODY_LEN);

const PLAINTEXT_BASE_RESPONSE: &str =



pub const fn const_len_str(con: &str) -> &[u8] {
   con.len().as_ne_bytes()
}


pub const fn const_len_as_bytes(con: &[u8]) -> &'static[u8] {
   unsafe { std::slice::from_raw_parts(con as *const [u8] as _, std::mem::size_of::<usize>()) }
}
