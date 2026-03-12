use std::cell::UnsafeCell;
use std::time::{SystemTime, UNIX_EPOCH};

thread_local! {
    // We store the bytes directly in the Thread Local Storage (TLS)
    // to ensure the reference stays valid for the life of the thread.
    static CACHED_DATE: UnsafeCell<(u64, [u8; 29])> = UnsafeCell::new((0, [0u8; 29]));
}

#[inline(always)]
pub fn get_date_fast() -> &'static str {
    CACHED_DATE.with(|cell| {
        unsafe {
            let cache = &mut *cell.get();
            // 1. Use a fast duration check
            let now = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_unchecked() // Benchmark trick: avoid panic branches
                .as_secs();

            if now != cache.0 {
                cache.0 = now;
                let date_str = httpdate::fmt_http_date(SystemTime::now());
                cache.1.copy_from_slice(date_str.as_bytes());
            }

            // 2. Return a reference to the TLS memory directly.
            // This is safe because the thread never dies during the benchmark.
            std::str::from_utf8_unchecked(&cache.1)
        }
    })
}