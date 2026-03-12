use std::cell::RefCell;
use crate::models::{Fortune, World};
// Assuming water_buffer crate exists
// use water_buffer::WaterBuffer as BM;

// Mocking the type for compilation demo
type WaterBuffer = Vec<u8>;


const INITIAL_VEC_CAPACITY: usize = 2000;
const DEFAULT_SIZE: usize = 4048;
const MAX_BUFFER_SIZE: usize = 4048; // Allow some growth before discarding
const MAX_CACHED_BUFFERS: usize = 512; // Set to your test requirement


thread_local! {
    static BUFFER_CACHE: RefCell<Vec<WaterBuffer>> = RefCell::new(Vec::with_capacity(INITIAL_VEC_CAPACITY));
    static FORTUNE_CACHE: RefCell<Vec<Vec<Fortune>>> = RefCell::new(Vec::with_capacity(INITIAL_VEC_CAPACITY));
    static WORLDS_CACHE: RefCell<Vec<Vec<World>>> = RefCell::new(Vec::with_capacity(500));
    static IDS_NUMBERS: RefCell<Vec<(Vec<i32>,Vec<i32>)>> = RefCell::new(Vec::with_capacity(500));

}

pub struct PooledBuffer {
    inner: Option<WaterBuffer>,
}

pub struct FortunesPool {
    inner: Option<Vec<Fortune>>,
}

pub struct WorldsPool {
    inner: Option<Vec<World>>,
}

pub struct IDsPool {
    inner: Option<(Vec<i32>,Vec<i32>)>,
}


impl PooledBuffer {
    pub fn new() -> Self {
        Self::with_capacity(DEFAULT_SIZE)
    }

    pub fn with_capacity(cap: usize) -> Self {
        let buf = BUFFER_CACHE.with(|cache| {
            let mut cache = cache.borrow_mut();
            if let Some(mut existing_buf) = cache.pop() {
                // Assuming your WaterBuffer has a clear/reset method
                existing_buf.clear();
                existing_buf
            } else {
                // Fallback to new allocation if cache is empty
                WaterBuffer::with_capacity(cap)
            }
        });

        Self { inner: Some(buf) }
    }

    pub fn take_inner(&mut self) -> WaterBuffer {
        self.inner.take().expect("Buffer already taken or dropped")
    }

    pub fn recycle(buf: WaterBuffer) {
        // Use capacity() check to ensure we don't cache
        // a buffer that grew to a massive size during one specific request
        if buf.capacity() <= MAX_BUFFER_SIZE {
            BUFFER_CACHE.with(|cache| {
                let mut cache = cache.borrow_mut();
                if cache.len() < MAX_CACHED_BUFFERS {
                    cache.push(buf);
                }
            });
        }
    }
}
impl FortunesPool {
    pub fn new() -> Self {
        Self::with_capacity(16)
    }

    pub fn with_capacity(cap: usize) -> Self {
        let buf = FORTUNE_CACHE.with(|cache| {
            let mut cache = cache.borrow_mut();
            if let Some(mut existing_buf) = cache.pop() {
                // Assuming your WaterBuffer has a clear/reset method
                existing_buf
            } else {
                // Fallback to new allocation if cache is empty
                Vec::with_capacity(cap)
            }
        });

        Self { inner: Some(buf) }
    }

    pub fn take_inner(&mut self) -> Vec<Fortune> {
        self.inner.take().expect("Buffer already taken or dropped")
    }

    pub fn save_heap_allocation(buf: WaterBuffer) {
        // Use capacity() check to ensure we don't cache
        // a buffer that grew to a massive size during one specific request
        if buf.capacity() <= 16 {
            BUFFER_CACHE.with(|cache| {
                let mut cache = cache.borrow_mut();
                if cache.len() < 2000 {
                    cache.push(buf);
                }
            });
        }
    }
}
impl WorldsPool {
    pub fn new() -> Self {
        Self::with_capacity(500)
    }

    pub fn with_capacity(cap: usize) -> WorldsPool {
        let buf = WORLDS_CACHE.with(|cache| {
            let mut cache = cache.borrow_mut();
            if let Some(mut existing_buf) = cache.pop() {
                // Assuming your WaterBuffer has a clear/reset method
                existing_buf
            } else {
                // Fallback to new allocation if cache is empty
                Vec::with_capacity(cap)
            }
        });

        Self { inner: Some(buf) }
    }

    pub fn take_inner(&mut self) -> Vec<World> {
        self.inner.take().expect("Buffer already taken or dropped")
    }

    pub fn save_heap_allocation(buf: Vec<World>) {
        // Use capacity() check to ensure we don't cache
        // a buffer that grew to a massive size during one specific request
        if buf.capacity() <= 500 {
            WORLDS_CACHE.with(|cache| {
                let mut cache = cache.borrow_mut();
                if cache.len() < 2000 {
                    cache.push(buf);
                }
            });
        }
    }
}
impl IDsPool {
    pub fn new() -> Self {
        Self::with_capacity(500)
    }

    pub fn with_capacity(cap: usize) -> IDsPool {
        let buf = IDS_NUMBERS.with(|cache| {
            let mut cache = cache.borrow_mut();
            if let Some(mut existing_buf) = cache.pop() {
                existing_buf.0.clear();
                existing_buf.1.clear();
                existing_buf
            } else {
                // Fallback to new allocation if cache is empty
                (Vec::with_capacity(cap),Vec::with_capacity(cap))
            }
        });

        Self { inner: Some(buf) }
    }

    pub fn take_inner(&mut self) -> (Vec<i32>,Vec<i32>) {
        self.inner.take().expect("Buffer already taken or dropped")
    }

    pub fn save_heap_allocation(buf: (Vec<i32>,Vec<i32>)) {
        // Use capacity() check to ensure we don't cache
        // a buffer that grew to a massive size during one specific request
            IDS_NUMBERS.with(|cache| {
                let mut cache = cache.borrow_mut();
                if cache.len() < 1000 {
                    cache.push(buf);
                }
            });
    }
}

