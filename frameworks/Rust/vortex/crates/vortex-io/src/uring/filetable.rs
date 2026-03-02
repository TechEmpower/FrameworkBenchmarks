//! Slot allocator for io_uring registered file descriptors.
//!
//! Each accepted connection gets a slot from the table. The slot index
//! is used with `Fixed(slot)` in io_uring opcodes instead of `Fd(raw_fd)`,
//! eliminating per-operation fd table lookup and atomic refcounting.

/// Fixed-size slot allocator backed by a free-list stack.
pub struct FileTable {
    free_slots: Vec<u32>,
}

impl FileTable {
    /// Create a new file table with `capacity` slots.
    /// All slots start as free.
    pub fn new(capacity: u32) -> Self {
        let mut free_slots = Vec::with_capacity(capacity as usize);
        // Push in reverse so slot 0 is allocated first (pop from end)
        for i in (0..capacity).rev() {
            free_slots.push(i);
        }
        Self { free_slots }
    }

    /// Allocate a free slot. Returns `None` if the table is full.
    #[inline]
    pub fn alloc(&mut self) -> Option<u32> {
        self.free_slots.pop()
    }

    /// Return a slot to the free pool.
    #[inline]
    pub fn free(&mut self, slot: u32) {
        self.free_slots.push(slot);
    }
}
