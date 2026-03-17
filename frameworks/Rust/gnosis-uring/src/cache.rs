//! File cache — LRU cache for resolved static files.
//!
//! In the topology, this is the "cache" arm of the FORK:
//!   (req)-[:FORK]->(cache, mmap, disk)
//!
//! Cache hit = microsecond resolution, no disk I/O.
//! The RACE edge means cache hits short-circuit mmap and disk arms.

use std::collections::HashMap;
use std::sync::RwLock;

/// A cached file entry.
#[derive(Clone)]
pub struct CacheEntry {
    pub data: Vec<u8>,
    pub content_type: String,
    pub size: usize,
}

/// Thread-safe LRU file cache.
pub struct FileCache {
    entries: RwLock<HashMap<String, CacheEntry>>,
    max_bytes: usize,
    current_bytes: RwLock<usize>,
}

impl FileCache {
    pub fn new(max_bytes: usize) -> Self {
        Self {
            entries: RwLock::new(HashMap::new()),
            max_bytes,
            current_bytes: RwLock::new(0),
        }
    }

    /// Look up a file in cache. Returns None on miss (triggers VENT).
    pub fn get(&self, path: &str) -> Option<CacheEntry> {
        let entries = self.entries.read().ok()?;
        entries.get(path).cloned()
    }

    /// Insert a file into cache. Evicts if over capacity.
    pub fn put(&self, path: String, data: Vec<u8>, content_type: String) {
        let size = data.len();

        // Don't cache files larger than 1/4 of total capacity
        if size > self.max_bytes / 4 {
            return;
        }

        // Evict if needed (simple: clear all if over)
        {
            let current = *self.current_bytes.read().unwrap();
            if current + size > self.max_bytes {
                let mut entries = self.entries.write().unwrap();
                entries.clear();
                *self.current_bytes.write().unwrap() = 0;
            }
        }

        let entry = CacheEntry { data, content_type, size };
        let mut entries = self.entries.write().unwrap();
        entries.insert(path, entry);
        *self.current_bytes.write().unwrap() += size;
    }
}
