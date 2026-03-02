//! Task spawning API.
//!
//! All tasks are !Send — they run exclusively on the worker thread
//! that spawned them. This eliminates all synchronization overhead.

// Task spawning is done through the Worker struct.
// This module will contain utility functions for spawning.
