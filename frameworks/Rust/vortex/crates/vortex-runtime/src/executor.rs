//! Per-thread task executor.
//!
//! A minimal single-threaded Future executor driven by io_uring completions.
//! Each CQE wakes the corresponding task via user_data -> task_id mapping.

use std::collections::VecDeque;
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};

/// A task wrapping a pinned, boxed future.
pub struct Task {
    future: Pin<Box<dyn Future<Output = ()>>>,
}

impl Task {
    pub fn new(future: impl Future<Output = ()> + 'static) -> Self {
        Self {
            future: Box::pin(future),
        }
    }

    /// Poll the task's future. Returns true if the task completed.
    pub fn poll(&mut self, cx: &mut Context<'_>) -> bool {
        matches!(self.future.as_mut().poll(cx), Poll::Ready(()))
    }
}

/// Simple slab-like task storage with ID-based access.
pub struct TaskSlab {
    tasks: Vec<Option<Task>>,
    free_ids: VecDeque<usize>,
    next_id: usize,
}

impl TaskSlab {
    pub fn new() -> Self {
        Self {
            tasks: Vec::with_capacity(1024),
            free_ids: VecDeque::with_capacity(256),
            next_id: 0,
        }
    }

    /// Insert a task and return its ID.
    pub fn insert(&mut self, task: Task) -> usize {
        if let Some(id) = self.free_ids.pop_front() {
            self.tasks[id] = Some(task);
            id
        } else {
            let id = self.next_id;
            self.next_id += 1;
            if id >= self.tasks.len() {
                self.tasks.push(Some(task));
            } else {
                self.tasks[id] = Some(task);
            }
            id
        }
    }

    /// Get a mutable reference to a task by ID.
    #[inline]
    pub fn get_mut(&mut self, id: usize) -> Option<&mut Task> {
        self.tasks.get_mut(id).and_then(|slot| slot.as_mut())
    }

    /// Remove a task by ID, returning it to the free list.
    pub fn remove(&mut self, id: usize) {
        if id < self.tasks.len() {
            self.tasks[id] = None;
            self.free_ids.push_back(id);
        }
    }
}

/// The run queue: a simple FIFO of task IDs ready to be polled.
pub struct RunQueue {
    queue: VecDeque<usize>,
}

impl RunQueue {
    pub fn new() -> Self {
        Self {
            queue: VecDeque::with_capacity(256),
        }
    }

    #[inline]
    pub fn push(&mut self, task_id: usize) {
        self.queue.push_back(task_id);
    }

    #[inline]
    pub fn pop(&mut self) -> Option<usize> {
        self.queue.pop_front()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }
}
