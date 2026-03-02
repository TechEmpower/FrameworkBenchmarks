//! Per-core worker thread.
//!
//! Each worker owns its io_uring ring, task slab, run queue,
//! and buffer pool. The event loop processes CQEs and polls ready tasks.

use crate::executor::{RunQueue, Task, TaskSlab};
use crate::waker::noop_waker;
use vortex_io::common::affinity;
use vortex_io::uring::ring::{Ring, RingConfig};
use std::io;
use std::task::Context;

/// A per-core worker thread with its own io_uring ring and task executor.
pub struct Worker {
    /// The core ID this worker is pinned to.
    pub core_id: usize,
    /// The main io_uring ring for network I/O.
    pub ring: Ring,
    /// All active tasks.
    pub tasks: TaskSlab,
    /// Tasks ready to be polled.
    pub run_queue: RunQueue,
    /// Whether the worker should keep running.
    pub running: bool,
}

impl Worker {
    /// Create a new worker for the given core ID.
    pub fn new(core_id: usize, ring_config: &RingConfig) -> io::Result<Self> {
        Ok(Self {
            core_id,
            ring: Ring::new(ring_config)?,
            tasks: TaskSlab::new(),
            run_queue: RunQueue::new(),
            running: true,
        })
    }

    /// Pin this thread to the worker's core.
    pub fn pin_to_core(&self) -> io::Result<()> {
        affinity::pin_to_core(self.core_id)
    }

    /// Spawn a new task on this worker.
    pub fn spawn(&mut self, future: impl std::future::Future<Output = ()> + 'static) -> usize {
        let task = Task::new(future);
        self.tasks.insert(task)
    }

    /// Run the event loop.
    ///
    /// 1. Poll all ready tasks
    /// 2. Submit pending io_uring operations and wait for completions
    /// 3. Process CQEs -> wake corresponding tasks
    /// 4. Repeat
    pub fn run(&mut self) -> io::Result<()> {
        let waker = noop_waker();
        let mut cx = Context::from_waker(&waker);

        while self.running {
            // Step 1: Poll all ready tasks
            while let Some(task_id) = self.run_queue.pop() {
                let completed = if let Some(task) = self.tasks.get_mut(task_id) {
                    task.poll(&mut cx)
                } else {
                    true // Task was removed
                };

                if completed {
                    self.tasks.remove(task_id);
                }
            }

            // Step 2: Submit pending operations and wait for at least 1 completion
            let _submitted = self.ring.submit_and_wait(1)?;

            // Step 3: Process completions
            for cqe in self.ring.completions() {
                let task_id = cqe.user_data() as usize;
                // The task_id encoded in user_data tells us which task to wake.
                // In the full implementation, we'd also pass the CQE result
                // to the task via shared state.
                if task_id > 0 {
                    self.run_queue.push(task_id);
                }
            }
        }

        Ok(())
    }
}

/// Spawn worker threads, one per core, and run the event loop.
pub fn spawn_workers(
    num_workers: usize,
    init: impl Fn(usize, &mut Worker) -> io::Result<()> + Send + Sync + 'static,
) -> io::Result<Vec<std::thread::JoinHandle<io::Result<()>>>> {
    let init = std::sync::Arc::new(init);
    let mut handles = Vec::with_capacity(num_workers);

    for core_id in 0..num_workers {
        let init = init.clone();
        let handle = std::thread::Builder::new()
            .name(format!("vortex-worker-{}", core_id))
            .spawn(move || {
                let config = RingConfig::default();
                let mut worker = Worker::new(core_id, &config)?;
                worker.pin_to_core()?;
                init(core_id, &mut worker)?;
                worker.run()
            })?;
        handles.push(handle);
    }

    Ok(handles)
}
