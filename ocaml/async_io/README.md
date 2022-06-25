# Async_io

A multi-threaded executor that can be used for asynchronous I/O and timers.

## Implementation Notes

- Task `unit -> unit` is the smallest unit of work.
- The async_io scheduler starts [n] worker domains. [n] equals the number of logical CPU cores.
- Each worker domain contains a thread safe run queue that new tasks can be dispatched onto.
- When a worker domain doesn't have any available tasks in its own run-queue, it attempts to steal a task from a sibling domain's run-queue.
- Tasks are scheduled cooperatively within a worker domain. Only one task runs at a time within a worker. Tasks can use `Task.yield` to cooperatively yield control, and give other tasks within a domain a chance to run.
- Tasks shouldn't use blocking I/O operations. Since tasks are cooperative any blocking task will block the entire work queue. Use non-blocking file-descriptors, and use the [Fd](./src/fd.ml) module to watch for an I/O readiness events.
- Tasks are "sticky". Once a worker domain starts operating on a task, the task will stay on that specific worker's run queue, and it won't be available for other workers to steal.
- Submitting new tasks uses a round-robin approach to dispatch taks among all worker domains.
- Use [Timer](./src/timer.ml) for performing [sleep] operations instead of `Unix.sleep`, as the timer module won't block the work-queue and will yield control so other tasks can run.
