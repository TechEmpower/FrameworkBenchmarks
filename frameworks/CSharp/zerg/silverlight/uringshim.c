// uringshim.c
#define _GNU_SOURCE

#include <errno.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>         // syscall()
#include <sys/syscall.h>    // __NR_io_uring_enter
#include <liburing.h>

// Build:
//   gcc -O2 -fPIC -shared -o liburingshim.so uringshim.c -luring
//
// Purpose
// -------
// This is a thin C shim around liburing that exposes a stable C ABI for P/Invoke.
// Keep it boring: return negative errno-style codes, avoid callbacks, avoid C++.
//
// Conventions
// -----------
// - Functions returning int: 0 / positive on success, or -errno on failure.
// - Pointers returned: NULL on failure, with *err_out / *ret_out filled if provided.
// - Managed side should treat all negative values as errors and map them to exceptions.
//
// Kernel/liburing assumptions
// ---------------------------
// This assumes a recent kernel + liburing for:
// - io_uring_prep_multishot_accept()
// - io_uring_prep_recv_multishot() + IOSQE_BUFFER_SELECT
// - io_uring_setup_buf_ring() / io_uring_free_buf_ring()
//
// Notes on io_uring_enter timeout ABI
// ----------------------------------
// There are two relevant ABIs:
// 1) "Simple" ABI: arg = (sigset_t*) or NULL, argsz = sigset_t size or 0.
// 2) "Extended" ABI (IORING_ENTER_EXT_ARG): arg points to io_uring_getevents_arg,
//    which can include a pointer to __kernel_timespec.
//
// liburing wraps these via io_uring_enter() / io_uring_enter2().
// We provide shim_enter() that directly uses the syscall with EXT_ARG when ts != NULL,
// so managed code can do "one enter per loop" without liburing helpers.

// -----------------------------------------------------------------------------
// Ring lifecycle
// -----------------------------------------------------------------------------

/**
 * Returns the ring's setup flags (IORING_SETUP_* bits) as stored in struct io_uring.
 * Useful to confirm that SQPOLL / SQ_AFF actually got enabled by the kernel.
 */
unsigned shim_get_ring_flags(struct io_uring* ring)
{
    if (!ring) return 0;
    return ring->flags;
}

/**
 * Create ring with io_uring_queue_init_params.
 *
 * entries           : ring size
 * flags             : IORING_SETUP_* flags (e.g. IORING_SETUP_SQPOLL | IORING_SETUP_SQ_AFF)
 * sq_thread_cpu     : cpu to pin SQPOLL thread to (only used if IORING_SETUP_SQ_AFF)
 *                     pass -1 to let kernel choose.
 * sq_thread_idle_ms : SQPOLL idle in milliseconds (only used if IORING_SETUP_SQPOLL)
 *
 * Returns: heap-allocated struct io_uring* or NULL on error.
 * err_out: set to 0 on success or -errno / -ENOMEM on failure.
 */
struct io_uring* shim_create_ring_ex(unsigned entries,
                                     unsigned flags,
                                     int      sq_thread_cpu,
                                     unsigned sq_thread_idle_ms,
                                     int*     err_out)
{
    struct io_uring* ring = (struct io_uring*)malloc(sizeof(struct io_uring));
    if (!ring)
    {
        if (err_out) *err_out = -ENOMEM;
        return NULL;
    }

    memset(ring, 0, sizeof(*ring));

    struct io_uring_params p;
    memset(&p, 0, sizeof(p));

    p.flags = flags;

    // SQPOLL tuning only matters if SQPOLL is requested.
    if (flags & IORING_SETUP_SQPOLL)
    {
        p.sq_thread_idle = sq_thread_idle_ms;

        // Only set sq_thread_cpu if SQ_AFF is requested and caller passed a valid cpu.
        if ((flags & IORING_SETUP_SQ_AFF) && sq_thread_cpu >= 0)
        {
            p.sq_thread_cpu = (unsigned)sq_thread_cpu;
        }
    }

    fprintf(stderr, "shim_create_ring_ex: flags=0x%x sq_cpu=%d idle=%u\n",
        flags, sq_thread_cpu, sq_thread_idle_ms);
    fprintf(stderr, "params: p.flags=0x%x p.wq_fd=%u\n", p.flags, p.wq_fd);
    
    int rc = io_uring_queue_init_params(entries, ring, &p);
    if (rc < 0)
    {
        free(ring);
        if (err_out) *err_out = rc;
        return NULL;
    }

    if (err_out) *err_out = 0;
    return ring;
}

/**
 * Create ring with default parameters (single issuer; no SQPOLL).
 */
struct io_uring* shim_create_ring(unsigned entries, int* err_out)
{
    struct io_uring* ring = (struct io_uring*)malloc(sizeof(struct io_uring));
    if (!ring)
    {
        if (err_out) *err_out = -ENOMEM;
        return NULL;
    }

    memset(ring, 0, sizeof(*ring));

    int rc = io_uring_queue_init(entries, ring, 0);
    if (rc < 0)
    {
        free(ring);
        if (err_out) *err_out = rc;
        return NULL;
    }

    if (err_out) *err_out = 0;
    return ring;
}

/**
 * Tear down ring and free heap memory (safe with NULL).
 */
void shim_destroy_ring(struct io_uring* ring)
{
    if (!ring) return;
    io_uring_queue_exit(ring);
    free(ring);
}

// -----------------------------------------------------------------------------
// SQ / CQ core operations
// -----------------------------------------------------------------------------

/** Get a free SQE or NULL if SQ is full. */
struct io_uring_sqe* shim_get_sqe(struct io_uring* ring)
{
    return io_uring_get_sqe(ring);
}

/** Number of SQEs currently queued (not yet submitted). */
unsigned shim_sq_ready(struct io_uring* ring)
{
    return io_uring_sq_ready(ring);
}

/** Submit pending SQEs. Returns number submitted or -errno. */
int shim_submit(struct io_uring* ring)
{
    return io_uring_submit(ring);
}

/** Number of CQEs available to consume right now (userspace-only check). */
unsigned shim_cq_ready(struct io_uring* ring)
{
    return io_uring_cq_ready(ring);
}

/** Block until at least 1 CQE is available. */
int shim_wait_cqe(struct io_uring* ring, struct io_uring_cqe** cqe)
{
    return io_uring_wait_cqe(ring, cqe);
}

/**
 * Wait for at least wait_nr CQEs or timeout (or earlier if kernel decides).
 * Returns 0 on success, -errno on error/timeout.
 *
 * NOTE: ts is a relative timeout here, kernel-style.
 */
int shim_wait_cqes(struct io_uring *ring,
                   struct io_uring_cqe **cqe_ptr,
                   unsigned wait_nr,
                   struct __kernel_timespec *ts)
{
    // sigmask=NULL => no signal mask changes
    return io_uring_wait_cqes(ring, cqe_ptr, wait_nr, ts, NULL);
}

/** Wait for 1 CQE or timeout. Returns 0 or -errno (commonly -ETIME). */
int shim_wait_cqe_timeout(struct io_uring *ring,
                          struct io_uring_cqe **cqe_ptr,
                          struct __kernel_timespec *ts)
{
    return io_uring_wait_cqe_timeout(ring, cqe_ptr, ts);
}

/**
 * Convenience: timeout in milliseconds.
 * Returns 0 on success, -ETIME on timeout, or -errno on error.
 */
int shim_wait_cqe_timeout_in(struct io_uring* ring,
                             struct io_uring_cqe** cqe,
                             long timeout_ms)
{
    struct __kernel_timespec ts;
    ts.tv_sec  = timeout_ms / 1000;
    ts.tv_nsec = (timeout_ms % 1000) * 1000000LL; // ms -> ns

    return io_uring_wait_cqe_timeout(ring, cqe, &ts);
}

/**
 * Non-blocking: peek up to 'count' CQEs.
 * Returns number of CQEs written to cqes (0..count).
 */
int shim_peek_batch_cqe(struct io_uring* ring, struct io_uring_cqe** cqes, unsigned count)
{
    return io_uring_peek_batch_cqe(ring, cqes, count);
}

/** Mark one CQE consumed. */
void shim_cqe_seen(struct io_uring* ring, struct io_uring_cqe* cqe)
{
    io_uring_cqe_seen(ring, cqe);
}

/**
 * Advance CQ head by 'count' CQEs, for batched consumption
 * after io_uring_peek_batch_cqe().
 */
void shim_cq_advance(struct io_uring* ring, unsigned count)
{
    io_uring_cq_advance(ring, count);
}

/**
 * Combined submit + wait (single enter). No timeout.
 * Returns number submitted or -errno.
 */
int shim_submit_and_wait(struct io_uring* ring, unsigned wait_nr)
{
    return io_uring_submit_and_wait(ring, wait_nr);
}

/**
 * Combined submit + wait with timeout, returning:
 * - >=0 / 0 on success (see liburing docs for exact behavior)
 * - -errno on error/timeout.
 *
 * This is the *liburing* helper, not raw syscall.
 */
int shim_submit_and_wait_timeout(struct io_uring *ring,
                                 struct io_uring_cqe **cqes,
                                 unsigned int wait_nr,
                                 struct __kernel_timespec *ts)
{
    return io_uring_submit_and_wait_timeout(ring, cqes, wait_nr, ts, NULL);
}

// -----------------------------------------------------------------------------
// User-data helpers (64-bit opaque tag propagated SQE -> CQE)
// -----------------------------------------------------------------------------

/** Store a 64-bit user-data value in SQE. */
void shim_sqe_set_data64(struct io_uring_sqe* sqe, unsigned long long data)
{
    io_uring_sqe_set_data64(sqe, data);
}

/** Load 64-bit user-data from CQE. */
unsigned long long shim_cqe_get_data64(const struct io_uring_cqe* cqe)
{
    return io_uring_cqe_get_data64(cqe);
}

// -----------------------------------------------------------------------------
// Multishot operations
// -----------------------------------------------------------------------------

/**
 * Prepare multishot accept. Each CQE yields one accepted fd until the kernel stops.
 * flags: typically SOCK_NONBLOCK | SOCK_CLOEXEC
 */
void shim_prep_multishot_accept(struct io_uring_sqe* sqe, int lfd, int flags)
{
    io_uring_prep_multishot_accept(sqe, lfd, NULL, NULL, flags);
}

/**
 * Prepare multishot recv that selects buffers from a registered buf-ring group.
 * buf_group must match the bgid used in setup_buf_ring.
 *
 * flags is passed to recv(2). Commonly 0; you may use MSG_WAITALL, etc. if you know why.
 */
void shim_prep_recv_multishot_select(struct io_uring_sqe* sqe, int fd, unsigned buf_group, int flags)
{
    io_uring_prep_recv_multishot(sqe, fd, NULL, 0, flags);

    // BUFFER_SELECT tells kernel to pick a buffer from the ring for each recv completion.
    sqe->flags |= IOSQE_BUFFER_SELECT;
    sqe->buf_group = (uint16_t)buf_group;
}

// -----------------------------------------------------------------------------
// Buf-ring helpers
// -----------------------------------------------------------------------------

/**
 * Create/register a buf-ring with 'entries' under buffer-group 'bgid'.
 *
 * On success:
 *  - returns buf_ring pointer
 *  - sets *ret_out = 0
 *
 * On failure:
 *  - returns NULL
 *  - sets *ret_out = -errno
 *
 * After creation, populate using io_uring_buf_ring_add() and then
 * io_uring_buf_ring_advance() to publish.
 */
struct io_uring_buf_ring* shim_setup_buf_ring(struct io_uring* ring,
                                              unsigned entries,
                                              unsigned bgid,
                                              unsigned flags,
                                              int* ret_out)
{
    return io_uring_setup_buf_ring(ring, entries, (int)bgid, flags, ret_out);
}

/**
 * Free/unregister a buf-ring. Caller must ensure no in-flight ops still reference it.
 */
void shim_free_buf_ring(struct io_uring* ring,
                        struct io_uring_buf_ring* br,
                        unsigned entries,
                        unsigned bgid)
{
    io_uring_free_buf_ring(ring, br, entries, (int)bgid);
}

/**
 * Stage a buffer into the producer view of the buf-ring.
 * Not visible to kernel until shim_buf_ring_advance() is called.
 *
 * mask: usually entries - 1 (entries should be power-of-two).
 * idx : monotonically increasing producer index (you handle wrap via mask).
 */
void shim_buf_ring_add(struct io_uring_buf_ring* br,
                       void* addr,
                       unsigned len,
                       unsigned short bid,
                       unsigned short mask,
                       unsigned idx)
{
    io_uring_buf_ring_add(br, addr, len, bid, mask, idx);
}

/** Publish 'count' staged buffers to the kernel. */
void shim_buf_ring_advance(struct io_uring_buf_ring* br, unsigned count)
{
    io_uring_buf_ring_advance(br, count);
}

// -----------------------------------------------------------------------------
// CQE helpers for buffer selection
// -----------------------------------------------------------------------------

/** Non-zero if CQE refers to a selected buffer (IORING_CQE_F_BUFFER). */
int shim_cqe_has_buffer(const struct io_uring_cqe* cqe)
{
    return (cqe->flags & IORING_CQE_F_BUFFER) != 0;
}

/**
 * Extract buffer id (bid) used by kernel.
 * Layout: flags contains bid in upper bits, shifted by IORING_CQE_BUFFER_SHIFT.
 */
unsigned shim_cqe_buffer_id(const struct io_uring_cqe* cqe)
{
    return cqe->flags >> IORING_CQE_BUFFER_SHIFT;
}

// -----------------------------------------------------------------------------
// Send / cancel
// -----------------------------------------------------------------------------

/** Prepare send(2). */
void shim_prep_send(struct io_uring_sqe* sqe,
                    int fd,
                    const void* buf,
                    unsigned nbytes,
                    int flags)
{
    io_uring_prep_send(sqe, fd, buf, nbytes, flags);
}

/**
 * Prepare cancel by user-data (64-bit).
 * Useful to cancel in-flight multishot recv when closing a connection.
 */
void shim_prep_cancel64(struct io_uring_sqe* sqe,
                        unsigned long long user_data,
                        int flags)
{
    io_uring_prep_cancel64(sqe, user_data, flags);
}

// -----------------------------------------------------------------------------
// Direct io_uring_enter wrapper (single syscall path with optional timeout)
// -----------------------------------------------------------------------------

/**
 * shim_enter
 * ---------
 * Direct wrapper for the io_uring_enter syscall, supporting both:
 * - Simple ABI when ts == NULL (arg=NULL, argsz=0)
 * - Extended ABI when ts != NULL (IORING_ENTER_EXT_ARG + io_uring_getevents_arg)
 *
 * Why you want this:
 * - You can do "submit + wait" in one syscall per loop, with an optional timeout.
 * - Avoid liburing paths that might force extra syscalls or additional checks.
 *
 * Parameters mirror io_uring_enter:
 * - to_submit    : number of SQEs to submit (usually io_uring_sq_ready())
 * - min_complete : minimum CQEs to wait for (0 => don't wait)
 * - flags        : IORING_ENTER_* flags (GETEVENTS, SQ_WAKEUP, etc.)
 * - ts           : optional timeout (relative). If NULL, no timeout is applied.
 *
 * Returns: >=0 on success (kernel return), or -errno on failure.
 */
int shim_enter(struct io_uring* ring,
               unsigned to_submit,
               unsigned min_complete,
               unsigned flags,
               struct __kernel_timespec* ts)
{
    if (!ring) return -EINVAL;

    // Simple ABI: no timeout, arg=NULL and argsz=0.
    if (ts == NULL)
    {
        return (int)syscall(__NR_io_uring_enter,
                            ring->ring_fd,
                            to_submit,
                            min_complete,
                            flags,
                            NULL,
                            0);
    }

    // Extended ABI: pass io_uring_getevents_arg with pointer to timespec.
    // IMPORTANT: This requires IORING_ENTER_EXT_ARG in flags.
    struct io_uring_getevents_arg arg;
    memset(&arg, 0, sizeof(arg));
    arg.ts = (uint64_t)(uintptr_t)ts;

    return (int)syscall(__NR_io_uring_enter,
                        ring->ring_fd,
                        to_submit,
                        min_complete,
                        flags | IORING_ENTER_EXT_ARG,
                        &arg,
                        sizeof(arg));
}

