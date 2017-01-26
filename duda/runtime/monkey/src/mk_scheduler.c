/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Monkey HTTP Daemon
 *  ------------------
 *  Copyright (C) 2001-2014, Eduardo Silva P. <edsiper@gmail.com>
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU Lesser General Public  License as published
 *  by the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 *  License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <pthread.h>
#include <sys/epoll.h>
#include <sys/eventfd.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <string.h>

#include "monkey.h"
#include "mk_connection.h"
#include "mk_scheduler.h"
#include "mk_memory.h"
#include "mk_epoll.h"
#include "mk_request.h"
#include "mk_cache.h"
#include "mk_config.h"
#include "mk_clock.h"
#include "mk_signals.h"
#include "mk_plugin.h"
#include "mk_utils.h"
#include "mk_macros.h"
#include "mk_rbtree.h"
#include "mk_linuxtrace.h"

pthread_key_t worker_sched_node;

struct sched_list_node *sched_list;

static pthread_mutex_t mutex_sched_init = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex_worker_init = PTHREAD_MUTEX_INITIALIZER;

__thread struct rb_root *cs_list;

/*
 * Returns the worker id which should take a new incomming connection,
 * it returns the worker id with less active connections
 */
static inline int _next_target()
{
    int i;
    int ret;
    int target = 0;
    unsigned long long tmp = 0, cur = 0;

    cur = sched_list[0].accepted_connections - sched_list[0].closed_connections;
    if (cur == 0)
        return 0;

    /* Finds the lowest load worker */
    for (i = 1; i < config->workers; i++) {
        tmp = sched_list[i].accepted_connections - sched_list[i].closed_connections;
        if (tmp < cur) {
            target = i;
            cur = tmp;

            if (cur == 0)
                break;
        }
    }

    /*
     * If sched_list[target] worker is full then the whole server too, because
     * it has the lowest load.
     */
    if (mk_unlikely(cur >= config->worker_capacity)) {
        MK_TRACE("Too many clients: %i", config->worker_capacity * config->workers);

        /*
         * ULONG_MAX BUG
         * =============
         * This is a workaround for a specific Bug that we still not find the
         * root cause for it. The conditions are:
         *
         *  - for some reason closed_connections > active_connections
         *  - when performing (active_connections - closed_connections it will
         *    return UMAX_LONG, which is OK but *not* for our case
         *
         * - if cur (current connections) is greater than worker capacity it will
         *   fail all new incoming connections, and as it may have ULONG_MAX that
         *   would case a very very bad behavior
         *
         * The temporal workaround is to when facing that we are over capacity,
         * check if we have a ULONG_MAX value in 'cur', on that moment signal
         * our workers so they can start performing a synchronization of their
         * counters. This workaround will not generate overhead but we must
         * solve the root cause.
         */

        uint64_t val;
        uint64_t accepted;
        uint64_t closed;

        for (i = 0; i < config->workers; i++) {
            accepted = sched_list[i].accepted_connections;
            closed   = sched_list[i].closed_connections;

            if (closed > accepted) {
                /* Ok, let everybody know about this... */
                MK_LT_SCHED(0, "ULONG_MAX BUG!");
                mk_warn("ULONG_MAX BUG: Performing synchronization");

                val = MK_SCHEDULER_SIGNAL_DEADBEEF;
                ret = write(sched_list[i].signal_channel, &val, sizeof(val));
                if (ret <= 0) {
                    mk_libc_error("write");
                }
            }
        }
        /*
         * take a short nap, nobody will die...: we need to
         * to give some time to the workers to update their
         * counters, of course it will not take more than a few
         * microseconds...
         */
        sleep(1);

        /* Instruct to close the connection anyways, we lie, it will die */
        return -1;
    }

    return target;
}

/*
 * This function synchronize the worker scheduler counters in case
 * we face the ULONG_MAX bug, this is an unexpected behavior but
 * until it become fixed this routine must run with mutual exclusion
 * to avoid data corruption.
 */
int mk_sched_sync_counters()
{
    uint64_t n = 0;
    struct mk_list *head;
    struct sched_list_node *sched;


    MK_LT_SCHED(sched_signal_channel, "SYNC COUNTERS");

    /* we only aim to fix the value of closed_connections */
    pthread_mutex_lock(&mutex_sched_init);
    sched = mk_sched_get_thread_conf();

    if (sched->closed_connections > sched->accepted_connections) {
        /* Count the real number of active connections */
        mk_list_foreach(head, &sched->busy_queue) {
            n++;
        }

        /*
         * At this point we have N active connections in the busy
         * queue, lets assume that we need to close N to reach the number
         * of accepted connections.
         */
        sched->accepted_connections = n;
        sched->closed_connections   = 0;
    }
    pthread_mutex_unlock(&mutex_sched_init);

    return 0;
}

/*
 * Assign a new incomming connection to a specific worker thread, this call comes
 * from the main monkey process.
 */
int mk_sched_add_client(int remote_fd)
{
    int r, t=0;
    struct sched_list_node *sched;

    /* Next worker target */
    t = _next_target();

    if (mk_unlikely(t == -1)) {
        MK_TRACE("[FD %i] Over Capacity, drop!", remote_fd);
        MK_LT_SCHED(remote_fd, "OVER_CAPACITY");
        return -1;
    }

    sched = &sched_list[t];

    MK_TRACE("[FD %i] Balance to WID %i", remote_fd, sched->idx);

    r  = mk_epoll_add(sched->epoll_fd, remote_fd, MK_EPOLL_WRITE,
                      MK_EPOLL_LEVEL_TRIGGERED);

    /* If epoll has failed, decrement the active connections counter */
    if (mk_likely(r == 0)) {
        sched->accepted_connections++;
    }

    MK_LT_SCHED(remote_fd, "ADD_CLIENT");
    return r;
}

/*
 * Register a new client connection into the scheduler, this call takes place
 * inside the worker/thread context.
 */
int mk_sched_register_client(int remote_fd, struct sched_list_node *sched)
{
    int ret;
    struct sched_connection *sched_conn;
    struct mk_list *av_queue = &sched->av_queue;

    sched_conn = mk_list_entry_first(av_queue, struct sched_connection, _head);

    /* Before to continue, we need to run plugin stage 10 */
    ret = mk_plugin_stage_run(MK_PLUGIN_STAGE_10,
                              remote_fd,
                              sched_conn, NULL, NULL);

    /* Close connection, otherwise continue */
    if (ret == MK_PLUGIN_RET_CLOSE_CONX) {
        mk_conn_close(remote_fd, MK_EP_SOCKET_CLOSED);
        MK_LT_SCHED(remote_fd, "PLUGIN_CLOSE");
        return -1;
    }

    /* Socket and status */
    sched_conn->socket = remote_fd;
    sched_conn->status = MK_SCHEDULER_CONN_PENDING;
    sched_conn->arrive_time = log_current_utime;

    /* Register the entry in the red-black tree queue for fast lookup */
    struct rb_node **new = &(sched->rb_queue.rb_node);
    struct rb_node *parent = NULL;

    /* Figure out where to put new node */
    while (*new) {
        struct sched_connection *this = container_of(*new, struct sched_connection, _rb_head);

        parent = *new;
        if (sched_conn->socket < this->socket)
            new = &((*new)->rb_left);
        else if (sched_conn->socket > this->socket)
            new = &((*new)->rb_right);
        else {
            break;
        }
    }

    /* Add new node and rebalance tree. */
    rb_link_node(&sched_conn->_rb_head, parent, new);
    rb_insert_color(&sched_conn->_rb_head, &sched->rb_queue);

    /* Move to busy queue */
    mk_list_del(&sched_conn->_head);
    mk_list_add(&sched_conn->_head, &sched->busy_queue);

    MK_LT_SCHED(remote_fd, "REGISTERED");

    return 0;
}

static void mk_sched_thread_lists_init()
{
    /* client_session mk_list */
    cs_list = mk_mem_malloc_z(sizeof(struct rb_root));
}

/* Register thread information. The caller thread is the thread information's owner */
static int mk_sched_register_thread(int efd)
{
    unsigned int i;
    struct sched_connection *sched_conn, *array;
    struct sched_list_node *sl;
    static int wid = 0;

    /*
     * If this thread slept inside this section, some other thread may touch wid.
     * So protect it with a mutex, only one thread may handle wid.
     */
    pthread_mutex_lock(&mutex_sched_init);

    sl = &sched_list[wid];
    sl->idx = wid++;
    sl->tid = pthread_self();

    /*
     * Under Linux does not exists the difference between process and
     * threads, everything is a thread in the kernel task struct, and each
     * one has it's own numerical identificator: PID .
     *
     * Here we want to know what's the PID associated to this running
     * task (which is different from parent Monkey PID), it can be
     * retrieved with gettid() but Glibc does not export to userspace
     * the syscall, we need to call it directly through syscall(2).
     */
    sl->pid = syscall(__NR_gettid);
    sl->epoll_fd = efd;

    pthread_mutex_unlock(&mutex_sched_init);

    /* Initialize lists */
    sl->rb_queue = RB_ROOT;
    mk_list_init(&sl->busy_queue);
    mk_list_init(&sl->av_queue);

    /* Start filling the array */
    array = mk_mem_malloc_z(sizeof(struct sched_connection) * config->worker_capacity);
    for (i = 0; i < config->worker_capacity; i++) {
        sched_conn = &array[i];
        sched_conn->status = MK_SCHEDULER_CONN_AVAILABLE;
        sched_conn->socket = -1;
        sched_conn->arrive_time = 0;
        mk_list_add(&sched_conn->_head, &sl->av_queue);
    }
    sl->request_handler = NULL;

    return sl->idx;
}

/* created thread, all this calls are in the thread context */
void *mk_sched_launch_worker_loop(void *thread_conf)
{
    int ret;
    char *thread_name = 0;
    unsigned long len;
    sched_thread_conf *thconf = thread_conf;
    int wid, epoll_max_events = thconf->epoll_max_events;
    struct sched_list_node *thinfo = NULL;
    struct epoll_event event = {0, {0}};

#ifndef SHAREDLIB
    /* Avoid SIGPIPE signals */
    mk_signal_thread_sigpipe_safe();
#endif

    /* Init specific thread cache */
    mk_sched_thread_lists_init();
    mk_cache_thread_init();

    /* Register working thread */
    wid = mk_sched_register_thread(thconf->epoll_fd);

    /* Plugin thread context calls */
    mk_epoll_state_init();
    mk_plugin_event_init_list();

    thinfo = &sched_list[wid];

    /*
     * Register the scheduler channel to signal active
     * workers, we do this directly here to avoid to have
     * an entry on the epoll_states.
     */
    thinfo->signal_channel = eventfd(0, 0);
    event.data.fd = thinfo->signal_channel;
    event.events  = EPOLLIN;
    ret = epoll_ctl(thinfo->epoll_fd, EPOLL_CTL_ADD, thinfo->signal_channel, &event);
    if (ret != 0) {
        mk_libc_error("epoll_ctl");
        exit(EXIT_FAILURE);
    }

    /*
     * ULONG_MAX BUG test only
     * =======================
     * to test the workaround we can use the following value:
     *
     *  thinfo->closed_connections = 1000;
     */

#ifdef SHAREDLIB
    thinfo->ctx = thconf->ctx;
#endif

    mk_mem_free(thread_conf);

    /* Rename worker */
    mk_string_build(&thread_name, &len, "monkey: wrk/%i", thinfo->idx);
    mk_utils_worker_rename(thread_name);
    mk_mem_free(thread_name);

    /* Export known scheduler node to context thread */
    pthread_setspecific(worker_sched_node, (void *) thinfo);
    mk_plugin_core_thread();

    __builtin_prefetch(thinfo);
    __builtin_prefetch(&worker_sched_node);

    /* Init epoll_wait() loop */
    mk_epoll_init(thinfo->epoll_fd, epoll_max_events);

    return 0;
}

/*
 * Create thread which will be listening
 * for incomings file descriptors
 */
int mk_sched_launch_thread(int max_events, pthread_t *tout, mklib_ctx ctx UNUSED_PARAM)
{
    int efd;
    pthread_t tid;
    pthread_attr_t attr;
    sched_thread_conf *thconf;

    /* Creating epoll file descriptor */
    efd = mk_epoll_create();
    if (efd < 1) {
        return -1;
    }

    /* Thread data */
    thconf = mk_mem_malloc_z(sizeof(sched_thread_conf));
    thconf->epoll_fd = efd;
    thconf->epoll_max_events = (max_events * 2);
    thconf->max_events = max_events;
#ifdef SHAREDLIB
    thconf->ctx = ctx;
#endif

    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    if (pthread_create(&tid, &attr, mk_sched_launch_worker_loop,
                       (void *) thconf) != 0) {
        mk_libc_error("pthread_create");
        return -1;
    }

    *tout = tid;

    return 0;
}

/*
 * The scheduler nodes are an array of struct sched_list_node type,
 * each worker thread belongs to a scheduler node, on this function we
 * allocate a scheduler node per number of workers defined.
 */
void mk_sched_init()
{
    sched_list = mk_mem_malloc_z(sizeof(struct sched_list_node) *
                                 config->workers);
}

void mk_sched_set_request_list(struct rb_root *list)
{
    cs_list = list;
}

int mk_sched_remove_client(struct sched_list_node *sched, int remote_fd)
{
    struct sched_connection *sc;

    /*
     * Close socket and change status: we must invoke mk_epoll_del()
     * because when the socket is closed is cleaned from the queue by
     * the Kernel at its leisure, and we may get false events if we rely
     * on that.
     */
    mk_epoll_del(sched->epoll_fd, remote_fd);

    sc = mk_sched_get_connection(sched, remote_fd);
    if (sc) {
        MK_TRACE("[FD %i] Scheduler remove", remote_fd);

        /* Invoke plugins in stage 50 */
        mk_plugin_stage_run(MK_PLUGIN_STAGE_50, remote_fd, NULL, NULL, NULL);
        sched->closed_connections++;

        /* Change node status */
        sc->status = MK_SCHEDULER_CONN_AVAILABLE;
        sc->socket = -1;

        /* Unlink from the red-black tree */
        rb_erase(&sc->_rb_head, &sched->rb_queue);

        /* Unlink from busy queue and put it in available queue again */
        mk_list_del(&sc->_head);
        mk_list_add(&sc->_head, &sched->av_queue);

        /* Only close if this was our connection.
         *
         * This has to happen _after_ the busy list removal,
         * otherwise we could get a new client accept()ed with
         * the same FD before we do the removal from the busy list,
         * causing ghosts.
         */
        mk_socket_close(remote_fd);
        MK_LT_SCHED(remote_fd, "DELETE_CLIENT");
        return 0;
    }
    else {
        MK_TRACE("[FD %i] Not found", remote_fd);
        MK_LT_SCHED(remote_fd, "DELETE_NOT_FOUND");
    }
    return -1;
}

struct sched_connection *mk_sched_get_connection(struct sched_list_node *sched,
                                                 int remote_fd)
{
    struct rb_node *node;
    struct sched_connection *this;

    /*
     * In some cases the sched node can be NULL when is a premature close,
     * an example of this situation is when the function mk_sched_add_client()
     * close an incoming connection when invoking the MK_PLUGIN_STAGE_10 stage plugin,
     * so no thread context exists.
     */
    if (!sched) {
        MK_TRACE("[FD %i] No scheduler information", remote_fd);
        mk_socket_close(remote_fd);
        return NULL;
    }

  	node = sched->rb_queue.rb_node;
  	while (node) {
  		this = container_of(node, struct sched_connection, _rb_head);
		if (remote_fd < this->socket)
  			node = node->rb_left;
		else if (remote_fd > this->socket)
  			node = node->rb_right;
		else {
            MK_LT_SCHED(remote_fd, "GET_CONNECTION");
  			return this;
        }
	}

    MK_TRACE("[FD %i] not found in scheduler list", remote_fd);
    MK_LT_SCHED(remote_fd, "GET_FAILED");
    return NULL;
}

int mk_sched_check_timeouts(struct sched_list_node *sched)
{
    int client_timeout;
    struct client_session *cs_node;
    struct sched_connection *entry_conn;
    struct mk_list *sched_head, *temp;

    /* PENDING CONN TIMEOUT */
    mk_list_foreach_safe(sched_head, temp, &sched->busy_queue) {
        entry_conn = mk_list_entry(sched_head, struct sched_connection, _head);
        if (entry_conn->status == MK_SCHEDULER_CONN_PENDING) {
            client_timeout = entry_conn->arrive_time + config->timeout;

            /* Check timeout */
            if (client_timeout <= log_current_utime) {
                MK_TRACE("Scheduler, closing fd %i due TIMEOUT", entry_conn->socket);
                MK_LT_SCHED(entry_conn->socket, "TIMEOUT_CONN_PENDING");
                mk_sched_remove_client(sched, entry_conn->socket);
            }
        }
    }

    /* PROCESSING CONN TIMEOUT */
    struct rb_node *node;

    for (node = rb_first(cs_list); node; node = rb_next(node)) {
        cs_node = rb_entry(node, struct client_session, _rb_head);
        if (cs_node->status == MK_REQUEST_STATUS_INCOMPLETE) {
            if (cs_node->counter_connections == 0) {
                client_timeout = cs_node->init_time + config->timeout;
            }
            else {
                client_timeout = cs_node->init_time + config->keep_alive_timeout;
            }

            /* Check timeout */
            if (client_timeout <= log_current_utime) {
                MK_TRACE("[FD %i] Scheduler, closing due to timeout (incomplete)",
                         cs_node->socket);
                MK_LT_SCHED(cs_node->socket, "TIMEOUT_REQ_INCOMPLETE");
                mk_sched_remove_client(sched, cs_node->socket);
                mk_session_remove(cs_node->socket);

                /* This removal invalidated our iterator. Start over from the beginning. */
                node = rb_first(cs_list);
                if (!node) break;
            }
        }
    }

    return 0;
}


int mk_sched_update_conn_status(struct sched_list_node *sched,
                                int remote_fd, int status)
{
    struct sched_connection *sched_conn;

    if (mk_unlikely(!sched)) {
        return -1;
    }

    sched_conn = mk_sched_get_connection(sched, remote_fd);
    mk_bug(!sched_conn);
    sched_conn->status = status;

    return 0;
}

struct sched_list_node *mk_sched_worker_info()
{
    return pthread_getspecific(worker_sched_node);
}
