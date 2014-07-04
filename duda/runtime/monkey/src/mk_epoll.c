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
#include <assert.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/epoll.h>

#include "monkey.h"
#include "mk_connection.h"
#include "mk_socket.h"
#include "mk_clock.h"
#include "mk_request.h"
#include "mk_config.h"
#include "mk_scheduler.h"
#include "mk_epoll.h"
#include "mk_utils.h"
#include "mk_macros.h"
#include "mk_linuxtrace.h"

static __thread struct epoll_state_index mk_epoll_state_k;

/*
 * Initialize the epoll state index per worker thread, every index struct contains
 * a fixed array of epoll_state entries and two mk_list to represent an available and
 * busy queue for each entry
 */
int mk_epoll_state_init()
{
    int i;
    struct epoll_state *es;
    struct epoll_state_index *index = &mk_epoll_state_k;

    memset(index, '\0', sizeof(struct epoll_state_index));
    index->size  = config->worker_capacity;

    index->rb_queue = RB_ROOT;
    mk_list_init(&index->busy_queue);
    mk_list_init(&index->av_queue);

    for (i = 0; i < index->size; i++) {
        es = mk_mem_malloc_z(sizeof(struct epoll_state));
        mk_list_add(&es->_head, &index->av_queue);
    }

    return 0;
}

struct epoll_state *mk_epoll_state_get(int fd)
{
  	struct rb_node *node;
    struct epoll_state *es_entry;
    const struct epoll_state_index *index = &mk_epoll_state_k;

    node = index->rb_queue.rb_node;
  	while (node) {
  		es_entry = container_of(node, struct epoll_state, _rb_head);
		if (fd < es_entry->fd)
  			node = node->rb_left;
		else if (fd > es_entry->fd)
  			node = node->rb_right;
		else {
            MK_LT_EPOLL_STATE(fd, es_entry->mode, "GET");
  			return es_entry;
        }
	}

    MK_LT_EPOLL_STATE(fd, 0, "GET: NOT FOUND");
	return NULL;
}

inline struct epoll_state *mk_epoll_state_set(int fd, uint8_t mode,
                                              unsigned int behavior,
                                              uint32_t events)
{
    int i;
    struct epoll_state_index *index = &mk_epoll_state_k;
    struct epoll_state *es_entry = NULL, *es_tmp;

    /*
     * Lets check if we are in the thread context, if dont, this can be the
     * situation when the file descriptor is new and comes from the parent
     * server loop and is just being assigned to the worker thread
     */
    if (mk_unlikely(index->size <= 0)) {
        return NULL;
    }

    /* Lookup entry */
    es_entry = mk_epoll_state_get(fd);

    /* Add new entry to the list */
    if (!es_entry) {
        /* check if we have available slots */
        if (mk_list_is_empty(&index->av_queue) == 0) {

            /* We need to grow the epoll_states list */
            es_tmp = mk_mem_malloc(sizeof(struct epoll_state) * MK_EPOLL_STATE_INDEX_CHUNK);
            for (i = 0; i < MK_EPOLL_STATE_INDEX_CHUNK; i++) {
                mk_list_add(&es_tmp[i]._head, &index->av_queue);
            }
            MK_TRACE("state index grow from %i to %i\n",
                     index->size, index->size + MK_EPOLL_STATE_INDEX_CHUNK);
            index->size += MK_EPOLL_STATE_INDEX_CHUNK;
        }

        /* New entry */
        es_entry = mk_list_entry_first(&index->av_queue, struct epoll_state, _head);
        es_entry->fd       = fd;
        es_entry->mode     = mode;
        es_entry->behavior = behavior;
        es_entry->events   = events;

        /* Unlink from available queue and link to busy queue */
        mk_list_del(&es_entry->_head);
        mk_list_add(&es_entry->_head, &index->busy_queue);

        /* Red-Black tree insert routine */
        struct rb_node **new = &(index->rb_queue.rb_node);
        struct rb_node *parent = NULL;

        /* Figure out where to put new node */
        while (*new) {
            struct epoll_state *this = container_of(*new, struct epoll_state, _rb_head);

            parent = *new;
            if (es_entry->fd < this->fd)
                new = &((*new)->rb_left);
            else if (es_entry->fd > this->fd)
                new = &((*new)->rb_right);
            else {
                break;
            }
        }

        /* Add new node and rebalance tree. */
        rb_link_node(&es_entry->_rb_head, parent, new);
        rb_insert_color(&es_entry->_rb_head, &index->rb_queue);

        MK_LT_EPOLL_STATE(fd, es_entry->mode, "SET: NEW");
        return es_entry;
    }

    /*
     * Sleep mode: the sleep mode disable the events in the epoll queue so the Kernel
     * will not trigger any events, when mode == MK_EPOLL_SLEEP, the epoll_state events
     * keeps the previous events state which can be used in the MK_EPOLL_WAKEUP routine.
     *
     * So we just touch the events and behavior state fields if mode != MK_EPOLL_SLEEP.
     */
    if (mode != MK_EPOLL_SLEEP) {
        es_entry->events   = events;
        es_entry->behavior = behavior;
    }

    /* Update current mode */
    es_entry->mode = mode;
    MK_LT_EPOLL_STATE(fd, es_entry->mode, "SET: CHANGE");
    return es_entry;
}

static int mk_epoll_state_del(int fd)
{
    struct epoll_state *es_entry;
    struct epoll_state_index *index = &mk_epoll_state_k;

    es_entry = mk_epoll_state_get(fd);
    if (es_entry) {
        rb_erase(&es_entry->_rb_head, &index->rb_queue);
        mk_list_del(&es_entry->_head);
        mk_list_add(&es_entry->_head, &index->av_queue);

        MK_LT_EPOLL_STATE(fd, es_entry->mode, "DELETE");
        return 0;
    }

    MK_LT_EPOLL_STATE(fd, 0, "DELETE: NOT FOUND");
    return -1;
}

int mk_epoll_create()
{
    int efd;

    efd = epoll_create1(EPOLL_CLOEXEC);
    if (efd == -1) {
        mk_libc_error("epoll_create");
    }
    return efd;
}

void *mk_epoll_init(int efd, int max_events)
{
    int i, fd, ret = -1;
    int num_fds;
    int fds_timeout;

    struct epoll_event *events;
    struct sched_list_node *sched;

    /* Get thread conf */
    sched = mk_sched_get_thread_conf();

    fds_timeout = log_current_utime + config->timeout;
    events = mk_mem_malloc_z(max_events * sizeof(struct epoll_event));

    pthread_mutex_lock(&mutex_worker_init);
    sched->initialized = 1;
    pthread_mutex_unlock(&mutex_worker_init);

    while (1) {
        ret = -1;
        num_fds = epoll_wait(efd, events, max_events, MK_EPOLL_WAIT_TIMEOUT);

        for (i = 0; i < num_fds; i++) {
            fd = events[i].data.fd;

            if (events[i].events & EPOLLIN) {
                MK_LT_EPOLL(fd, "EPOLLIN");
                MK_TRACE("[FD %i] EPoll Event READ", fd);

                if (mk_unlikely(fd == sched->signal_channel)) {
                    uint64_t val = 0;
                    ret = read(fd, &val, sizeof(val));
                    if (ret > 0) {
                        if (val == MK_SCHEDULER_SIGNAL_DEADBEEF) {
                            mk_sched_sync_counters();
                            continue;
                        }
                    }
                }

                ret = mk_conn_read(fd);
            }
            else if (events[i].events & EPOLLOUT) {
                MK_LT_EPOLL(fd, "EPOLLOUT");
                MK_TRACE("[FD %i] EPoll Event WRITE", fd);

                ret = mk_conn_write(fd);
            }
            else if (events[i].events & (EPOLLHUP | EPOLLERR | EPOLLRDHUP)) {
#ifdef LINUX_TRACE
                if (events[i].events & (EPOLLHUP))   MK_LT_EPOLL(fd, "EPOLLHUP");
                if (events[i].events & (EPOLLERR))   MK_LT_EPOLL(fd, "EPOLLERR");
                if (events[i].events & (EPOLLRDHUP)) MK_LT_EPOLL(fd, "EPOLLRDHUP");
#endif
                MK_TRACE("[FD %i] EPoll Event EPOLLHUP/EPOLLER", fd);
                mk_conn_close(fd, MK_EP_SOCKET_CLOSED);
                ret = 0;
            }

            if (ret < 0) {
                MK_LT_EPOLL(fd, "FORCED CLOSE");
                MK_TRACE("[FD %i] Epoll Event FORCE CLOSE | ret = %i", fd, ret);
                mk_conn_close(fd, MK_EP_SOCKET_CLOSED);
            }
        }

        /* Check timeouts and update next one */
        if (log_current_utime >= fds_timeout) {
            MK_LT_EPOLL(0, "TIMEOUT CHECK");
            mk_sched_check_timeouts(sched);
            fds_timeout = log_current_utime + config->timeout;
        }
    }

    return NULL;
}

int mk_epoll_add(int efd, int fd, int init_mode, unsigned int behavior)
{
    int ret;
    struct epoll_event event = {0, {0}};

    event.data.fd = fd;
    event.events = EPOLLERR | EPOLLHUP | EPOLLRDHUP;

    if (behavior == MK_EPOLL_EDGE_TRIGGERED) {
        event.events |= EPOLLET;
    }

    switch (init_mode) {
    case MK_EPOLL_READ:
        event.events |= EPOLLIN;
        break;
    case MK_EPOLL_WRITE:
        event.events |= EPOLLOUT;
        break;
    case MK_EPOLL_RW:
        event.events |= EPOLLIN | EPOLLOUT;
        break;
    case MK_EPOLL_SLEEP:
        event.events = 0;
        break;
    }

    /* Add to epoll queue */
    ret = epoll_ctl(efd, EPOLL_CTL_ADD, fd, &event);
    if (mk_unlikely(ret < 0 && errno != EEXIST)) {
        MK_TRACE("[FD %i] epoll_ctl() %s", fd, strerror(errno));
        return ret;
    }

    /* Add to event state list */
    mk_epoll_state_set(fd, init_mode, behavior, event.events);

    return ret;
}

int mk_epoll_del(int efd, int fd)
{
    int ret;

    ret = epoll_ctl(efd, EPOLL_CTL_DEL, fd, NULL);
    MK_TRACE("[FD %i] Epoll, remove from QUEUE_FD=%i", fd, efd);

#ifdef TRACE
    if (ret < 0) {
        mk_libc_error("epoll_ctl");
        MK_TRACE("[FD %i] epoll_ctl() = %i", fd, ret);
    }
#endif

    /* remove epoll state */
    mk_epoll_state_del(fd);

    return ret;
}

int mk_epoll_change_mode(int efd, int fd, int mode, unsigned int behavior)
{
    int ret;
    struct epoll_event event = {0, {0}};
    struct epoll_state *state;

    event.events = EPOLLERR | EPOLLHUP | EPOLLRDHUP;
    event.data.fd = fd;

    switch (mode) {
    case MK_EPOLL_READ:
        MK_TRACE("[FD %i] EPoll changing mode to READ", fd);
        event.events |= EPOLLIN;
        break;
    case MK_EPOLL_WRITE:
        MK_TRACE("[FD %i] EPoll changing mode to WRITE", fd);
        event.events |= EPOLLOUT;
        break;
    case MK_EPOLL_RW:
        MK_TRACE("[FD %i] Epoll changing mode to READ/WRITE", fd);
        event.events |= EPOLLIN | EPOLLOUT;
        break;
    case MK_EPOLL_SLEEP:
        MK_TRACE("[FD %i] Epoll changing mode to DISABLE", fd);
        event.events = 0;
        break;
    case MK_EPOLL_WAKEUP:
        state = mk_epoll_state_get(fd);
        if (!state) {
            mk_warn("[FD %i] MK_EPOLL_WAKEUP error, invalid connection",
                    fd);
            return -1;
        }
        else if (state->mode == MK_EPOLL_SLEEP) {
            event.events = state->events;
            behavior     = state->behavior;
        }
        else {
            mk_warn("[FD %i] MK_EPOLL_WAKEUP error, current mode is %i",
                    fd, state->mode);
            return -1;
        }
        break;
    }

    if (behavior == MK_EPOLL_EDGE_TRIGGERED) {
        event.events |= EPOLLET;
    }

    /* Update epoll fd events */
    ret = epoll_ctl(efd, EPOLL_CTL_MOD, fd, &event);
#ifdef TRACE
    if (ret < 0) {
        mk_libc_error("epoll_ctl");
        MK_TRACE("[FD %i] epoll_ctl() = %i", fd, ret);
    }
#endif

    /* Update state */
    mk_epoll_state_set(fd, mode, behavior, event.events);
    return ret;
}
