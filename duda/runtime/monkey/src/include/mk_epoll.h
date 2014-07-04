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

#include <sys/epoll.h>
#include "mk_rbtree.h"

#ifndef MK_EPOLL_H
#define MK_EPOLL_H

/* Epoll States */
#define MK_EPOLL_READ     0
#define MK_EPOLL_WRITE    1
#define MK_EPOLL_RW       2
#define MK_EPOLL_SLEEP    3
#define MK_EPOLL_WAKEUP   4

/* Epoll timeout is 3 seconds */
#define MK_EPOLL_WAIT_TIMEOUT 3000

#define MK_EPOLL_LEVEL_TRIGGERED 2        /* default */
#define MK_EPOLL_EDGE_TRIGGERED  EPOLLET

/*
 * Once a connection is dropped, define
 * a reason.
 */
#define MK_EP_SOCKET_CLOSED   0
#define MK_EP_SOCKET_ERROR    1
#define MK_EP_SOCKET_TIMEOUT  2

/* Just in case RHDUP is not defined */
#ifndef EPOLLRDHUP
#define EPOLLRDHUP 0x2000
#endif

#define MK_EPOLL_STATE_INDEX_CHUNK 64

typedef struct
{
    int (*read) (int);
    int (*write) (int);
    int (*close) (int, int);
} mk_epoll_handlers;

/*
 * An epoll_state represents the state of the descriptor from
 * a Monkey core point of view.
 */
struct epoll_state
{
    int          fd;            /* File descriptor                    */
    uint8_t      mode;          /* Operation mode                     */
    uint32_t     events;        /* Events mask                        */
    unsigned int behavior;      /* Triggered behavior                 */

    struct rb_node _rb_head;
    struct mk_list _head;
};

struct epoll_state_index
{
    int size;

    struct rb_root rb_queue;
    struct mk_list busy_queue;
    struct mk_list av_queue;
};

/* Monkey epoll calls */
int mk_epoll_create();
void *mk_epoll_init(int efd, int max_events);
struct epoll_state *mk_epoll_state_get(int fd);

int mk_epoll_add(int efd, int fd, int mode, unsigned int behavior);
int mk_epoll_del(int efd, int fd);
int mk_epoll_change_mode(int efd, int fd, int mode, unsigned int behavior);

/* epoll state handlers */
struct epoll_state *mk_epoll_state_set(int fd, uint8_t mode,
                                       unsigned int behavior,
                                       uint32_t events);
int mk_epoll_state_init();

#endif
