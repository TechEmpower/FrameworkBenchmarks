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

#define _GNU_SOURCE
#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include <sys/time.h>
#include <sys/resource.h>

#include "monkey.h"
#include "mk_config.h"
#include "mk_scheduler.h"
#include "mk_epoll.h"
#include "mk_socket.h"
#include "mk_plugin.h"
#include "mk_utils.h"
#include "mk_macros.h"

/* Return the number of clients that can be attended
 * at the same time per worker thread
 */
unsigned int mk_server_worker_capacity(unsigned short nworkers)
{
    unsigned int max, avl;
    struct rlimit lim;

    /* Limit by system */
    getrlimit(RLIMIT_NOFILE, &lim);
    max = lim.rlim_cur;

    /* Minimum of fds needed by Monkey:
     * --------------------------------
     * 3 fds: stdin, stdout, stderr
     * 1 fd for main socket server
     * 1 fd for epoll array (per thread)
     * 1 fd for worker logger when writing to FS
     * 2 fd for worker logger pipe
     */

    avl = max - (3 + 1 + nworkers + 1 + 2);

    /* The avl is divided by two as we need to consider
     * a possible additional FD for each plugin working
     * on the same request.
     */
    return ((avl / 2) / nworkers);
}

#ifndef SHAREDLIB

/* Here we launch the worker threads to attend clients */
void mk_server_launch_workers()
{
    int i;
    pthread_t skip;

    /* Launch workers */
    for (i = 0; i < config->workers; i++) {
        mk_sched_launch_thread(config->worker_capacity, &skip, NULL);
    }
}

void mk_server_loop(int server_fd)
{
    int ret;
    int remote_fd;

    /* Activate TCP_DEFER_ACCEPT */
    if (mk_socket_set_tcp_defer_accept(server_fd) != 0) {
            mk_warn("TCP_DEFER_ACCEPT failed");
    }

    /* Rename worker */
    mk_utils_worker_rename("monkey: server");

    mk_info("HTTP Server started");

    while (1) {
        remote_fd = mk_socket_accept(server_fd);

        if (mk_unlikely(remote_fd == -1)) {
            continue;
        }

#ifdef TRACE
        MK_TRACE("New connection arrived: FD %i", remote_fd);

        int i;
        struct sched_list_node *node;

        node = sched_list;
        for (i=0; i < config->workers; i++) {
            MK_TRACE("Worker Status");
            MK_TRACE(" WID %i / conx = %llu", node[i].idx, node[i].accepted_connections - node[i].closed_connections);
        }
#endif

        /* Assign socket to worker thread */
        ret = mk_sched_add_client(remote_fd);
        if (ret == -1) {
            mk_socket_close(remote_fd);
        }
    }
}

#endif // !SHAREDLIB
