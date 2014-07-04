/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Duda I/O
 *  --------
 *  Copyright (C) 2012-2014, Eduardo Silva P. <edsiper@gmail.com>.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

#include <unistd.h>
#include <sys/ioctl.h>
#include "websocket.h"
#include "broadcast.h"

/*
 * Brodcast interface
 * ------------------
 * We work on top of a threaded HTTP stack, hopefully is lock-free and we don't
 * want to use mutual exclusion to synchronize the access to the global data at
 * thread scope to send the message to each client balanced among different
 * workers.
 *
 * We use a proxy-interface based on kernel pipes to distribute the message to
 * each broadcast worker which later distribute the message to all worker
 * clients. After the message have been sent through the pipe the caller returns
 * pretty quickly, in the meanwhile the broadcast workers are distributing the
 * messages to each client.
 *
 *                       +-------------------------+
 *                       | websocket->broadcast(); |
 *                       +-------------------------+
 *                                   |
 *                         +--------------------+
 *                         |  broadcaster proxy |
 *                         +--------------------+
 *                                   |
 *                                   |  <-- dispatch the message to each pipe
 *                                   |
 *             +---------------------+-----------------------+
 *             |                     |                       |
 *        +---------+           +---------+             +---------+
 *        | pipe #1 |           | pipe #2 |             | pipe #N |
 *        +---------+           +---------+             +---------+
 *             |                     |                       |
 *   +--------------------+  +--------------------+  +--------------------+
 *   | broadcast worker 1 |  | broadcast worker 2 |  | broadcast worker N |
 *   +--------------------+  +--------------------+  +--------------------+
 *             |                     |                       |
 *     +-----------------+     +-----------------+     +-----------------+
 *     |  |  |  |  |  |  |     |  |  |  |  |  |  |     |  |  |  |  |  |  |
 *     C  C  C  C  C  C  C     C  C  C  C  C  C  C     C  C  C  C  C  C  C
 *
 * Each end-point named 'C' represents a websocket client session.
 *
 */

/*
 * @METHOD_NAME: broadcaster
 * @METHOD_DESC: Initialize the websocket broadcaster service and interfaces. This method
 * must be invoked inside duda_main().
 * @METHOD_PROTO: int broadcaster()
 * @METHOD_RETURN: On success it returns 0, on error this method perform an explicit exit.
 */

/* Initialize the websocket broadcaster interface */
int ws_broadcaster()
{
    int i;
    int ret;
    struct ws_broadcast_t *br;

    /* Initialize mutex */
    pthread_mutex_init(&ws_spawn_mutex, NULL);

    /* generic counter */
    ws_broadcast_count = 0;

    /* enable broadcast flag */
    ws_config->is_broadcast = MK_TRUE;

    /* Initiallize list of channels */
    mk_list_init(&ws_broadcast_channels);

    /* For each Monkey worker, register a broadcast node */
    for (i = 0; i < monkey->config->workers; i++) {
        br = monkey->mem_alloc(sizeof(struct ws_broadcast_t));
        br->wid = i;
        ret = pipe(br->pipe);

        if (ret != 0) {
            printf("WebSocket: Could not create broadcast pipe on wid %i\n", i);
            exit(EXIT_FAILURE);
        }
        mk_list_add(&br->_head, &ws_broadcast_channels);
    }
    return 0;
}

/*
 * @METHOD_NAME: broadcast
 * @METHOD_DESC: It sends a message in broadcast mode to all active connections registered under a specific channel.
 * This method is intended to be used inside a websocket callback. If you look for a method
 * to broadcast all connection from a worker please review the method broadcast_all().
 * @METHOD_PROTO: int broadcast(ws_request_t *wr, unsigned char *data, uint64_t len, int msg_type, int channel)
 * @METHOD_PARAM: wr the websocket request context struct
 * @METHOD_PARAM: data the data to be send
 * @METHOD_PARAM: msg_type define the message type, it can be WS_OPCODE_TEXT or WS_OPCODE_BINARY.
 * @METHOD_PARAM: channel specify the channel number, use -1 for all.
 * @METHOD_RETURN: This method always returns 0.
 */
int ws_broadcast(ws_request_t *wr, unsigned char *data,
                 uint64_t len, int msg_type, int channel)
{
    int n;
    struct mk_list *head;
    struct ws_broadcast_t *entry;
    struct ws_broadcast_frame br;

    /* internal broadcast frame */
    br.len     = len;
    br.type    = msg_type;
    br.channel = channel;

    if (wr) {
        br.source = wr->socket;
    }
    else {
        br.source = 0;
    }

    memset(br.data, '\0', sizeof(br.data));
    memcpy(br.data, data, len);

    mk_list_foreach(head, &ws_broadcast_channels) {
        entry = mk_list_entry(head, struct ws_broadcast_t, _head);

        /* write the fixed size frame */
        n = write(entry->pipe[1], &br, sizeof(br));
        if (n < 0) {
            printf("Duda/WS: error broadcasting message\n");
        }
    }
    return 0;
}

/*
 * @METHOD_NAME: broadcast_all
 * @METHOD_DESC: It sends a message in broadcast mode to all active connections registered under a specific channel number. This method
 * is intended to be used from a worker.
 * @METHOD_PROTO: int broadcast_all(unsigned char *data, uint64_t len, int msg_type, int channel)
 * @METHOD_PARAM: data the data to be send
 * @METHOD_PARAM: msg_type define the message type, it can be WS_OPCODE_TEXT or WS_OPCODE_BINARY.
 * @METHOD_PARAM: channel specify a specific channel number, use -1 for all.
 * @METHOD_RETURN: This method always returns 0.
 */
int ws_broadcast_all(unsigned char *data, uint64_t len,
                     int msg_type, int channel)
{
    return ws_broadcast(NULL, data, len, msg_type, channel);
}


void ws_broadcast_worker(void *args)
{
    int i, n, fd;
    int n_events = 50;
    int efd;
    int num_fds;
    struct epoll_event event = {0, {0}};
    struct epoll_event *events;
    struct ws_request *wr;
    struct ws_broadcast_t *br = NULL;
    struct ws_broadcast_frame brf;
    struct ws_broadcast_worker *brw = (struct ws_broadcast_worker *) args;
    struct mk_list *head;

    monkey->worker_rename("duda: ws bc/N\n");

    /* Lookup our file descriptor based in the channel number */
    i = 0;
    mk_list_foreach(head, &ws_broadcast_channels) {
        if (i == brw->channel) {
            br = mk_list_entry(head, struct ws_broadcast_t, _head);
            break;
        }
        i++;
    }

    /* Initialize epoll queue and register the file descriptor */
    efd = epoll_create(100);
    event.data.fd = br->pipe[0];
    event.events = EPOLLERR | EPOLLHUP | EPOLLIN;
    events = monkey->mem_alloc(n_events * sizeof(struct epoll_event));
    epoll_ctl(efd, EPOLL_CTL_ADD, br->pipe[0], &event);

    /* loop! */
    while (1) {
        num_fds = epoll_wait(efd, events, n_events, -1);
        for (i = 0; i< num_fds; i++) {
            if (events[i].events & EPOLLIN) {
                fd = events[i].data.fd;

                /* Just read our capacity */
                n = read(fd, &brf, sizeof(brf));
                if (n <= 0) {
                    continue;
                }

                /*
                 * For each websocket request registered in the thread list,
                 * send the websocket message. This is the real broadcast.
                 */
                mk_list_foreach(head, brw->conn_list) {
                    wr = mk_list_entry(head, struct ws_request, _head);

                    /*
                     * Check if the message is destinated to some specific
                     * channel.
                     */
                    if (brf.channel >=0 && wr->channel != brf.channel) {
                        continue;
                    }

                    /* Do not send the message to our selfs (origin) */
                    if (brf.source == wr->socket) {
                        continue;
                    }

                    /* Send message */
                    ws_write(wr, brf.type, brf.data, brf.len);
                }
            }
        }
    }
}
