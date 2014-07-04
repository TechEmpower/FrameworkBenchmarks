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

#include <pthread.h>
#include <stdio.h>

#include "mk_scheduler.h"
#include "duda_package.h"
#include "request.h"
#include "broadcast.h"
#include "websocket.h"

#ifdef PLUGIN_TRACE
#undef PLUGIN_TRACE
#define PLUGIN_TRACE   printf
#endif

/* Create a ws_request node */
struct ws_request *ws_request_create(int socket_fd,
                                     int channel,
                                     struct duda_request *dr,
                                     void (*on_open)   (duda_request_t *, ws_request_t *),
                                     void (*on_message)(duda_request_t *, ws_request_t *),
                                     void (*on_error)  (duda_request_t *, ws_request_t *),
                                     void (*on_close)  (duda_request_t *, ws_request_t *),
                                     void (*on_timeout)(duda_request_t *, ws_request_t *))
{
    struct ws_request *new;

    new = monkey->mem_alloc(sizeof(struct ws_request));
    new->socket = socket_fd;
    new->channel = channel;
    new->dr = dr;
    new->cb_on_open    = on_open;
    new->cb_on_message = on_message;
    new->cb_on_error   = on_error;
    new->cb_on_close   = on_close;
    new->cb_on_timeout = on_timeout;
    new->payload = NULL;
    new->payload_len = 0;

    return new;
}

void ws_request_add(struct ws_request *wr)
{
    /* websocket request list (thread context) */
    struct mk_list *wr_list;

    /* Get thread data */
    wr_list = global->get(ws_request_list);

    /* Add node to list */
    mk_list_add(&wr->_head, wr_list);

    /* Update thread key */
    pthread_setspecific(_mkp_data, wr_list);
}

/*
 * It register the request and connection data, if it doesn't
 * exists it will be create it, otherwise will return the pointer
 * to the ws_request struct node
 */
struct ws_request *ws_request_get(int socket_fd)
{
    struct ws_request *wr_node;
    struct mk_list *wr_list, *wr_head;

    /* Get thread data */
    wr_list = global->get(ws_request_list);

    /* No connection previously was found */
    if (mk_list_is_empty(wr_list) == 0) {
        return NULL;
    }

    mk_list_foreach(wr_head, wr_list) {
        wr_node = mk_list_entry(wr_head, struct ws_request, _head);
        if(wr_node->socket == socket_fd){
            return wr_node;
        }
    }

    return NULL;
}

/*
 * Remove a ws_request from the main list, return 0 on success or -1
 * when for some reason the request was not found
 */
int ws_request_delete(int socket)
{
    struct ws_request *wr_node;
    struct mk_list *wr_list, *wr_temp, *wr_head;

    PLUGIN_TRACE("[FD %i] remove request from list", socket);

    wr_list = global->get(ws_request_list);
    if (mk_list_is_empty(wr_list) == 0) {
        return -1;
    }

    mk_list_foreach_safe(wr_head, wr_temp, wr_list) {
        wr_node = mk_list_entry(wr_head, struct ws_request, _head);

        if (wr_node->socket == socket) {
            mk_list_del(wr_head);
            monkey->mem_free(wr_node->payload);
            monkey->mem_free(wr_node);
            return 0;
        }
    }

    return -1;
}

void *cb_request_list_init()
{
    struct mk_list *list;
    struct ws_broadcast_worker *bw;
    struct sched_list_node *thinfo = NULL;

    /* Initialize the list for each request based in a handshake */
    list = monkey->mem_alloc_z(sizeof(struct mk_list));
    mk_list_init(list);

    /*
     * Before to return the data to setup the global key, we will
     * use this same interface to launch a new thread if the brodcaster have been
     * initiallized from the web service.
     *
     * The technical objective of this routine is to create a new thread
     * per Monkey thread. If Monkey was launched with 5 workers, we aim to launch
     * 5 extra threads here to handle the broadcast buffers in a separate context.
     */
    if (ws_config->is_broadcast == MK_TRUE) {
        /* Critical section */
        pthread_mutex_lock(&ws_spawn_mutex);

        thinfo = monkey->sched_worker_info();

        /* Broadcast worker info */
        bw = monkey->mem_alloc(sizeof(struct ws_broadcast_worker));
        bw->pid = thinfo->tid;
        bw->conn_list = list;
        bw->channel = ws_broadcast_count;

        ws_broadcast_count++;

        /* Unlock mutex */
        pthread_mutex_unlock(&ws_spawn_mutex);

        monkey->worker_spawn(ws_broadcast_worker, bw);
    }

    return list;
}
