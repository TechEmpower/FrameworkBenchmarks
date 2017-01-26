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

#include "monkey.h"
#include "mk_http.h"
#include "mk_plugin.h"
#include "mk_macros.h"

int mk_conn_read(int socket)
{
    int ret;
    struct client_session *cs;
    struct sched_list_node *sched;

    MK_TRACE("[FD %i] Connection Handler / read", socket);

    /* Plugin hook */
    ret = mk_plugin_event_read(socket);

    switch(ret) {
    case MK_PLUGIN_RET_EVENT_OWNED:
        return MK_PLUGIN_RET_CONTINUE;
    case MK_PLUGIN_RET_EVENT_CLOSE:
        return -1;
    case MK_PLUGIN_RET_EVENT_CONTINUE:
        break; /* just return controller to invoker */
    }

    sched = mk_sched_get_thread_conf();
    cs = mk_session_get(socket);
    if (!cs) {
        /* Check if is this a new connection for the Scheduler */
        if (!mk_sched_get_connection(sched, socket)) {
            MK_TRACE("[FD %i] Registering new connection");
            if (mk_sched_register_client(socket, sched) == -1) {
                MK_TRACE("[FD %i] Close requested", socket);
                return -1;
            }
            /*
             * New connections are not registered yet into the epoll
             * event state list, we need to do it manually
             */
            mk_epoll_state_set(socket,
                               MK_EPOLL_READ,
                               MK_EPOLL_LEVEL_TRIGGERED,
                               (EPOLLERR | EPOLLHUP | EPOLLRDHUP | EPOLLIN));
            return 0;
        }

        /* Create session for the client */
        MK_TRACE("[FD %i] Create session", socket);
        cs = mk_session_create(socket, sched);
        if (!cs) {
            return -1;
        }
    }

    /* Read incomming data */
    ret = mk_handler_read(socket, cs);
    if (ret > 0) {
        if (mk_http_pending_request(cs) == 0) {
            mk_epoll_change_mode(sched->epoll_fd,
                                 socket, MK_EPOLL_WRITE, MK_EPOLL_LEVEL_TRIGGERED);
        }
        else if (cs->body_length + 1 >= (unsigned int) config->max_request_size) {
            /*
             * Request is incomplete and our buffer is full,
             * close connection
             */
            mk_session_remove(socket);
            return -1;
        }
        else {
            MK_TRACE("[FD %i] waiting for pending data", socket);
        }
    }

    return ret;
}

int mk_conn_write(int socket)
{
    int ret = -1;
    struct client_session *cs;
    struct sched_list_node *sched;
    struct sched_connection *conx;

    MK_TRACE("[FD %i] Connection Handler / write", socket);

    /* Plugin hook */
    ret = mk_plugin_event_write(socket);
    switch(ret) {
    case MK_PLUGIN_RET_EVENT_OWNED:
        return MK_PLUGIN_RET_CONTINUE;
    case MK_PLUGIN_RET_EVENT_CLOSE:
        return -1;
    case MK_PLUGIN_RET_EVENT_CONTINUE:
        break; /* just return controller to invoker */
    }

    MK_TRACE("[FD %i] Normal connection write handling", socket);

    sched = mk_sched_get_thread_conf();
    conx = mk_sched_get_connection(sched, socket);
    if (!conx) {
        MK_TRACE("[FD %i] Registering new connection");
        if (mk_sched_register_client(socket, sched) == -1) {
            MK_TRACE("[FD %i] Close requested", socket);
            return -1;
        }

        mk_epoll_change_mode(sched->epoll_fd, socket,
                             MK_EPOLL_READ, MK_EPOLL_LEVEL_TRIGGERED);
        return 0;
    }

    mk_sched_update_conn_status(sched, socket, MK_SCHEDULER_CONN_PROCESS);

    /* Get node from schedule list node which contains
     * the information regarding to the current client/socket
     */
    cs = mk_session_get(socket);
    if (!cs) {
        /* This is a ghost connection that doesn't exist anymore.
         * Closing it could accidentally close some other thread's
         * socket, so pass it to remove_client that checks it's ours.
         */
        mk_sched_remove_client(sched, socket);
        return 0;
    }

    ret = mk_handler_write(socket, cs);

    /* if ret < 0, means that some error
     * happened in the writer call, in the
     * other hand, 0 means a successful request
     * processed, if ret > 0 means that some data
     * still need to be send.
     */
    if (ret < 0) {
        mk_request_free_list(cs);
        mk_session_remove(socket);
        return -1;
    }
    else if (ret == 0) {
        return mk_http_request_end(socket);
    }
    else if (ret > 0) {
        return 0;
    }

    /* avoid to make gcc cry :_( */
    return -1;
}

int mk_conn_close(int socket, int event)
{
    struct sched_list_node *sched;

    MK_TRACE("[FD %i] Connection Handler, closed", socket);

    /* Plugin hook: this is a wrap-workaround to do not
     * break plugins until the whole interface events and
     * return values are re-worked.
     */
    if (event == MK_EP_SOCKET_CLOSED)
        mk_plugin_event_close(socket);
    else if (event == MK_EP_SOCKET_ERROR) {
        mk_plugin_event_error(socket);
    }
    else if (event == MK_EP_SOCKET_TIMEOUT) {
        mk_plugin_event_timeout(socket);
    }

    sched = mk_sched_get_thread_conf();
    mk_sched_remove_client(sched, socket);

    return 0;
}
