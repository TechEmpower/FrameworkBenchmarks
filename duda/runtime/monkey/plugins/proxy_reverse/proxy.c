/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Monkey HTTP Daemon
 *  ------------------
 *  Copyright (C) 2013, Nikola Nikov <nikola.h.nikov@gmail.com>
 *  Copyright (C) 2013, Eduardo Silva <edsiper@gmail.com>
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *  MA 02110-1301  USA.
 */

#include <fcntl.h>
#include <regex.h>
#include <sys/socket.h>
#include <time.h>
#include <unistd.h>

#include "types.h"
#include "config.h"
#include "balancer.h"

#define RESPONSE_BUFFER_MIN 4096
#define RESPONSE_BUFFER_MAX 65536

/* https://lotrax.org/gsoc/two-common-monkey-plugin-patterns.html */

MONKEY_PLUGIN("proxy_reverse", "Reverse Proxy", "0.3",
              MK_PLUGIN_STAGE_30 | MK_PLUGIN_CORE_THCTX);

#include <stdio.h>

struct proxy_context
{
    struct dict client, slave;
};
static pthread_key_t proxy_key;

struct plugin_api *mk_api;

struct proxy_peer
{
    int fd_client, fd_slave;
    int mode_client, mode_slave;
    struct session_request *sr;
    size_t request_index;
    struct
    {
        char *buffer;
        size_t size, index, total;
    } response;
    void *connection;
    int slave;                  /* whether the slave is connected */
};

static int log;

static struct proxy_entry_array *proxy_config;

const mk_pointer mk_proxy_default_mime = mk_pointer_init("text/html\r\n");

static int response_buffer_adjust(struct proxy_peer *peer, size_t size)
{

    if (size > RESPONSE_BUFFER_MAX) {
        return 0;
    }

    if (size < RESPONSE_BUFFER_MIN) {
        size = RESPONSE_BUFFER_MIN;
    }
    else {
        size = (size + RESPONSE_BUFFER_MIN - 1) & ~(RESPONSE_BUFFER_MIN - 1);
    }

    if (!peer->response.size) {
        peer->response.buffer = mk_api->mem_alloc(size);
        if (!peer) {
            return 0;
        }
    }
    else if (peer->response.size < size) {
        /* Make sure the data is at the beginning of the buffer. */
        if (peer->response.index) {
            size_t available = peer->response.total - peer->response.index;
            if (available) {
                memmove(peer->response.buffer,
                        peer->response.buffer + peer->response.index,
                        available);
            }
            peer->response.total = available;
            peer->response.index = 0;
        }

        peer->response.buffer =
            mk_api->mem_realloc(peer->response.buffer, size);
        if (!peer) {
            return 0;
        }
    }
    else {
        return 1;
    }

    peer->response.size = size;

    return 1;
}

static int slave_connect(struct proxy_peer *peer,
                         struct proxy_entry_array *proxy_config, int sock)
{
    const struct session_request *sr = peer->sr;
    char *string = mk_api->mem_alloc(sr->uri_processed.len + 1);
    if (!string) {
        return -1;
    }
    memcpy(string, sr->uri_processed.data, sr->uri_processed.len);
    string[sr->uri_processed.len] = 0;

    MK_TRACE("Balancer for %s", string);

    struct proxy_entry *match = proxy_check_match(string, proxy_config);
    mk_api->mem_free(string);
    if (!match) {
        return -1;
    }

    peer->connection = 0;
    peer->slave = 1;

    /* Invoke the appropriate balancer based on the configuration */
    switch (match->balancer_type) {
    case Naive:
        MK_TRACE("Balance: \"Naive\"");
        return proxy_balance_naive(match->server_list, time(0));
    case FirstAlive:
        MK_TRACE("Balance: \"First Alive\"");
        return proxy_balance_firstalive(match->server_list);
    case SourceHash:
        MK_TRACE("Balance: \"Source Hash\"");
        return proxy_balance_hash(match->server_list, sock);
    case RoundRobin:
        MK_TRACE("Balance: \"Round Robin\"");
        return proxy_balance_rr_lockless(match->server_list);
    case LockingRoundRobin:
        MK_TRACE("Balance: \"Locking Round Robin\"");
        return proxy_balance_rr_locking(match->server_list);
    case LeastConnections:
        MK_TRACE("Balance: \"Least Connections\"");
        return proxy_balance_leastconnections(match->server_list,
                                              &peer->connection);
    default:
        MK_TRACE("Invalid balancer");
        return -1;
    }
}

static void slave_disconnect(struct proxy_peer *peer)
{
    peer->slave = 0;

    if (peer->connection) {
        proxy_balance_close(peer->connection);
        peer->connection = 0;
    }
}

static int proxy_peer_add(struct dict *dict, int fd, struct proxy_peer *peer)
{
    struct string key = string((char *) &fd, sizeof(int));
    return dict_add(dict, &key, peer);
}

static struct proxy_peer *proxy_peer_get(struct dict *dict, int fd)
{
    struct string key = string((char *) &fd, sizeof(fd));
    return dict_get(dict, &key);
}

static struct proxy_peer *proxy_peer_remove(struct dict *dict, int fd)
{
    struct string key = string((char *) &fd, sizeof(fd));
    return dict_remove(dict, &key);
}

static int proxy_close(int fd)
{
    int connected;
    struct proxy_context *context = pthread_getspecific(proxy_key);

    struct proxy_peer *peer = proxy_peer_remove(&context->slave, fd);
    if (!peer) {
        peer = proxy_peer_remove(&context->client, fd);
        if (!peer) {
            return MK_PLUGIN_RET_EVENT_CONTINUE;        /* nothing to do */
        }

    }

    connected = peer->slave;
    slave_disconnect(peer);

    if (fd == peer->fd_client) {        /* the event is for the client socket */
        if (connected) {        /* the slave is still connected */
            proxy_peer_remove(&context->slave, peer->fd_slave);
            mk_api->event_del(peer->fd_slave);
        }

        MK_TRACE("[FD %i] Closing client socket", fd);

        /* avoid TIME_WAIT by sending RST to the slave */
        struct linger linger;
        linger.l_onoff = 1;
        linger.l_linger = 0;
        setsockopt(peer->fd_slave, SOL_SOCKET, SO_LINGER, &linger,
                   sizeof(linger));
        mk_api->socket_close(peer->fd_slave);

        mk_api->event_del(peer->fd_client);
    }
    else {                      /* the event is for the slave socket */

        MK_TRACE("[FD %i] Disabling slave socket", socket);

        mk_api->event_del(peer->fd_slave);
        return MK_PLUGIN_RET_EVENT_OWNED;
    }

    mk_api->mem_free(peer->response.buffer);
    mk_api->mem_free(peer);

    return MK_PLUGIN_RET_EVENT_OWNED;
    //return MK_PLUGIN_RET_EVENT_CLOSE;
}

int _mkp_init(struct plugin_api **api, char *confdir)
{
    mk_api = *api;

    pthread_key_create(&proxy_key, 0);

    proxy_config = proxy_reverse_read_config(confdir);
    if (!proxy_config->length) {
        return -1;
    }

    if (proxy_balance_init(proxy_config) < 0) {
        return -1;
    }

    return 0;
}

/*int _mkp_core_prctx(struct server_config *config)
{
    return 0;
}*/

void _mkp_core_thctx(void)
{
    struct proxy_context *context =
        mk_api->mem_alloc(sizeof(struct proxy_context));
    if (!context) {
        mk_err("ProxyReverse: Failed to allocate proxy reverse context.");
        abort();
    }

    if (!dict_init(&context->client, 4)) {
        return;
    }
    if (!dict_init(&context->slave, 4)) {
        return;
    }

    pthread_setspecific(proxy_key, context);
}

void _mkp_exit(void)
{
    close(log);
}

int _mkp_stage_30(struct plugin *plugin, struct client_session *cs,
                  struct session_request *sr)
{
    struct string *html_stats;
    struct proxy_context *context = pthread_getspecific(proxy_key);

    (void) plugin;

    struct proxy_peer *peer = proxy_peer_get(&context->client, cs->socket);
    if (peer) {
        /* Non-first request on a keep-alive connection. */

        MK_TRACE("[FD %i] New request", cs->socket);

        peer->request_index = 0;

        peer->mode_client = MK_EPOLL_SLEEP;
        peer->mode_slave = MK_EPOLL_WRITE;
        peer->sr = sr;

        mk_api->event_socket_change_mode(peer->fd_client, peer->mode_client,
                                         MK_EPOLL_LEVEL_TRIGGERED);
        mk_api->event_socket_change_mode(peer->fd_slave, peer->mode_slave,
                                         MK_EPOLL_LEVEL_TRIGGERED);

        peer->response.index = 0;
        peer->response.total = 0;
    }
    else {
        /* First request for this connection. */

        MK_TRACE("[FD %i] New connection", cs->socket);

        /* check for statistics request */
        html_stats = proxy_balance_generate_statistics(sr);
        if (html_stats) {
            mk_api->header_set_http_status(sr, MK_HTTP_OK);

            sr->headers.content_length = html_stats->length;
            sr->headers.content_type = mk_proxy_default_mime;

            mk_api->header_send(cs->socket, cs, sr);

            mk_api->socket_send(cs->socket, html_stats->data,
                                html_stats->length);

            mk_api->mem_free(html_stats->data);
            mk_api->mem_free(html_stats);

            return MK_PLUGIN_RET_END;
        }

        peer = mk_api->mem_alloc(sizeof(struct proxy_peer));
        if (!peer) {
            return MK_PLUGIN_RET_CLOSE_CONX;
        }

        peer->sr = sr;
        peer->fd_client = cs->socket;
        peer->fd_slave = slave_connect(peer, proxy_config, cs->socket);
        if (peer->fd_slave < 0) {
            mk_api->mem_free(peer);
            return MK_PLUGIN_RET_CLOSE_CONX;    // TODO
        }

        MK_TRACE("[FD %i] Created slave socket", peer->fd_slave);

        peer->mode_client = MK_EPOLL_SLEEP;
        peer->mode_slave = MK_EPOLL_WRITE;

        peer->request_index = 0;

        mk_api->event_socket_change_mode(peer->fd_client, peer->mode_client,
                                         MK_EPOLL_LEVEL_TRIGGERED);
        mk_api->event_add(peer->fd_slave, peer->mode_slave, 0,
                          MK_EPOLL_LEVEL_TRIGGERED);

        peer->response.buffer = 0;
        peer->response.size = 0;
        peer->response.index = 0;
        peer->response.total = 0;
        response_buffer_adjust(peer, RESPONSE_BUFFER_MIN);

        if (proxy_peer_add(&context->client, cs->socket, peer)) {
            return MK_PLUGIN_RET_CLOSE_CONX;
        }
        if (proxy_peer_add(&context->slave, peer->fd_slave, peer)) {
            return MK_PLUGIN_RET_CLOSE_CONX;
        }
    }

    return MK_PLUGIN_RET_CONTINUE;
}

int _mkp_event_read(int socket)
{
    struct proxy_context *context = pthread_getspecific(proxy_key);
    struct proxy_peer *peer;

    peer = proxy_peer_get(&context->slave, socket);
    if (peer) {
        /* We can read from the slave server. */

        MK_TRACE("[FD %i] Read event on slave socket", socket);

        /*if (!peer->response.total)
           {
           peer->mode_client |= MK_EPOLL_WRITE;
           mk_api->event_socket_change_mode(peer->fd_client, peer->mode_client, MK_EPOLL_LEVEL_TRIGGERED);
           } */

        size_t left = peer->response.size - peer->response.total;
        if (!left) {
            if (!response_buffer_adjust(peer, peer->response.size + 1)) {
                /* Don't poll for reading until we have free space in the buffer. */
                MK_TRACE("[FD %i] Read buffer full", socket);
                //peer->mode_slave &= ~MK_EPOLL_READ;
                //mk_api->event_socket_change_mode(peer->fd_slave, peer->mode_slave, MK_EPOLL_LEVEL_TRIGGERED);
                //return MK_PLUGIN_RET_EVENT_OWNED;
                return MK_PLUGIN_RET_EVENT_NEXT;
            }
            left = peer->response.size - peer->response.total;
        }

        ssize_t size =
            read(peer->fd_slave, peer->response.buffer + peer->response.total,
                 left);
        if (size <= 0) {
            return proxy_close(peer->fd_slave);
        }
        peer->response.total += size;

        // Set response code.
        if ((!peer->sr->headers.status) && (peer->response.total >= 12)) {
            int code = strtol(peer->response.buffer + 9, 0, 10);
            MK_TRACE("[FD %i] Response code = %i", socket, code);
            mk_api->header_set_http_status(peer->sr, code);
        }

        mk_api->event_socket_change_mode(peer->fd_client, MK_EPOLL_WRITE,
                                         MK_EPOLL_LEVEL_TRIGGERED);

        return MK_PLUGIN_RET_EVENT_OWNED;
    }
    else {
        /* We can read from a client. */

        MK_TRACE("[FD %i] Read event on client socket", socket);

        peer = proxy_peer_get(&context->client, socket);
        if (peer) {
            //write(2, "C ->  \n", 7);
            mk_api->event_socket_change_mode(peer->fd_client, MK_EPOLL_RW,
                                             MK_EPOLL_LEVEL_TRIGGERED);
            return MK_PLUGIN_RET_EVENT_NEXT;
        }
        else {
            return MK_PLUGIN_RET_EVENT_NEXT;
        }
    }
}

int _mkp_event_write(int socket)
{
    struct proxy_context *context = pthread_getspecific(proxy_key);
    struct proxy_peer *peer;
    ssize_t size;

    peer = proxy_peer_get(&context->client, socket);
    if (peer) {
        /* We can write to the client. */

        MK_TRACE("[FD %i] Write event on client socket", socket);

        /* Write response to the client. Don't poll for writing if we don't have anything to write. */
        if (peer->response.index < peer->response.total) {
            size =
                write(peer->fd_client,
                      peer->response.buffer + peer->response.index,
                      peer->response.total - peer->response.index);
            if (size < 0) {
                return proxy_close(peer->fd_client);
            }
            peer->response.index += size;

            if (peer->response.index == peer->response.total) {
                MK_TRACE("[FD %i] Response sent", socket);
                if (mk_api->http_request_end(socket)) {
                    return MK_PLUGIN_RET_EVENT_CLOSE;
                }
                return MK_PLUGIN_RET_EVENT_CONTINUE;
            }

            return MK_PLUGIN_RET_EVENT_OWNED;
        }

        MK_TRACE("[FD %i] Nothing to write; Wait for read event", socket);
        mk_api->event_socket_change_mode(peer->fd_client, MK_EPOLL_READ,
                                         MK_EPOLL_LEVEL_TRIGGERED);

        return MK_PLUGIN_RET_EVENT_CONTINUE;

    }
    else {
        peer = proxy_peer_get(&context->slave, socket);
        if (!peer) {
            return MK_PLUGIN_RET_EVENT_NEXT;
        }

        /* We can write to the slave server. */

        MK_TRACE("[FD %i] Write event on slave socket", socket);



        /* Write request to the slave server. */
        size_t total =
            peer->sr->body.len + sizeof("\r\n\r\n") - 1 + peer->sr->data.len;
        if (peer->request_index < total) {
            size =
                write(peer->fd_slave,
                      peer->sr->body.data + peer->request_index,
                      total - peer->request_index);
            if (size < 0) {
                return proxy_close(peer->fd_slave);
            }
            peer->request_index += size;
        }

        if (peer->request_index == total) {
            MK_TRACE("[FD %i] Request sent; Wait for read event", socket);
            mk_api->event_socket_change_mode(peer->fd_slave, MK_EPOLL_READ,
                                             MK_EPOLL_LEVEL_TRIGGERED);
        }

    }

    return MK_PLUGIN_RET_EVENT_OWNED;
}

int _mkp_event_close(int fd)
{
    return proxy_close(fd);
}

int _mkp_event_timeout(int fd)
{
    return proxy_close(fd);
}

int _mkp_event_error(int fd)
{
    return proxy_close(fd);
}
