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
#include <stdlib.h>
#include <errno.h>
#include <time.h>
#include <arpa/inet.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <sys/sendfile.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

#include "monkey.h"
#include "mk_socket.h"
#include "mk_memory.h"
#include "mk_utils.h"
#include "mk_plugin.h"
#include "mk_macros.h"

static void mk_socket_safe_event_write(int socket)
{
    struct sched_list_node *sched;

    sched = mk_sched_get_thread_conf();
    MK_TRACE("[FD %i] Safe event write ON", socket);
    mk_epoll_change_mode(sched->epoll_fd, socket,
                         MK_EPOLL_WRITE, MK_EPOLL_LEVEL_TRIGGERED);
}

/*
 * Example from:
 * http://www.baus.net/on-tcp_cork
 */
int mk_socket_set_cork_flag(int fd, int state)
{
    MK_TRACE("Socket, set Cork Flag FD %i to %s", fd, (state ? "ON" : "OFF"));

    return setsockopt(fd, SOL_TCP, TCP_CORK, &state, sizeof(state));
}

int mk_socket_set_nonblocking(int sockfd)
{

    MK_TRACE("Socket, set FD %i to non-blocking", sockfd);

    if (fcntl(sockfd, F_SETFL, fcntl(sockfd, F_GETFL, 0) | O_NONBLOCK) == -1) {
        mk_err("Can't set to non-blocking mode socket %i", sockfd);
        return -1;
    }
    fcntl(sockfd, F_SETFD, FD_CLOEXEC);

    return 0;
}

/*
 * Enable the TCP_FASTOPEN feature for server side implemented in Linux Kernel >= 3.7,
 * for more details read here:
 *
 *  TCP Fast Open: expediting web services: http://lwn.net/Articles/508865/
 */

int mk_socket_set_tcp_fastopen(int sockfd)
{
    int qlen = 5;
    return setsockopt(sockfd, SOL_TCP, TCP_FASTOPEN, &qlen, sizeof(qlen));
}

int mk_socket_set_tcp_nodelay(int sockfd)
{
    int on = 1;

    return setsockopt(sockfd, SOL_TCP, TCP_NODELAY, &on, sizeof(on));
}

int mk_socket_set_tcp_defer_accept(int sockfd)
{
    int timeout = 0;

    return setsockopt(sockfd, IPPROTO_TCP, TCP_DEFER_ACCEPT, &timeout, sizeof(int));
}

int mk_socket_close(int socket)
{
    return plg_netiomap->close(socket);
}

int mk_socket_create()
{
    int sockfd;

    if ((sockfd = socket(AF_INET6, SOCK_STREAM, 0)) == -1) {
        mk_libc_error("socket");
        return -1;
    }

    return sockfd;
}

int mk_socket_connect(char *host, int port)
{
    int sockfd;

    sockfd = plg_netiomap->connect(host, port);

    return sockfd;
}

int mk_socket_reset(int socket)
{
    int status = 1;

    if (setsockopt(socket, SOL_SOCKET, SO_REUSEADDR, &status, sizeof(int)) == -1) {
        mk_libc_error("socket");
        exit(EXIT_FAILURE);
    }

    return 0;
}

/* Just IPv4 for now... */
int mk_socket_server(int port, char *listen_addr)
{
    int socket_fd;

    socket_fd = plg_netiomap->server(port, listen_addr);

    if (socket_fd < 0) {
        exit(EXIT_FAILURE);
    }

    return socket_fd;
}

/* NETWORK_IO plugin functions */
int mk_socket_accept(int server_fd)
{
    return plg_netiomap->accept(server_fd);
}

int mk_socket_sendv(int socket_fd, struct mk_iov *mk_io)
{
    int bytes;
    bytes = plg_netiomap->writev(socket_fd, mk_io);

    if (config->safe_event_write == MK_TRUE) {
        mk_socket_safe_event_write(socket_fd);
    }
    return bytes;
}

int mk_socket_send(int socket_fd, const void *buf, size_t count)
{
    int bytes;
    bytes = plg_netiomap->write(socket_fd, buf, count);

    if (config->safe_event_write == MK_TRUE) {
        mk_socket_safe_event_write(socket_fd);
    }
    return bytes;
}

int mk_socket_read(int socket_fd, void *buf, int count)
{
    return plg_netiomap->read(socket_fd, (void *)buf, count);
}

int mk_socket_send_file(int socket_fd, int file_fd, off_t *file_offset,
                        size_t file_count)
{
    int bytes;

    bytes = plg_netiomap->send_file(socket_fd, file_fd,
                                    file_offset, file_count);

    if (config->safe_event_write == MK_TRUE) {
        mk_socket_safe_event_write(socket_fd);
    }
    return bytes;
}

int mk_socket_ip_str(int socket_fd, char **buf, int size, unsigned long *len)
{
    int ret;
    struct sockaddr_storage addr;
    socklen_t s_len = sizeof(addr);

    ret = getpeername(socket_fd, (struct sockaddr *) &addr, &s_len);

    if (mk_unlikely(ret == -1)) {
        MK_TRACE("[FD %i] Can't get addr for this socket", socket_fd);
        return -1;
    }

    errno = 0;

    if(addr.ss_family == AF_INET) {
        if((inet_ntop(AF_INET, &((struct sockaddr_in *)&addr)->sin_addr,
                      *buf, size)) == NULL) {
            mk_warn("mk_socket_ip_str: Can't get the IP text form (%i)", errno);
            return -1;
        }
    }
    else if(addr.ss_family == AF_INET6) {
        if((inet_ntop(AF_INET6, &((struct sockaddr_in6 *)&addr)->sin6_addr,
                      *buf, size)) == NULL) {
            mk_warn("mk_socket_ip_str: Can't get the IP text form (%i)", errno);
            return -1;
        }
    }

    *len = strlen(*buf);
    return 0;
}
