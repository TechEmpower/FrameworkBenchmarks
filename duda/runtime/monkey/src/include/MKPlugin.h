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

#ifndef MONKEY_PLUGIN_H
#define MONKEY_PLUGIN_H

/* General Headers */
#include <errno.h>

/* Monkey Headers */
#include "mk_plugin.h"
#include "mk_vhost.h"
#include "mk_list.h"
#include "mk_http.h"
#include "mk_file.h"
#include "mk_socket.h"
#include "mk_macros.h"

/* global vars */
struct plugin_api *mk_api;
struct plugin_info MK_EXPORT _plugin_info;

pthread_key_t MK_EXPORT _mkp_data;

#define MONKEY_PLUGIN(a, b, c, d)                   \
    struct plugin_info MK_EXPORT _plugin_info = {a, b, c, d}

#ifdef TRACE
#define PLUGIN_TRACE(...) \
    mk_api->trace(_plugin_info.shortname,     \
                  MK_TRACE_PLUGIN,            \
                  __FUNCTION__,               \
                  __FILE__,                   \
                  __LINE__,                   \
                  __VA_ARGS__)
#else
#define PLUGIN_TRACE(...) do {} while(0)
#endif

/* Hook defines */
int MK_EXPORT _mkp_init(struct plugin_api **api, char *confdir);
void MK_EXPORT _mkp_exit();
int MK_EXPORT _mkp_core_prctx(struct server_config *config);
void MK_EXPORT _mkp_core_thctx();
int MK_EXPORT _mkp_stage_10(unsigned int socket, struct sched_connection *conx);
int MK_EXPORT _mkp_stage_20(struct client_session *cs, struct session_request *sr);
int MK_EXPORT _mkp_stage_30(struct plugin *plugin, struct client_session *cs,
                            struct session_request *sr);
int MK_EXPORT _mkp_stage_40(struct client_session *cs, struct session_request *sr);
int MK_EXPORT _mkp_stage_50(int sockfd);
int MK_EXPORT _mkp_network_io_accept(int server_fd);
int MK_EXPORT _mkp_network_io_read(int socket_fd, void *buf, int count);
int MK_EXPORT _mkp_network_io_write(int socket_fd, const void *buf, size_t count);
int MK_EXPORT _mkp_network_io_writev(int socket_fd, struct mk_iov *mk_io);
int MK_EXPORT _mkp_network_io_close(int socket_fd);
int MK_EXPORT _mkp_network_io_connect(char *host, int port);
int MK_EXPORT _mkp_network_io_send_file(int socket_fd, int file_fd, off_t *file_offset,
                                        size_t file_count);
int MK_EXPORT _mkp_network_io_create_socket(int domain, int type, int protocol);
int MK_EXPORT _mkp_network_io_bind(int socket_fd, const struct sockaddr *addr,
                                   socklen_t addrlen, int backlog);
int MK_EXPORT _mkp_network_io_server(int port, char *listen_addr);
int MK_EXPORT _mkp_event_read(int sockfd);
int MK_EXPORT _mkp_event_write(int sockfd);
int MK_EXPORT _mkp_event_error(int sockfd);
int MK_EXPORT _mkp_event_close(int sockfd);
int MK_EXPORT _mkp_event_timeout(int sockfd);


/*
 * Redefine messages macros
 */

#undef  mk_info
#define mk_info(...) mk_api->_error(MK_INFO, __VA_ARGS__)

#undef  mk_err
#define mk_err(...) mk_api->_error(MK_ERR, __VA_ARGS__)

#undef  mk_warn
#define mk_warn(...) mk_api->_error(MK_WARN, __VA_ARGS__)

#undef  mk_bug
#define mk_bug(condition) do {                  \
        if (mk_unlikely((condition)!=0)) {         \
            mk_api->_error(MK_BUG, "[%s] Bug found in %s() at %s:%d",    \
                           _plugin_info.shortname, __FUNCTION__, __FILE__, __LINE__); \
            abort();                                                    \
        }                                                               \
    } while(0)

#endif
