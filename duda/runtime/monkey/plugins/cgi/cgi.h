/*  Monkey HTTP Daemon
 *  ------------------
 *  Copyright (C) 2012-2013, Lauri Kasanen
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

#ifndef CGI_H
#define CGI_H

#define _GNU_SOURCE

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <regex.h>
#include <unistd.h>
#include <signal.h>
#include <libgen.h>

#include "MKPlugin.h"

enum {
    PATHLEN = 1024,
    SHORTLEN = 64
};

regex_t match_regex;

struct cgi_request **requests_by_socket;

struct post_t {
    int fd;
    void *buf;
    unsigned long len;
};

struct cgi_match_t {
    regex_t match;
    char *bin;
    mk_pointer content_type;

    struct mk_list _head;
};

struct cgi_vhost_t {
    struct host *host;
    struct mk_list matches;
};

struct cgi_vhost_t *cgi_vhosts;
struct mk_list cgi_global_matches;


struct cgi_request {

    char in_buf[PATHLEN];

    struct mk_list _head;

    struct session_request *sr;
    struct client_session *cs;

    unsigned int in_len;

    int fd;			/* From the CGI app */
    int socket;

    unsigned char status_done;
    unsigned char all_headers_done;
    unsigned char chunked;
};

/* Global list per worker */
pthread_key_t cgi_request_list;

extern struct cgi_request **requests_by_socket;

int swrite(const int fd, const void *buf, const size_t count);

struct cgi_request *cgi_req_create(int fd, int socket, struct session_request *sr,
					struct client_session *cs);
void cgi_req_add(struct cgi_request *r);
int cgi_req_del(struct cgi_request *r);

// Get the CGI request by the client socket
static inline struct cgi_request *cgi_req_get(int socket)
{
    struct cgi_request *r = requests_by_socket[socket];

    return r;
}

// Get the CGI request by the CGI app's fd
static inline struct cgi_request *cgi_req_get_by_fd(int fd)
{
    struct mk_list *list, *node;
    struct cgi_request *r;

    list = pthread_getspecific(cgi_request_list);
    if (mk_list_is_empty(list) == 0)
        return NULL;

    mk_list_foreach(node, list) {
        r = mk_list_entry(node, struct cgi_request, _head);
        if (r->fd == fd)
            return r;
    }

    return NULL;
}

#endif
