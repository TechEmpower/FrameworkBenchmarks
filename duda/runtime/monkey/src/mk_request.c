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
#include <limits.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <time.h>
#include <netdb.h>
#include <sys/wait.h>
#include <signal.h>
#include <ctype.h>
#include <errno.h>

#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <fcntl.h>

#include "monkey.h"
#include "mk_request.h"
#include "mk_http.h"
#include "mk_http_status.h"
#include "mk_string.h"
#include "mk_config.h"
#include "mk_scheduler.h"
#include "mk_epoll.h"
#include "mk_utils.h"
#include "mk_header.h"
#include "mk_user.h"
#include "mk_method.h"
#include "mk_memory.h"
#include "mk_socket.h"
#include "mk_cache.h"
#include "mk_clock.h"
#include "mk_plugin.h"
#include "mk_macros.h"
#include "mk_vhost.h"

const mk_pointer mk_crlf = mk_pointer_init(MK_CRLF);
const mk_pointer mk_endblock = mk_pointer_init(MK_ENDBLOCK);

const mk_pointer mk_rh_accept = mk_pointer_init(RH_ACCEPT);
const mk_pointer mk_rh_accept_charset = mk_pointer_init(RH_ACCEPT_CHARSET);
const mk_pointer mk_rh_accept_encoding = mk_pointer_init(RH_ACCEPT_ENCODING);
const mk_pointer mk_rh_accept_language = mk_pointer_init(RH_ACCEPT_LANGUAGE);
const mk_pointer mk_rh_connection = mk_pointer_init(RH_CONNECTION);
const mk_pointer mk_rh_cookie = mk_pointer_init(RH_COOKIE);
const mk_pointer mk_rh_content_length = mk_pointer_init(RH_CONTENT_LENGTH);
const mk_pointer mk_rh_content_range = mk_pointer_init(RH_CONTENT_RANGE);
const mk_pointer mk_rh_content_type = mk_pointer_init(RH_CONTENT_TYPE);
const mk_pointer mk_rh_if_modified_since = mk_pointer_init(RH_IF_MODIFIED_SINCE);
const mk_pointer mk_rh_host = mk_pointer_init(RH_HOST);
const mk_pointer mk_rh_last_modified = mk_pointer_init(RH_LAST_MODIFIED);
const mk_pointer mk_rh_last_modified_since = mk_pointer_init(RH_LAST_MODIFIED_SINCE);
const mk_pointer mk_rh_referer = mk_pointer_init(RH_REFERER);
const mk_pointer mk_rh_range = mk_pointer_init(RH_RANGE);
const mk_pointer mk_rh_user_agent = mk_pointer_init(RH_USER_AGENT);

pthread_key_t request_list;

/* Create a memory allocation in order to handle the request data */
static void mk_request_init(struct session_request *request)
{
    memset(request, 0, sizeof(struct session_request));

    request->status = MK_TRUE;
    request->method = MK_HTTP_METHOD_UNKNOWN;

    request->file_info.size = -1;

    request->bytes_to_send = -1;
    request->fd_file = -1;

    /* Response Headers */
    mk_header_response_reset(&request->headers);
}

static void mk_request_free(struct session_request *sr)
{
    if (sr->fd_file > 0) {
        mk_vhost_close(sr);
    }

    if (sr->headers.location) {
        mk_mem_free(sr->headers.location);
    }

    if (sr->uri_processed.data != sr->uri.data) {
        mk_pointer_free(&sr->uri_processed);
    }

    if (sr->real_path.data != sr->real_path_static) {
        mk_pointer_free(&sr->real_path);
    }
}

int mk_request_header_toc_parse(struct headers_toc *toc, const char *data, int len)
{
    int i = 0;
    int header_len;
    int colon;
    char *q;
    char *p = (char *) data;
    char *l = p;

    toc->length = 0;

    for (i = 0; l < (data + len) && p && i < MK_HEADERS_TOC_LEN; i++) {
        if (*p == '\r') goto out;

        /* Locate the colon character and the end of the line by CRLF */
        colon = -1;
        for (q = p; *q != 0x0D; ++q) {
            if (*q == ':' && colon == -1) {
                colon = (q - p);
            }
        }

        /* it must be a LF after CR */
        if (*(q + 1) != 0x0A) {
            return -1;
        }

        /*
         * Check if we reach the last header, take in count the first one can
         * be also the last.
         */
        if (data + len == (q - 1) && colon == -1) {
            break;
        }

        /*
         * By this version we force that after the colon must exists a white
         * space before the value field
         */
        if (*(p + colon + 1) != 0x20) {
            return -1;
        }


        /* Each header key must have a value */
        header_len = q - p - colon - 2;
        if (header_len == 0) {
            return -1;
        }

        /* Register the entry */
        toc->rows[i].init = p;
        toc->rows[i].end = q;
        toc->rows[i].status = 0;
        p = (q + mk_crlf.len);
        l = p;
        toc->length++;
    }

 out:
    return toc->length;
}

/* Return a struct with method, URI , protocol version
and all static headers defined here sent in request */
static int mk_request_header_process(struct session_request *sr)
{
    int uri_init = 0, uri_end = 0;
    int query_init = 0;
    int prot_init = 0, prot_end = 0, pos_sep = 0;
    int fh_limit;
    char *headers;
    char *temp = 0;
    mk_pointer host;

    /* Method */
    sr->method_p = mk_http_method_check_str(sr->method);

    /* Request URI */
    temp = index(sr->body.data, ' ');
    if (mk_unlikely(!temp)) {
        MK_TRACE("Error, invalid first header");
        return -1;
    }
    uri_init = (temp - sr->body.data) + 1;

    temp = index(sr->body.data, '\n');
    if (mk_unlikely(!temp)) {
        MK_TRACE("Error, invalid header CRLF");
        return -1;
    }
    fh_limit = (temp - sr->body.data);

    uri_end = mk_string_char_search_r(sr->body.data, ' ', fh_limit) - 1;
    if (mk_unlikely(uri_end <= 0)) {
        MK_TRACE("Error, first header bad formed");
        return -1;
    }

    prot_init = uri_end + 2;

    if (mk_unlikely(uri_end < uri_init)) {
        return -1;
    }

    /* Query String */
    query_init = mk_string_char_search(sr->body.data + uri_init, '?', prot_init);
    if (query_init > 0) {
        int init, end;

        init = query_init + uri_init;
        if (init <= uri_end) {
            end = uri_end;
            uri_end = init - 1;

            sr->query_string = mk_pointer_create(sr->body.data,
                                                 init + 1, end + 1);
        }
    }

    /* Request URI Part 2 */
    sr->uri = mk_pointer_create(sr->body.data, uri_init, uri_end + 1);
    if (mk_unlikely(sr->uri.len < 1)) {
        return -1;
    }

    /* HTTP Version */
    prot_end = fh_limit - 1;
    if (mk_unlikely(prot_init == prot_end)) {
        return  -1;
    }

    if (prot_end != prot_init && prot_end > 0) {
        sr->protocol = mk_http_protocol_check(sr->body.data + prot_init,
                                              prot_end - prot_init);
        sr->protocol_p = mk_http_protocol_check_str(sr->protocol);
    }

    headers = sr->body.data + prot_end + mk_crlf.len;

    /*
     * Process URI, if it contains ASCII encoded strings like '%20',
     * it will return a new memory buffer with the decoded string, otherwise
     * it returns NULL
     */
    temp = mk_utils_url_decode(sr->uri);
    if (temp) {
        sr->uri_processed.data = temp;
        sr->uri_processed.len  = strlen(temp);
    }
    else {
        sr->uri_processed.data = sr->uri.data;
        sr->uri_processed.len  = sr->uri.len;
    }

    /* Creating Table of Content (index) for HTTP headers */
    sr->headers_len = sr->body.len - (prot_end + mk_crlf.len);
    if (mk_request_header_toc_parse(&sr->headers_toc, headers, sr->headers_len) < 0) {
        MK_TRACE("Invalid headers");
        return -1;
    }

    /* Host */
    host = mk_request_header_get(&sr->headers_toc,
                                 mk_rh_host.data,
                                 mk_rh_host.len);
    if (host.data) {
        if ((pos_sep = mk_string_char_search_r(host.data, ':', host.len)) >= 0) {
            /* TCP port should not be higher than 65535 */
            char *p;
            short int port_len, port_size = 6;
            char port[port_size];

            /* just the host */
            sr->host.data = host.data;
            sr->host.len = pos_sep;

            /* including the port */
            sr->host_port = host;

            /* Port string length */
            port_len = (host.len - pos_sep - 1);
            if (port_len >= port_size) {
                return -1;
            }

            /* Copy to buffer */
            memcpy(port, host.data + pos_sep + 1, port_len);
            port[port_len] = '\0';

            /* Validate that the input port is numeric */
            p = port;
            while (*p) {
                if (!isdigit(*p)) return -1;
                p++;
            }

            /* Convert to base 10 */
            errno = 0;
            sr->port = strtol(port, (char **) NULL, 10);
            if ((errno == ERANGE && (sr->port == LONG_MAX || sr->port == LONG_MIN))
                || sr->port == 0) {
                return -1;
            }
        }
        else {
            sr->host = host;    /* maybe null */
            sr->port = config->standard_port;
        }
    }
    else {
        sr->host.data = NULL;
    }

    /* Looking for headers that ONLY Monkey uses */
    sr->connection = mk_request_header_get(&sr->headers_toc,
                                           mk_rh_connection.data,
                                           mk_rh_connection.len);

    sr->range = mk_request_header_get(&sr->headers_toc,
                                      mk_rh_range.data,
                                      mk_rh_range.len);

    sr->if_modified_since = mk_request_header_get(&sr->headers_toc,
                                                  mk_rh_if_modified_since.data,
                                                  mk_rh_if_modified_since.len);

    /* Default Keepalive is off */
    if (sr->protocol == MK_HTTP_PROTOCOL_10) {
        sr->keep_alive = MK_FALSE;
        sr->close_now = MK_TRUE;
    }
    else if(sr->protocol == MK_HTTP_PROTOCOL_11) {
        sr->keep_alive = MK_TRUE;
        sr->close_now = MK_FALSE;
    }

    if (sr->connection.data) {
        if (mk_string_search_n(sr->connection.data, "Keep-Alive",
                               MK_STR_INSENSITIVE, sr->connection.len) >= 0) {
            sr->keep_alive = MK_TRUE;
            sr->close_now = MK_FALSE;
        }
        else if (mk_string_search_n(sr->connection.data, "Close",
                                    MK_STR_INSENSITIVE, sr->connection.len) >= 0) {
            sr->keep_alive = MK_FALSE;
            sr->close_now = MK_TRUE;
        }
        else {
            /* Set as a non-valid connection header value */
            sr->connection.len = 0;
        }
    }

    return 0;
}

static int mk_request_parse(struct client_session *cs)
{
    int i, end;
    int blocks = 0;
    struct session_request *sr_node;
    struct mk_list *sr_list, *sr_head;

    for (i = 0; i <= cs->body_pos_end; i++) {
        /*
         * Pipelining can just exists in a persistent connection or
         * well known as KeepAlive, so if we are in keepalive mode
         * we should check if we have multiple request in our body buffer
         */
        if (cs->counter_connections > 0) {
            /*
             * Look for CRLFCRLF (\r\n\r\n), maybe some pipelining
             * request can be involved.
             */
            end = mk_string_search(cs->body + i, mk_endblock.data, MK_STR_SENSITIVE) + i;
        }
        else {
            end = cs->body_pos_end;
        }

        if (end <  0) {
            return -1;
        }

        /* Allocating request block */
        if (blocks == 0) {
            sr_node = &cs->sr_fixed;
        }
        else {
            sr_node = mk_mem_malloc(sizeof(struct session_request));
        }
        mk_request_init(sr_node);

        /* We point the block with a mk_pointer */
        sr_node->body.data = cs->body + i;
        sr_node->body.len = end - i;

        /* Method, previous catch in mk_http_pending_request */
        if (i == 0) {
            sr_node->method = cs->first_method;
        }
        else {
            sr_node->method = mk_http_method_get(sr_node->body.data);
        }

        /* Looking for POST data */
        if (sr_node->method == MK_HTTP_METHOD_POST) {
            int offset;
            offset = end + mk_endblock.len;
            sr_node->data = mk_method_get_data(cs->body + offset,
                                               cs->body_length - offset);
        }

        /* Increase index to the end of the current block */
        i = (end + mk_endblock.len) - 1;

        /* Link block */
        mk_list_add(&sr_node->_head, &cs->request_list);

        /* Update counter */
        blocks++;
    }

    /* DEBUG BLOCKS
    struct mk_list *head;
    struct session_request *entry;

    printf("\n*******************\n");
    mk_list_foreach(head, &cs->request_list) {
        entry = mk_list_entry(head, struct session_request, _head);
        mk_pointer_print(entry->body);
        fflush(stdout);
    }
    */

    /* Checking pipelining connection */
    if (blocks > 1) {
        sr_list = &cs->request_list;
        mk_list_foreach(sr_head, sr_list) {
            sr_node = mk_list_entry(sr_head, struct session_request, _head);
            /* Pipelining request must use GET or HEAD methods */
            if (sr_node->method != MK_HTTP_METHOD_GET &&
                sr_node->method != MK_HTTP_METHOD_HEAD) {
                return -1;
            }
        }

        cs->pipelined = MK_TRUE;
    }

    return 0;
}

/* This function allow the core to invoke the closing connection process
 * when some connection was not proceesed due to a premature close or similar
 * exception, it also take care of invoke the STAGE_40 and STAGE_50 plugins events
 */
static void mk_request_premature_close(int http_status, struct client_session *cs)
{
    struct session_request *sr;
    struct mk_list *sr_list = &cs->request_list;
    struct mk_list *host_list = &config->hosts;

    /*
     * If the connection is too premature, we need to allocate a temporal session_request
     * to do not break the plugins stages
     */
    if (mk_list_is_empty(sr_list) == 0) {
        sr = &cs->sr_fixed;
        mk_request_init(sr);
        mk_list_add(&sr->_head, &cs->request_list);
    }
    else {
        sr = mk_list_entry_first(sr_list, struct session_request, _head);
    }

    /* Raise error */
    if (http_status > 0) {
        if (!sr->host_conf) {
            sr->host_conf = mk_list_entry_first(host_list, struct host, _head);
        }
        mk_request_error(http_status, cs, sr);

        /* STAGE_40, request has ended */
        mk_plugin_stage_run(MK_PLUGIN_STAGE_40, cs->socket,
                            NULL, cs, sr);
    }

    /* STAGE_50, connection closed  and remove client_session*/
    mk_plugin_stage_run(MK_PLUGIN_STAGE_50, cs->socket, NULL, NULL, NULL);
    mk_session_remove(cs->socket);
}

static int mk_request_process(struct client_session *cs, struct session_request *sr)
{
    int status = 0;
    int socket = cs->socket;
    struct mk_list *hosts = &config->hosts;
    struct mk_list *alias;

    /* Always assign the first node 'default vhost' */
    sr->host_conf = mk_list_entry_first(hosts, struct host, _head);

    /* Parse request */
    status = mk_request_header_process(sr);
    if (status < 0) {
        mk_header_set_http_status(sr, MK_CLIENT_BAD_REQUEST);
        mk_request_error(MK_CLIENT_BAD_REQUEST, cs, sr);
        return EXIT_ABORT;
    }

    sr->user_home = MK_FALSE;

    /* Valid request URI? */
    if (sr->uri_processed.data[0] != '/') {
        mk_request_error(MK_CLIENT_BAD_REQUEST, cs, sr);
        return EXIT_NORMAL;
    }

    /* HTTP/1.1 needs Host header */
    if (!sr->host.data && sr->protocol == MK_HTTP_PROTOCOL_11) {
        mk_request_error(MK_CLIENT_BAD_REQUEST, cs, sr);
        return EXIT_NORMAL;
    }

    /* Validating protocol version */
    if (sr->protocol == MK_HTTP_PROTOCOL_UNKNOWN) {
        mk_request_error(MK_SERVER_HTTP_VERSION_UNSUP, cs, sr);
        return EXIT_ABORT;
    }

    /* Assign the first node alias */
    alias = &sr->host_conf->server_names;
    sr->host_alias = mk_list_entry_first(alias,
                                         struct host_alias, _head);

    if (sr->host.data) {
        mk_vhost_get(sr->host, &sr->host_conf, &sr->host_alias);
    }

    /* Is requesting an user home directory ? */
    if (config->user_dir &&
        sr->uri_processed.len > 2 &&
        sr->uri_processed.data[1] == MK_USER_HOME) {

        if (mk_user_init(cs, sr) != 0) {
            mk_request_error(MK_CLIENT_NOT_FOUND, cs, sr);
            return EXIT_ABORT;
        }
    }

    /* Handling method requested */
    if (sr->method == MK_HTTP_METHOD_POST || sr->method == MK_HTTP_METHOD_PUT) {
        if ((status = mk_method_parse_data(cs, sr)) != 0) {
            return status;
        }
    }

    /* Plugins Stage 20 */
    int ret;
    ret = mk_plugin_stage_run(MK_PLUGIN_STAGE_20, socket, NULL, cs, sr);
    if (ret == MK_PLUGIN_RET_CLOSE_CONX) {
        MK_TRACE("STAGE 20 requested close conexion");
        return EXIT_ABORT;
    }

    /* Normal HTTP process */
    status = mk_http_init(cs, sr);

    MK_TRACE("[FD %i] HTTP Init returning %i", socket, status);

    return status;
}

/* Build error page */
static mk_pointer *mk_request_set_default_page(char *title, mk_pointer message,
                                        char *signature)
{
    char *temp;
    mk_pointer *p;

    p = mk_mem_malloc(sizeof(mk_pointer));
    p->data = NULL;

    temp = mk_pointer_to_buf(message);
    mk_string_build(&p->data, &p->len,
                    MK_REQUEST_DEFAULT_PAGE, title, temp, signature);

    mk_mem_free(temp);

    return p;
}

int mk_handler_read(int socket, struct client_session *cs)
{
    int bytes;
    int available = 0;
    int new_size;
    char *tmp = 0;

    MK_TRACE("MAX REQUEST SIZE: %i", config->max_request_size);

    available = cs->body_size - cs->body_length;
    if (available <= 0) {
        /* Reallocate buffer size if pending data does not have space */
        new_size = cs->body_size + MK_REQUEST_CHUNK;
        if (new_size >= config->max_request_size) {
            MK_TRACE("Requested size is > config->max_request_size");
            mk_request_premature_close(MK_CLIENT_REQUEST_ENTITY_TOO_LARGE, cs);
            return -1;
        }

        /*
         * Check if the body field still points to the initial body_fixed, if so,
         * allow the new space required in body, otherwise perform a realloc over
         * body.
         */
        if (cs->body == cs->body_fixed) {
            MK_TRACE("Fixed to dynamic");
            cs->body = mk_mem_malloc(new_size + 1);
            cs->body_size = new_size;
            memcpy(cs->body, cs->body_fixed, cs->body_length);
            MK_TRACE("Size: %i, Length: %i", new_size, cs->body_length);
        }
        else {
            MK_TRACE("Realloc from %i to %i", cs->body_size, new_size);
            tmp = mk_mem_realloc(cs->body, new_size + 1);
            if (tmp) {
                cs->body = tmp;
                cs->body_size = new_size;
            }
            else {
                mk_request_premature_close(MK_SERVER_INTERNAL_ERROR, cs);
                return -1;
            }
        }
    }

    /* Read content */
    bytes = mk_socket_read(socket, cs->body + cs->body_length,
                           (cs->body_size - cs->body_length));

    MK_TRACE("[FD %i] read %i", socket, bytes);

    if (bytes < 0) {
        if (errno == EAGAIN) {
            return 1;
        }
        else {
            mk_session_remove(socket);
            return -1;
        }
    }
    if (bytes == 0) {
        mk_session_remove(socket);
        return -1;
    }

    if (bytes > 0) {
        cs->body_length += bytes;
        cs->body[cs->body_length] = '\0';
    }

    return bytes;
}

int mk_handler_write(int socket, struct client_session *cs)
{
    int final_status = 0;
    struct session_request *sr_node;
    struct mk_list *sr_list, *sr_head;

    if (mk_list_is_empty(&cs->request_list) == 0) {
        if (mk_request_parse(cs) != 0) {
            return -1;
        }
    }

    sr_list = &cs->request_list;
    mk_list_foreach(sr_head, sr_list) {
        sr_node = mk_list_entry(sr_head, struct session_request, _head);

        if (sr_node->bytes_to_send > 0) {
            /* Request with data to send by static file sender */
            final_status = mk_http_send_file(cs, sr_node);
        }
        else if (sr_node->bytes_to_send < 0) {
            final_status = mk_request_process(cs, sr_node);
        }

        /*
         * If we got an error, we don't want to parse
         * and send information for another pipelined request
         */
        if (final_status > 0) {
            return final_status;
        }
        else {
            /* STAGE_40, request has ended */
            mk_plugin_stage_run(MK_PLUGIN_STAGE_40, socket,
                                NULL, cs, sr_node);
            switch (final_status) {
            case EXIT_NORMAL:
            case EXIT_ERROR:
                if (sr_node->close_now == MK_TRUE) {
                    return -1;
                }
                break;
            case EXIT_ABORT:
                  return -1;
            }
        }
    }

    /*
     * If we are here, is because all pipelined request were
     * processed successfully, let's return 0;
     */
    return 0;
}

/* Look for some  index.xxx in pathfile */
mk_pointer mk_request_index(char *pathfile, char *file_aux, const unsigned int flen)
{
    unsigned long len;
    mk_pointer f;
    struct mk_string_line *entry;
    struct mk_list *head;

    mk_pointer_reset(&f);
    if (!config->index_files) return f;

    mk_list_foreach(head, config->index_files) {
        entry = mk_list_entry(head, struct mk_string_line, _head);
        len = snprintf(file_aux, flen, "%s%s", pathfile, entry->val);
        if (mk_unlikely(len > flen)) {
            len = flen;
            mk_warn("Path too long, truncated! '%s'", file_aux);
        }

        if (access(file_aux, F_OK) == 0) {
            f.data = file_aux;
            f.len = len;
            return f;
        }
    }

    return f;
}

/* Send error responses */
int mk_request_error(int http_status, struct client_session *cs,
                     struct session_request *sr) {
    int ret, fd;
    mk_pointer message, *page = 0;
    struct error_page *entry;
    struct mk_list *head;
    struct file_info finfo;

    mk_header_set_http_status(sr, http_status);

    /*
     * We are nice sending error pages for clients who at least respect
     * the especification
     */
    if (http_status != MK_CLIENT_LENGTH_REQUIRED &&
        http_status != MK_CLIENT_BAD_REQUEST &&
        http_status != MK_CLIENT_REQUEST_ENTITY_TOO_LARGE) {

        /* Lookup a customized error page */
        mk_list_foreach(head, &sr->host_conf->error_pages) {
            entry = mk_list_entry(head, struct error_page, _head);
            if (entry->status != http_status) {
                continue;
            }

            /* validate error file */
            ret = mk_file_get_info(entry->real_path, &finfo);
            if (ret == -1) {
                break;
            }

            /* open file */
            fd = open(entry->real_path, config->open_flags);
            if (fd == -1) {
                break;
            }

            sr->fd_file = fd;
            sr->bytes_to_send = finfo.size;
            sr->headers.content_length = finfo.size;
            sr->headers.real_length    = finfo.size;

            memcpy(&sr->file_info, &finfo, sizeof(struct file_info));

            mk_header_send(cs->socket, cs, sr);
            return mk_http_send_file(cs, sr);
        }
    }

    mk_pointer_reset(&message);

    switch (http_status) {
    case MK_CLIENT_BAD_REQUEST:
        page = mk_request_set_default_page("Bad Request",
                                           sr->uri,
                                           sr->host_conf->host_signature);
        break;

    case MK_CLIENT_FORBIDDEN:
        page = mk_request_set_default_page("Forbidden",
                                           sr->uri,
                                           sr->host_conf->host_signature);
        break;

    case MK_CLIENT_NOT_FOUND:
        mk_string_build(&message.data, &message.len,
                        "The requested URL was not found on this server.");
        page = mk_request_set_default_page("Not Found",
                                           message,
                                           sr->host_conf->host_signature);
        mk_pointer_free(&message);
        break;

    case MK_CLIENT_REQUEST_ENTITY_TOO_LARGE:
        mk_string_build(&message.data, &message.len,
                        "The request entity is too large.");
        page = mk_request_set_default_page("Entity too large",
                                           message,
                                           sr->host_conf->host_signature);
        mk_pointer_free(&message);
        break;

    case MK_CLIENT_METHOD_NOT_ALLOWED:
        page = mk_request_set_default_page("Method Not Allowed",
                                           sr->uri,
                                           sr->host_conf->host_signature);
        break;

    case MK_CLIENT_REQUEST_TIMEOUT:
    case MK_CLIENT_LENGTH_REQUIRED:
        break;

    case MK_SERVER_NOT_IMPLEMENTED:
        page = mk_request_set_default_page("Method Not Implemented",
                                           sr->uri,
                                           sr->host_conf->host_signature);
        break;

    case MK_SERVER_INTERNAL_ERROR:
        page = mk_request_set_default_page("Internal Server Error",
                                           sr->uri,
                                           sr->host_conf->host_signature);
        break;

    case MK_SERVER_HTTP_VERSION_UNSUP:
        mk_pointer_reset(&message);
        page = mk_request_set_default_page("HTTP Version Not Supported",
                                           message,
                                           sr->host_conf->host_signature);
        break;
    }

    if (page) {
        sr->headers.content_length = page->len;
    }

    sr->headers.location = NULL;
    sr->headers.cgi = SH_NOCGI;
    sr->headers.pconnections_left = 0;
    sr->headers.last_modified = -1;

    if (!page) {
        mk_pointer_reset(&sr->headers.content_type);
    }
    else {
        mk_pointer_set(&sr->headers.content_type, "text/html\r\n");
    }

    mk_header_send(cs->socket, cs, sr);

    if (page) {
        if (sr->method != MK_HTTP_METHOD_HEAD)
            mk_socket_send(cs->socket, page->data, page->len);

        mk_pointer_free(page);
        mk_mem_free(page);
    }

    /* Turn off TCP_CORK */
    mk_socket_set_cork_flag(cs->socket, TCP_CORK_OFF);
    return EXIT_ERROR;
}

void mk_request_free_list(struct client_session *cs)
{
    struct session_request *sr_node;
    struct mk_list *sr_head, *temp;

    /* sr = last node */
    MK_TRACE("[FD %i] Free struct client_session", cs->socket);

    mk_list_foreach_safe(sr_head, temp, &cs->request_list) {
        sr_node = mk_list_entry(sr_head, struct session_request, _head);
        mk_list_del(sr_head);

        mk_request_free(sr_node);
        if (sr_node != &cs->sr_fixed) {
            mk_mem_free(sr_node);
        }
    }
}

/* Create a client request struct and put it on the
 * main list
 */
struct client_session *mk_session_create(int socket, struct sched_list_node *sched)
{
    struct client_session *cs;
    struct sched_connection *sc;

    sc = mk_sched_get_connection(sched, socket);
    if (!sc) {
        MK_TRACE("[FD %i] No sched node, could not create session", socket);
        return NULL;
    }

    /* Alloc memory for node */
    cs = mk_mem_malloc(sizeof(struct client_session));

    cs->pipelined = MK_FALSE;
    cs->counter_connections = 0;
    cs->socket = socket;
    cs->status = MK_REQUEST_STATUS_INCOMPLETE;

    /* creation time in unix time */
    cs->init_time = sc->arrive_time;

    /* alloc space for body content */
    cs->body = cs->body_fixed;

    /* Buffer size based in Chunk bytes */
    cs->body_size = MK_REQUEST_CHUNK;
    /* Current data length */
    cs->body_length = 0;

    cs->body_pos_end = -1;
    cs->first_method = MK_HTTP_METHOD_UNKNOWN;

    /* Init session request list */
    mk_list_init(&cs->request_list);

    /* Add this SESSION to the thread list */

    /* Add node to list */
    /* Red-Black tree insert routine */
    struct rb_node **new = &(cs_list->rb_node);
    struct rb_node *parent = NULL;

    /* Figure out where to put new node */
    while (*new) {
        struct client_session *this = container_of(*new, struct client_session, _rb_head);

        parent = *new;
        if (cs->socket < this->socket)
            new = &((*new)->rb_left);
        else if (cs->socket > this->socket)
            new = &((*new)->rb_right);
        else {
            break;
        }
    }
    /* Add new node and rebalance tree. */
    rb_link_node(&cs->_rb_head, parent, new);
    rb_insert_color(&cs->_rb_head, cs_list);

    return cs;
}

struct client_session *mk_session_get(int socket)
{
    struct client_session *cs;
    struct rb_node *node;

    node = cs_list->rb_node;
  	while (node) {
  		cs = container_of(node, struct client_session, _rb_head);
		if (socket < cs->socket)
  			node = node->rb_left;
		else if (socket > cs->socket)
  			node = node->rb_right;
		else {
  			return cs;
        }
	}
	return NULL;
}

/*
 * From thread sched_list_node "list", remove the client_session
 * struct information
 */
void mk_session_remove(int socket)
{
    struct client_session *cs_node;

    cs_node = mk_session_get(socket);
    if (cs_node) {
        rb_erase(&cs_node->_rb_head, cs_list);
        if (cs_node->body != cs_node->body_fixed) {
            mk_mem_free(cs_node->body);
        }
        mk_mem_free(cs_node);
    }
}

/* Return value of some variable sent in request */
mk_pointer mk_request_header_get(struct headers_toc *toc, const char *key_name, int key_len)
{
    int i;
    struct header_toc_row *row;
    mk_pointer var;

    var.data = NULL;
    var.len = 0;

    row = toc->rows;
    for (i = 0; i < toc->length; i++) {

        /*
         * status = 1 means that the toc entry was already
         * checked by Monkey
         */
        if (row[i].status == 1) {
            continue;
        }

        if (strncasecmp(row[i].init, key_name, key_len) == 0) {
            var.data = row[i].init + key_len + 1;
            var.len = row[i].end - var.data;
            row[i].status = 1;
            break;
        }
    }

    return var;
}

void mk_request_ka_next(struct client_session *cs)
{
    cs->first_method = -1;
    cs->body_pos_end = -1;
    cs->body_length = 0;
    cs->counter_connections++;

    /* Update data for scheduler */
    cs->init_time = log_current_utime;
    cs->status = MK_REQUEST_STATUS_INCOMPLETE;
}
