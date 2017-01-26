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
#include <string.h>
#include <stdlib.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "monkey.h"
#include "mk_memory.h"
#include "mk_http.h"
#include "mk_http_status.h"
#include "mk_clock.h"
#include "mk_file.h"
#include "mk_utils.h"
#include "mk_config.h"
#include "mk_string.h"
#include "mk_method.h"
#include "mk_socket.h"
#include "mk_mimetype.h"
#include "mk_header.h"
#include "mk_epoll.h"
#include "mk_plugin.h"
#include "mk_macros.h"
#include "mk_vhost.h"

const mk_pointer mk_http_method_get_p = mk_pointer_init(MK_HTTP_METHOD_GET_STR);
const mk_pointer mk_http_method_post_p = mk_pointer_init(MK_HTTP_METHOD_POST_STR);
const mk_pointer mk_http_method_head_p = mk_pointer_init(MK_HTTP_METHOD_HEAD_STR);
const mk_pointer mk_http_method_put_p = mk_pointer_init(MK_HTTP_METHOD_PUT_STR);
const mk_pointer mk_http_method_delete_p = mk_pointer_init(MK_HTTP_METHOD_DELETE_STR);
const mk_pointer mk_http_method_options_p = mk_pointer_init(MK_HTTP_METHOD_OPTIONS_STR);
const mk_pointer mk_http_method_null_p = { NULL, 0 };

const mk_pointer mk_http_protocol_09_p = mk_pointer_init(MK_HTTP_PROTOCOL_09_STR);
const mk_pointer mk_http_protocol_10_p = mk_pointer_init(MK_HTTP_PROTOCOL_10_STR);
const mk_pointer mk_http_protocol_11_p = mk_pointer_init(MK_HTTP_PROTOCOL_11_STR);
const mk_pointer mk_http_protocol_null_p = { NULL, 0 };


int mk_http_method_check(mk_pointer method)
{
    if (strncmp(method.data, MK_HTTP_METHOD_GET_STR, method.len) == 0) {
        return MK_HTTP_METHOD_GET;
    }

    if (strncmp(method.data, MK_HTTP_METHOD_POST_STR, method.len) == 0) {
        return MK_HTTP_METHOD_POST;
    }

    if (strncmp(method.data, MK_HTTP_METHOD_HEAD_STR, method.len) == 0) {
        return MK_HTTP_METHOD_HEAD;
    }

    if (strncmp(method.data, MK_HTTP_METHOD_PUT_STR, method.len) == 0) {
        return MK_HTTP_METHOD_PUT;
    }

    if (strncmp(method.data, MK_HTTP_METHOD_DELETE_STR, method.len) == 0) {
        return MK_HTTP_METHOD_DELETE;
    }

    if (strncmp(method.data, MK_HTTP_METHOD_OPTIONS_STR, method.len) == 0) {
        return MK_HTTP_METHOD_OPTIONS;
    }

    return MK_HTTP_METHOD_UNKNOWN;
}

mk_pointer mk_http_method_check_str(int method)
{
    switch (method) {
    case MK_HTTP_METHOD_GET:
        return mk_http_method_get_p;
    case MK_HTTP_METHOD_POST:
        return mk_http_method_post_p;
    case MK_HTTP_METHOD_HEAD:
        return mk_http_method_head_p;
    case MK_HTTP_METHOD_PUT:
        return mk_http_method_put_p;
    case MK_HTTP_METHOD_DELETE:
        return mk_http_method_delete_p;
    case MK_HTTP_METHOD_OPTIONS:
        return mk_http_method_options_p;
    }
    return mk_http_method_null_p;
}

static int mk_http_range_set(struct session_request *sr, long file_size)
{
    struct response_headers *sh = &sr->headers;

    sr->bytes_to_send = file_size;
    sr->bytes_offset = 0;

    if (config->resume == MK_TRUE && sr->range.data) {
        /* yyy- */
        if (sh->ranges[0] >= 0 && sh->ranges[1] == -1) {
            sr->bytes_offset = sh->ranges[0];
            sr->bytes_to_send = file_size - sr->bytes_offset;
        }

        /* yyy-xxx */
        if (sh->ranges[0] >= 0 && sh->ranges[1] >= 0) {
            sr->bytes_offset = sh->ranges[0];
            sr->bytes_to_send = labs(sh->ranges[1] - sh->ranges[0]) + 1;
        }

        /* -xxx */
        if (sh->ranges[0] == -1 && sh->ranges[1] > 0) {
            sr->bytes_to_send = sh->ranges[1];
            sr->bytes_offset = file_size - sh->ranges[1];
        }

        if (sr->bytes_offset >= file_size || sr->bytes_to_send > file_size) {
            return -1;
        }

        lseek(sr->fd_file, sr->bytes_offset, SEEK_SET);
    }
    return 0;
}

static int mk_http_range_parse(struct session_request *sr)
{
    int eq_pos, sep_pos, len;
    char *buffer = 0;
    struct response_headers *sh;

    if (!sr->range.data)
        return -1;

    if ((eq_pos = mk_string_char_search(sr->range.data, '=', sr->range.len)) < 0)
        return -1;

    if (strncasecmp(sr->range.data, "Bytes", eq_pos) != 0)
        return -1;

    if ((sep_pos = mk_string_char_search(sr->range.data, '-', sr->range.len)) < 0)
        return -1;

    len = sr->range.len;
    sh = &sr->headers;

    /* =-xxx */
    if (eq_pos + 1 == sep_pos) {
        sh->ranges[0] = -1;
        sh->ranges[1] = (unsigned long) atol(sr->range.data + sep_pos + 1);

        if (sh->ranges[1] <= 0) {
            return -1;
        }

        sh->content_length = sh->ranges[1];
        return 0;
    }

    /* =yyy-xxx */
    if ((eq_pos + 1 != sep_pos) && (len > sep_pos + 1)) {
        buffer = mk_string_copy_substr(sr->range.data, eq_pos + 1, sep_pos);
        sh->ranges[0] = (unsigned long) atol(buffer);
        mk_mem_free(buffer);

        buffer = mk_string_copy_substr(sr->range.data, sep_pos + 1, len);
        sh->ranges[1] = (unsigned long) atol(buffer);
        mk_mem_free(buffer);

        if (sh->ranges[1] < 0 || (sh->ranges[0] > sh->ranges[1])) {
            return -1;
        }

        sh->content_length = abs(sh->ranges[1] - sh->ranges[0]) + 1;
        return 0;
    }
    /* =yyy- */
    if ((eq_pos + 1 != sep_pos) && (len == sep_pos + 1)) {
        buffer = mk_string_copy_substr(sr->range.data, eq_pos + 1, len);
        sr->headers.ranges[0] = (unsigned long) atol(buffer);
        mk_mem_free(buffer);

        sh->content_length = (sh->content_length - sh->ranges[0]);
        return 0;
    }

    return -1;
}

int mk_http_method_get(char *body)
{
    int int_method, pos = 0;
    int max_len_method = 8;
    mk_pointer method;

    /* Max method length is 7 (GET/POST/HEAD/PUT/DELETE/OPTIONS) */
    pos = mk_string_char_search(body, ' ', max_len_method);
    if (mk_unlikely(pos <= 2 || pos >= max_len_method)) {
        return MK_HTTP_METHOD_UNKNOWN;
    }

    method.data = body;
    method.len = (unsigned long) pos;

    int_method = mk_http_method_check(method);

    return int_method;
}

int mk_http_protocol_check(char *protocol, int len)
{
    if (strncmp(protocol, MK_HTTP_PROTOCOL_11_STR, len) == 0) {
        return MK_HTTP_PROTOCOL_11;
    }
    if (strncmp(protocol, MK_HTTP_PROTOCOL_10_STR, len) == 0) {
        return MK_HTTP_PROTOCOL_10;
    }
    if (strncmp(protocol, MK_HTTP_PROTOCOL_09_STR, len) == 0) {
        return MK_HTTP_PROTOCOL_09;
    }

    return MK_HTTP_PROTOCOL_UNKNOWN;
}

mk_pointer mk_http_protocol_check_str(int protocol)
{
    if (protocol == MK_HTTP_PROTOCOL_11) {
        return mk_http_protocol_11_p;
    }
    if (protocol == MK_HTTP_PROTOCOL_10) {
        return mk_http_protocol_10_p;
    }
    if (protocol == MK_HTTP_PROTOCOL_09) {
        return mk_http_protocol_09_p;
    }

    return mk_http_protocol_null_p;
}

static int mk_http_directory_redirect_check(struct client_session *cs,
                                            struct session_request *sr)
{
    int port_redirect = 0;
    char *host;
    char *location = 0;
    char *real_location = 0;
    unsigned long len;

    /*
     * We have to check if exist an slash to the end of
     * this string, if doesn't exist we send a redirection header
     */
    if (sr->uri_processed.data[sr->uri_processed.len - 1] == '/') {
        return 0;
    }

    host = mk_pointer_to_buf(sr->host);

    /*
     * Add ending slash to the location string
     */
    location = mk_mem_malloc(sr->uri_processed.len + 2);
    memcpy(location, sr->uri_processed.data, sr->uri_processed.len);
    location[sr->uri_processed.len]     = '/';
    location[sr->uri_processed.len + 1] = '\0';

    /* FIXME: should we done something similar for SSL = 443 */
    if (sr->host.data && sr->port > 0) {
        if (sr->port != config->standard_port) {
            port_redirect = sr->port;
        }
    }

    if (port_redirect > 0) {
        mk_string_build(&real_location, &len, "%s://%s:%i%s",
                        config->transport, host, port_redirect, location);
    }
    else {
        mk_string_build(&real_location, &len, "%s://%s%s",
                        config->transport, host, location);
    }

#ifdef TRACE
    MK_TRACE("Redirecting to '%s'", real_location);
#endif

    mk_mem_free(host);

    mk_header_set_http_status(sr, MK_REDIR_MOVED);
    sr->headers.content_length = 0;

    mk_pointer_reset(&sr->headers.content_type);
    sr->headers.location = real_location;
    sr->headers.cgi = SH_NOCGI;
    sr->headers.pconnections_left =
        (config->max_keep_alive_request - cs->counter_connections);

    mk_header_send(cs->socket, cs, sr);
    mk_socket_set_cork_flag(cs->socket, TCP_CORK_OFF);

    /*
     *  we do not free() real_location
     *  as it's freed by iov
     */
    mk_mem_free(location);
    sr->headers.location = NULL;
    return -1;
}

int mk_http_init(struct client_session *cs, struct session_request *sr)
{
    int ret;
    int bytes = 0;
    struct mimetype *mime;

    MK_TRACE("HTTP Protocol Init");

    /* Request to root path of the virtualhost in question */
    if (sr->uri_processed.len == 1 && sr->uri_processed.data[0] == '/') {
        sr->real_path.data = sr->host_conf->documentroot.data;
        sr->real_path.len = sr->host_conf->documentroot.len;
    }

    /* Compose real path */
    if (sr->user_home == MK_FALSE) {
        int len;

        len = sr->host_conf->documentroot.len + sr->uri_processed.len;
        if (len < MK_PATH_BASE) {
            memcpy(sr->real_path_static,
                   sr->host_conf->documentroot.data,
                   sr->host_conf->documentroot.len);
            memcpy(sr->real_path_static + sr->host_conf->documentroot.len,
                   sr->uri_processed.data,
                   sr->uri_processed.len);
            sr->real_path_static[len] = '\0';
            sr->real_path.data = sr->real_path_static;
            sr->real_path.len = len;
        }
        else {
            ret = mk_buffer_cat(&sr->real_path,
                                sr->host_conf->documentroot.data,
                                sr->host_conf->documentroot.len,
                                sr->uri_processed.data,
                                sr->uri_processed.len);

            if (ret < 0) {
                MK_TRACE("Error composing real path");
                return EXIT_ERROR;
            }
        }
    }

    /* Check backward directory request */
    if (memmem(sr->uri_processed.data, sr->uri_processed.len,
               MK_HTTP_DIRECTORY_BACKWARD,
               sizeof(MK_HTTP_DIRECTORY_BACKWARD) - 1)) {
        return mk_request_error(MK_CLIENT_FORBIDDEN, cs, sr);
    }


    if (mk_file_get_info(sr->real_path.data, &sr->file_info) != 0) {
        /* if the resource requested doesn't exists, let's
         * check if some plugin would like to handle it
         */
        MK_TRACE("No file, look for handler plugin");
        ret = mk_plugin_stage_run(MK_PLUGIN_STAGE_30, cs->socket, NULL, cs, sr);
        if (ret == MK_PLUGIN_RET_CLOSE_CONX) {
            if (sr->headers.status > 0) {
                return mk_request_error(sr->headers.status, cs, sr);
            }
            else {
                return mk_request_error(MK_CLIENT_FORBIDDEN, cs, sr);
            }
        }
        else if (ret == MK_PLUGIN_RET_CONTINUE) {
            return MK_PLUGIN_RET_CONTINUE;
        }
        else if (ret == MK_PLUGIN_RET_END) {
            return EXIT_NORMAL;
        }

        if (sr->file_info.exists == MK_FALSE) {
            return mk_request_error(MK_CLIENT_NOT_FOUND, cs, sr);
        }
        else if (sr->stage30_blocked == MK_FALSE) {
            return mk_request_error(MK_CLIENT_FORBIDDEN, cs, sr);
        }
    }

    /* is it a valid directory ? */
    if (sr->file_info.is_directory == MK_TRUE) {
        /* Send redirect header if end slash is not found */
        if (mk_http_directory_redirect_check(cs, sr) == -1) {
            MK_TRACE("Directory Redirect");

            /* Redirect has been sent */
            return -1;
        }

        /* looking for a index file */
        mk_pointer index_file;
        char tmppath[MK_MAX_PATH];
        index_file = mk_request_index(sr->real_path.data, tmppath, MK_MAX_PATH);

        if (index_file.data) {
            if (sr->real_path.data != sr->real_path_static) {
                mk_pointer_free(&sr->real_path);
                sr->real_path = index_file;
                sr->real_path.data = mk_string_dup(index_file.data);
            }
            /* If it's static, and still fits */
            else if (index_file.len < MK_PATH_BASE) {
                memcpy(sr->real_path_static, index_file.data, index_file.len);
                sr->real_path_static[index_file.len] = '\0';
                sr->real_path.len = index_file.len;
            }
            /* It was static, but didn't fit */
            else {
                sr->real_path = index_file;
                sr->real_path.data = mk_string_dup(index_file.data);
            }

            mk_file_get_info(sr->real_path.data, &sr->file_info);
        }
    }

    /* Check symbolic link file */
    if (sr->file_info.is_link == MK_TRUE) {
        if (config->symlink == MK_FALSE) {
            return mk_request_error(MK_CLIENT_FORBIDDEN, cs, sr);
        }
        else {
            int n;
            char linked_file[MK_MAX_PATH];
            n = readlink(sr->real_path.data, linked_file, MK_MAX_PATH);
            if (n < 0) {
                return mk_request_error(MK_CLIENT_FORBIDDEN, cs, sr);
            }
        }
    }

    /* Plugin Stage 30: look for handlers for this request */
    if (sr->stage30_blocked == MK_FALSE) {
        ret  = mk_plugin_stage_run(MK_PLUGIN_STAGE_30, cs->socket, NULL, cs, sr);
        MK_TRACE("[FD %i] STAGE_30 returned %i", cs->socket, ret);
        switch (ret) {
        case MK_PLUGIN_RET_CONTINUE:
            return MK_PLUGIN_RET_CONTINUE;
        case MK_PLUGIN_RET_CLOSE_CONX:
            if (sr->headers.status > 0) {
                return mk_request_error(sr->headers.status, cs, sr);
            }
            else {
                return mk_request_error(MK_CLIENT_FORBIDDEN, cs, sr);
            }
        case MK_PLUGIN_RET_END:
            return EXIT_NORMAL;
        }
    }

    /*
     * Monkey listen for PUT and DELETE methods in addition to GET, POST and
     * HEAD, but it does not care about them, so if any plugin did not worked
     * on it, Monkey will return error 501 (501 Not Implemented).
     */
    if (sr->method == MK_HTTP_METHOD_PUT || sr->method == MK_HTTP_METHOD_DELETE ||
        sr->method == MK_HTTP_METHOD_UNKNOWN) {
        return mk_request_error(MK_SERVER_NOT_IMPLEMENTED, cs, sr);
    }

    /* counter connections */
    sr->headers.pconnections_left = (int)
        (config->max_keep_alive_request - cs->counter_connections);

    /* Set default value */
    mk_header_set_http_status(sr, MK_HTTP_OK);
    sr->headers.location = NULL;
    sr->headers.content_length = 0;

    /*
     * For OPTIONS method, we let the plugin handle it and
     * return without any content.
     */
    if (sr->method == MK_HTTP_METHOD_OPTIONS) {
        sr->headers.allow_methods.data = MK_HTTP_METHOD_AVAILABLE;
        sr->headers.allow_methods.len = strlen(MK_HTTP_METHOD_AVAILABLE);

        mk_pointer_reset(&sr->headers.content_type);
        mk_header_send(cs->socket, cs, sr);
        return EXIT_NORMAL;
    }
    else {
        mk_pointer_reset(&sr->headers.allow_methods);
    }

    /* read permissions and check file */
    if (sr->file_info.read_access == MK_FALSE) {
        return mk_request_error(MK_CLIENT_FORBIDDEN, cs, sr);
    }

    /* Matching MimeType  */
    mime = mk_mimetype_find(&sr->real_path);
    if (!mime) {
        mime = mimetype_default;
    }

    if (sr->file_info.is_directory == MK_TRUE) {
        return mk_request_error(MK_CLIENT_FORBIDDEN, cs, sr);
    }

    /* get file size */
    if (sr->file_info.size < 0) {
        return mk_request_error(MK_CLIENT_NOT_FOUND, cs, sr);
    }

    sr->headers.last_modified = sr->file_info.last_modification;

    if (sr->if_modified_since.data && sr->method == MK_HTTP_METHOD_GET) {
        time_t date_client;       /* Date sent by client */
        time_t date_file_server;  /* Date server file */

        date_client = mk_utils_gmt2utime(sr->if_modified_since.data);
        date_file_server = sr->file_info.last_modification;

        if (date_file_server <= date_client && date_client > 0 &&
            date_client <= log_current_utime) {

            mk_header_set_http_status(sr, MK_NOT_MODIFIED);
            mk_header_send(cs->socket, cs, sr);
            return EXIT_NORMAL;
        }
    }

    /* Object size for log and response headers */
    sr->headers.content_length = sr->file_info.size;
    sr->headers.real_length = sr->file_info.size;

    /* Open file */
    if (mk_likely(sr->file_info.size > 0)) {
        sr->fd_file = mk_vhost_open(sr);
        if (sr->fd_file == -1) {
            MK_TRACE("open() failed");
            return mk_request_error(MK_CLIENT_FORBIDDEN, cs, sr);
        }
        sr->bytes_to_send = sr->file_info.size;
    }

    /* Process methods */
    if (sr->method == MK_HTTP_METHOD_GET || sr->method == MK_HTTP_METHOD_HEAD) {
        sr->headers.content_type = mime->type;

        /* HTTP Ranges */
        if (sr->range.data != NULL && config->resume == MK_TRUE) {
            if (mk_http_range_parse(sr) < 0) {
                sr->headers.ranges[0] = -1;
                sr->headers.ranges[1] = -1;
                return mk_request_error(MK_CLIENT_BAD_REQUEST, cs, sr);
            }
            if (sr->headers.ranges[0] >= 0 || sr->headers.ranges[1] >= 0) {
                mk_header_set_http_status(sr, MK_HTTP_PARTIAL);
            }

            /* Calc bytes to send & offset */
            if (mk_http_range_set(sr, sr->file_info.size) != 0) {
                sr->headers.content_length = -1;
                sr->headers.ranges[0] = -1;
                sr->headers.ranges[1] = -1;
                return mk_request_error(MK_CLIENT_REQUESTED_RANGE_NOT_SATISF, cs, sr);
            }
        }
    }
    else {
        /* without content-type */
        mk_pointer_reset(&sr->headers.content_type);
    }

    /* Send headers */
    mk_header_send(cs->socket, cs, sr);

    if (mk_unlikely(sr->headers.content_length == 0)) {
        return 0;
    }

    /* Send file content */
    if (sr->method == MK_HTTP_METHOD_GET || sr->method == MK_HTTP_METHOD_POST) {
        bytes = mk_http_send_file(cs, sr);
    }

    return bytes;
}

int mk_http_send_file(struct client_session *cs, struct session_request *sr)
{
    long int nbytes = 0;

    nbytes = mk_socket_send_file(cs->socket, sr->fd_file,
                                 &sr->bytes_offset, sr->bytes_to_send);

    if (nbytes > 0) {
        sr->bytes_to_send -= nbytes;
        if (sr->bytes_to_send == 0) {
            mk_socket_set_cork_flag(cs->socket, TCP_CORK_OFF);
        }
    }

    sr->loop++;

    /*
     * In some circumstances when writing data the connection can get broken,
     * so we must be aware of that.
     *
     * Also, if for some reason the file that is being serve change it size
     * we can get a zero bytes send as return value. We need to validate the
     * return values <= zero
     */
    if (mk_unlikely(nbytes <= 0)) {
        MK_TRACE("sendfile() = -1;");
        return EXIT_ABORT;
    }

    return sr->bytes_to_send;
}

/*
 * Check if a connection can continue open using as criteria
 * the keepalive headers vars and Monkey configuration
 */
int mk_http_keepalive_check(struct client_session *cs)
{
    struct session_request *sr_node;
    struct mk_list *sr_head;

    if (mk_list_is_empty(&cs->request_list) == 0) {
        return -1;
    }

    sr_head = &cs->request_list;
    sr_node = mk_list_entry_last(sr_head, struct session_request, _head);
    if (config->keep_alive == MK_FALSE || sr_node->keep_alive == MK_FALSE) {
        return -1;
    }

    /* Old client without Connection header */
    if (sr_node->protocol < MK_HTTP_PROTOCOL_11 && sr_node->connection.len <= 0) {
        return -1;
    }

    /* Old client and content length to send is unknown */
    if (sr_node->protocol < MK_HTTP_PROTOCOL_11 && sr_node->headers.content_length <= 0) {
        return -1;
    }

    /* Connection was forced to close */
    if (sr_node->close_now == MK_TRUE) {
        return -1;
    }

    /* Client has reached keep-alive connections limit */
    if (cs->counter_connections >= config->max_keep_alive_request) {
        return -1;
    }

    return 0;
}

/*
 * Check if client request still has pending data
 *
 * Return 0 when all expected data has arrived or -1 when
 * the connection is on a pending status due to HTTP spec
 *
 * This function is called from request.c :: mk_handler_read(..)
 */
int mk_http_pending_request(struct client_session *cs)
{
    int n;
    char *end;

    if (cs->body_length >= mk_endblock.len) {
        end = (cs->body + cs->body_length) - mk_endblock.len;
    }
    else {
        return -1;
    }

    /* try to match CRLF at the end of the request */
    if (cs->body_pos_end < 0) {
        if (strncmp(end, mk_endblock.data, mk_endblock.len) == 0) {
            cs->body_pos_end = cs->body_length - mk_endblock.len;
        }
        else if ((n = mk_string_search(cs->body, mk_endblock.data, MK_STR_SENSITIVE)) >= 0 ){
            cs->body_pos_end = n;
        }
        else {
            return -1;
        }
    }

    if (cs->first_method == MK_HTTP_METHOD_UNKNOWN) {
        cs->first_method = mk_http_method_get(cs->body);
    }

    if (cs->first_method == MK_HTTP_METHOD_POST || cs->first_method == MK_HTTP_METHOD_PUT) {
        if (cs->body_pos_end > 0) {
            int content_length;
            int current;

            current = cs->body_length - cs->body_pos_end - mk_endblock.len;
            content_length = mk_method_validate_content_length(cs->body, current);

            MK_TRACE("HTTP DATA %i/%i", current, content_length);

            if (content_length >= config->max_request_size) {
                return 0;
            }

            /* if first block has ended, we need to verify if exists
             * a previous block end, that will means that the POST
             * method has sent the whole information.
             * just for ref: pipelining is not allowed with POST
             */
            if ((unsigned int) cs->body_pos_end == cs->body_length - mk_endblock.len) {
                /* Content-length is required, if is it not found,
                 * we pass as successfull in order to raise the error
                 * later
                 */
                if (content_length <= 0) {
                    cs->status = MK_REQUEST_STATUS_COMPLETED;
                    return 0;
                }
                else {
                    return -1;
                }
            }
            else {
                if (current < content_length) {
                    return -1;
                }
                else {
                    cs->status = MK_REQUEST_STATUS_COMPLETED;
                    return 0;
                }
            }
        }
        else {
            return -1;
        }
    }

    cs->status = MK_REQUEST_STATUS_COMPLETED;
    return 0;
}

int mk_http_request_end(int socket)
{
    int ka;
    struct client_session *cs;
    struct sched_list_node *sched;

    sched = mk_sched_get_thread_conf();
    cs = mk_session_get(socket);

    if (!cs) {
        MK_TRACE("[FD %i] Not found", socket);
        return -1;
    }

    if (mk_unlikely(!sched)) {
        MK_TRACE("Could not find sched list node :/");
        return -1;
    }

    /*
     * We need to ask to http_keepalive if this
     * connection can continue working or we must
     * close it.
     */
    ka = mk_http_keepalive_check(cs);
    mk_request_free_list(cs);

    if (ka < 0) {
        MK_TRACE("[FD %i] No KeepAlive mode, remove", socket);
        mk_session_remove(socket);
    }
    else {
        mk_request_ka_next(cs);
        mk_epoll_change_mode(sched->epoll_fd,
                             socket, MK_EPOLL_READ, MK_EPOLL_LEVEL_TRIGGERED);
        return 0;
    }

    return -1;
}
