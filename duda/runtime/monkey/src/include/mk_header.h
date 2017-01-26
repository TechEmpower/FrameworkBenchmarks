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

#ifndef MK_HEADER_H
#define MK_HEADER_H

#include "mk_request.h"
#include "mk_http_status.h"

#define MK_HEADER_BREAKLINE 1

/*
 * header response: We handle this as static global data in order
 * to save some process time when building the response header.
 */

/* Informational */
#define MK_RH_INFO_CONTINUE "HTTP/1.1 100 Continue\r\n"
#define MK_RH_INFO_SWITCH_PROTOCOL "HTTP/1.1 101 Switching Protocols\r\n"

/* Successfull */
#define MK_RH_HTTP_OK "HTTP/1.1 200 OK\r\n"
#define MK_RH_HTTP_CREATED "HTTP/1.1 201 Created\r\n"
#define MK_RH_HTTP_ACCEPTED "HTTP/1.1 202 Accepted\r\n"
#define MK_RH_HTTP_NON_AUTH_INFO "HTTP/1.1 203 Non-Authoritative Information\r\n"
#define MK_RH_HTTP_NOCONTENT "HTTP/1.1 204 No Content\r\n"
#define MK_RH_HTTP_RESET "HTTP/1.1 205 Reset Content\r\n"
#define MK_RH_HTTP_PARTIAL "HTTP/1.1 206 Partial Content\r\n"

/* Redirections */
#define MK_RH_REDIR_MULTIPLE "HTTP/1.1 300 Multiple Choices\r\n"
#define MK_RH_REDIR_MOVED "HTTP/1.1 301 Moved Permanently\r\n"
#define MK_RH_REDIR_MOVED_T "HTTP/1.1 302 Found\r\n"
#define	MK_RH_REDIR_SEE_OTHER "HTTP/1.1 303 See Other\r\n"
#define MK_RH_NOT_MODIFIED "HTTP/1.1 304 Not Modified\r\n"
#define MK_RH_REDIR_USE_PROXY "HTTP/1.1 305 Use Proxy\r\n"

/* Client side errors */
#define MK_RH_CLIENT_BAD_REQUEST "HTTP/1.1 400 Bad Request\r\n"
#define MK_RH_CLIENT_UNAUTH "HTTP/1.1 401 Unauthorized\r\n"
#define MK_RH_CLIENT_PAYMENT_REQ "HTTP/1.1 402 Payment Required\r\n"
#define MK_RH_CLIENT_FORBIDDEN "HTTP/1.1 403 Forbidden\r\n"
#define MK_RH_CLIENT_NOT_FOUND "HTTP/1.1 404 Not Found\r\n"
#define MK_RH_CLIENT_METHOD_NOT_ALLOWED "HTTP/1.1 405 Method Not Allowed\r\n"
#define MK_RH_CLIENT_NOT_ACCEPTABLE "HTTP/1.1 406 Not Acceptable\r\n"
#define MK_RH_CLIENT_PROXY_AUTH "HTTP/1.1 407 Proxy Authentication Required\r\n"
#define MK_RH_CLIENT_REQUEST_TIMEOUT "HTTP/1.1 408 Request Timeout\r\n"
#define MK_RH_CLIENT_CONFLICT "HTTP/1.1 409 Conflict\r\n"
#define MK_RH_CLIENT_GONE "HTTP/1.1 410 Gone\r\n"
#define MK_RH_CLIENT_LENGTH_REQUIRED "HTTP/1.1 411 Length Required\r\n"
#define MK_RH_CLIENT_PRECOND_FAILED "HTTP/1.1 412 Precondition Failed\r\n"
#define MK_RH_CLIENT_REQUEST_ENTITY_TOO_LARGE   \
    "HTTP/1.1 413 Request Entity Too Large\r\n"
#define MK_RH_CLIENT_REQUEST_URI_TOO_LONG "HTTP/1.1 414 Request-URI Too Long\r\n"
#define MK_RH_CLIENT_UNSUPPORTED_MEDIA  "HTTP/1.1 415 Unsupported Media Type\r\n"
#define MK_RH_CLIENT_REQUESTED_RANGE_NOT_SATISF \
    "HTTP/1.1 416 Requested Range Not Satisfiable\r\n"

/* Server side errors */
#define MK_RH_SERVER_INTERNAL_ERROR "HTTP/1.1 500 Internal Server Error\r\n"
#define MK_RH_SERVER_NOT_IMPLEMENTED "HTTP/1.1 501 Not Implemented\r\n"
#define MK_RH_SERVER_BAD_GATEWAY "HTTP/1.1 502 Bad Gateway\r\n"
#define MK_RH_SERVER_SERVICE_UNAV "HTTP/1.1 503 Service Unavailable\r\n"
#define MK_RH_SERVER_GATEWAY_TIMEOUT "HTTP/1.1 504 Gateway Timeout\r\n"
#define MK_RH_SERVER_HTTP_VERSION_UNSUP "HTTP/1.1 505 HTTP Version Not Supported\r\n"

struct header_status_response {
    int   status;
    int   length;
    char *response;
};

/* Short header values */
#define MK_HEADER_SHORT_DATE "Date: "
#define MK_HEADER_SHORT_LOCATION "Location: "
#define MK_HEADER_SHORT_CT "Content-Type: "

/* Accept ranges */
#define MK_HEADER_ACCEPT_RANGES "Accept-Ranges: bytes" MK_CRLF

/* Allowed methods */
#define MK_HEADER_ALLOWED_METHODS "Allow: "

#define MK_HEADER_CONN_KA "Connection: Keep-Alive" MK_CRLF
#define MK_HEADER_CONN_CLOSE "Connection: Close" MK_CRLF
#define MK_HEADER_CONTENT_LENGTH "Content-Length: "
#define MK_HEADER_CONTENT_ENCODING "Content-Encoding: "

/* Transfer Encoding */
#define MK_HEADER_TE_TYPE_CHUNKED 0
#define MK_HEADER_TE_CHUNKED "Transfer-Encoding: Chunked" MK_CRLF

#define MK_HEADER_LAST_MODIFIED "Last-Modified: "

extern const mk_pointer mk_header_short_date;
extern const mk_pointer mk_header_short_location;
extern const mk_pointer mk_header_short_ct;

/* mk pointers with response server headers */
extern const mk_pointer mk_header_conn_ka;
extern const mk_pointer mk_header_conn_close;
extern const mk_pointer mk_header_content_length;
extern const mk_pointer mk_header_content_encoding;
extern const mk_pointer mk_header_accept_ranges;
extern const mk_pointer mk_header_te_chunked;
extern const mk_pointer mk_header_last_modified;

int mk_header_send(int fd, struct client_session *cs, struct session_request *sr);
void mk_header_response_reset(struct response_headers *header);
void mk_header_set_http_status(struct session_request *sr, int status);
void mk_header_set_content_length(struct session_request *sr, long len);

#endif
