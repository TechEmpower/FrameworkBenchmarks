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

#ifndef MK_HTTP_STATUS_H
#define MK_HTTP_STATUS_H

#include "mk_memory.h"

/*
 * - New macro names and structure by Monkey Dev Team
 * - Initial HTTP Status provided by Juan C. Inostroza <jci@codemonkey.cl>
 */

/* Monkey allow plugins to set their customized status */
#define MK_CUSTOM_STATUS                          7

/* Informational status */
#define MK_INFO_CONTINUE	                100
#define MK_INFO_SWITCH_PROTOCOL	                101

/* Succesful */
#define MK_HTTP_OK				200
#define MK_HTTP_CREATED				201
#define MK_HTTP_ACCEPTED			202
#define MK_HTTP_NON_AUTH_INFO			203
#define MK_HTTP_NOCONTENT			204
#define MK_HTTP_RESET				205
#define MK_HTTP_PARTIAL				206

/* Redirections */
#define MK_REDIR_MULTIPLE			300
#define MK_REDIR_MOVED				301
#define MK_REDIR_MOVED_T			302
#define	MK_REDIR_SEE_OTHER			303
#define MK_NOT_MODIFIED			        304
#define MK_REDIR_USE_PROXY			305

/* Client Errors */
#define MK_CLIENT_BAD_REQUEST			400
#define MK_CLIENT_UNAUTH			401
#define MK_CLIENT_PAYMENT_REQ   		402     /* Wtf?! :-) */
#define MK_CLIENT_FORBIDDEN			403
#define MK_CLIENT_NOT_FOUND			404
#define MK_CLIENT_METHOD_NOT_ALLOWED		405
#define MK_CLIENT_NOT_ACCEPTABLE		406
#define MK_CLIENT_PROXY_AUTH			407
#define MK_CLIENT_REQUEST_TIMEOUT		408
#define MK_CLIENT_CONFLICT			409
#define MK_CLIENT_GONE				410
#define MK_CLIENT_LENGTH_REQUIRED		411
#define MK_CLIENT_PRECOND_FAILED		412
#define MK_CLIENT_REQUEST_ENTITY_TOO_LARGE	413
#define MK_CLIENT_REQUEST_URI_TOO_LONG		414
#define MK_CLIENT_UNSUPPORTED_MEDIA		415
#define MK_CLIENT_REQUESTED_RANGE_NOT_SATISF    416

/* Server Errors */
#define MK_SERVER_INTERNAL_ERROR		500
#define MK_SERVER_NOT_IMPLEMENTED		501
#define MK_SERVER_BAD_GATEWAY			502
#define MK_SERVER_SERVICE_UNAV			503
#define MK_SERVER_GATEWAY_TIMEOUT		504
#define MK_SERVER_HTTP_VERSION_UNSUP		505

/* Text header messages */
#define M_HTTP_OK_TXT				"HTTP/1.1 200 OK\r\n"

#endif
