/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Monkey HTTP Daemon
 *  ------------------
 *  Copyright (C) 2001-2012, Eduardo Silva P. <edsiper@gmail.com>
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

#ifndef MK_LOGGER_POINTERS_H
#define MK_LOGGER_POINTERS_H

#include <memory.h>

/* Request error messages for log file */
#define ERROR_MSG_400 "[error 400] Bad Request"
#define ERROR_MSG_403 "[error 403] Forbidden"
#define ERROR_MSG_404 "[error 404] Not Found"
#define ERROR_MSG_405 "[error 405] Method Not Allowed"
#define ERROR_MSG_408 "[error 408] Request Timeout"
#define ERROR_MSG_411 "[error 411] Length Required"
#define ERROR_MSG_413 "[error 413] Request Entity Too Large"
#define ERROR_MSG_500 "[error 500] Internal Server Error"
#define ERROR_MSG_501 "[error 501] Not Implemented"
#define ERROR_MSG_505 "[error 505] HTTP Version Not Supported"

#define MK_LOGGER_IOV_DASH " - "
#define MK_LOGGER_IOV_SPACE " "
#define MK_LOGGER_IOV_EMPTY "-"

/* mk pointers for errors */
extern const mk_pointer error_msg_400;
extern const mk_pointer error_msg_403;
extern const mk_pointer error_msg_404;
extern const mk_pointer error_msg_405;
extern const mk_pointer error_msg_408;
extern const mk_pointer error_msg_411;
extern const mk_pointer error_msg_413;
extern const mk_pointer error_msg_500;
extern const mk_pointer error_msg_501;
extern const mk_pointer error_msg_505;

/* mk pointer for IOV */
extern const mk_pointer mk_logger_iov_dash;
extern const mk_pointer mk_logger_iov_space;
extern const mk_pointer mk_logger_iov_lf;
extern const mk_pointer mk_logger_iov_empty;

#endif
