/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Monkey HTTP Daemon
 *  ------------------
 *  Copyright (C) 2001-2009, Eduardo Silva P. <edsiper@gmail.com>
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
 *  MA 02110-1301  USA
 */

#include "logger.h"
#include "pointers.h"
#include "mk_plugin.h"

const mk_pointer mk_iov_none = mk_pointer_init("");

/* Writter helpers */
const mk_pointer mk_logger_iov_dash = mk_pointer_init(MK_LOGGER_IOV_DASH);
const mk_pointer mk_logger_iov_space = mk_pointer_init(MK_IOV_SPACE);
const mk_pointer mk_logger_iov_lf = mk_pointer_init(MK_IOV_LF);
const mk_pointer mk_logger_iov_empty = mk_pointer_init(MK_LOGGER_IOV_EMPTY);

/* Error messages */
const mk_pointer error_msg_400 = mk_pointer_init(ERROR_MSG_400);
const mk_pointer error_msg_403 = mk_pointer_init(ERROR_MSG_403);
const mk_pointer error_msg_404 = mk_pointer_init(ERROR_MSG_404);
const mk_pointer error_msg_405 = mk_pointer_init(ERROR_MSG_405);
const mk_pointer error_msg_408 = mk_pointer_init(ERROR_MSG_408);
const mk_pointer error_msg_411 = mk_pointer_init(ERROR_MSG_411);
const mk_pointer error_msg_413 = mk_pointer_init(ERROR_MSG_413);
const mk_pointer error_msg_500 = mk_pointer_init(ERROR_MSG_500);
const mk_pointer error_msg_501 = mk_pointer_init(ERROR_MSG_501);
const mk_pointer error_msg_505 = mk_pointer_init(ERROR_MSG_505);
