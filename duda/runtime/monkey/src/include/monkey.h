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

#ifndef MK_MONKEY_H
#define MK_MONKEY_H

#include <pthread.h>
#include <sys/types.h>
#include <netinet/in.h>
#include "mk_memory.h"

/* Max Path lenth */
#define MK_MAX_PATH 1024

/* Send_Header(...,int cgi) */
#define SH_NOCGI 0
#define SH_CGI 1

/* Monkey Protocol */
extern const mk_pointer mk_monkey_protocol;

/* Process UID/GID */
extern gid_t EGID;
extern gid_t EUID;

void mk_details(void);
void mk_thread_keys_init();

#endif
