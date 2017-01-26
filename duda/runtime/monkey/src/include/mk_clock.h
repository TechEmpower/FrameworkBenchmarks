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

/* clock.h */

#ifndef MK_CLOCK_H
#define MK_CLOCK_H

#include <time.h>
#include "memory.h"

extern time_t log_current_utime;
extern time_t monkey_init_time;

extern mk_pointer log_current_time;
extern mk_pointer header_current_time;

#define GMT_DATEFORMAT "%a, %d %b %Y %H:%M:%S GMT\r\n"
#define HEADER_TIME_BUFFER_SIZE 32
#define LOG_TIME_BUFFER_SIZE 30

void *mk_clock_worker_init(void *args);
void mk_clock_set_time(void);
void mk_clock_sequential_init();

#endif
