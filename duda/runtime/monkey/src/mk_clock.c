/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Monkey HTTP Daemon
 *  ------------------
 *  Copyright (C) 2001-2012, Eduardo Silva P. <edsiper@gmail.com>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>
#include <unistd.h>

#include "mk_memory.h"
#include "mk_clock.h"
#include "mk_utils.h"

time_t log_current_utime;
time_t monkey_init_time;

mk_pointer log_current_time = { NULL, LOG_TIME_BUFFER_SIZE - 2 };
mk_pointer header_current_time = { NULL, HEADER_TIME_BUFFER_SIZE - 1 };

static char *log_time_buffers[2];
static char *header_time_buffers[2];

/* The mk_pointers have two buffers for avoid in half-way access from
 * another thread while a buffer is being modified. The function below returns
 * one of two buffers to work with.
 */
static inline char *_next_buffer(mk_pointer *pointer, char **buffers)
{
    if(pointer->data == buffers[0]) {
        return buffers[1];
    } else {
        return buffers[0];
    }
}

static void mk_clock_log_set_time(time_t utime)
{
    char *time_string;
    struct tm result;

    time_string = _next_buffer(&log_current_time, log_time_buffers);
    log_current_utime = utime;

    strftime(time_string, LOG_TIME_BUFFER_SIZE, "[%d/%b/%G %T %z]",
             localtime_r(&utime, &result));

    log_current_time.data = time_string;
}

static void mk_clock_header_set_time(time_t utime)
{
    struct tm *gmt_tm;
    struct tm result;
    char *time_string;

    time_string = _next_buffer(&header_current_time, header_time_buffers);

    gmt_tm = gmtime_r(&utime, &result);
    strftime(time_string, HEADER_TIME_BUFFER_SIZE, GMT_DATEFORMAT, gmt_tm);

    header_current_time.data = time_string;
}

void *mk_clock_worker_init(void *args UNUSED_PARAM)
{
    time_t cur_time;

    mk_utils_worker_rename("monkey: clock");

    while (1) {
        cur_time = time(NULL);

        if(cur_time != ((time_t)-1)) {
            mk_clock_log_set_time(cur_time);
            mk_clock_header_set_time(cur_time);
        }

        sleep(1);
    }

    return NULL;
}

/* This function must be called before any threads are created */
void mk_clock_sequential_init()
{
    /* Time when monkey was started */
    monkey_init_time = time(NULL);

    header_time_buffers[0] = mk_mem_malloc_z(HEADER_TIME_BUFFER_SIZE);
    header_time_buffers[1] = mk_mem_malloc_z(HEADER_TIME_BUFFER_SIZE);

    log_time_buffers[0] = mk_mem_malloc_z(LOG_TIME_BUFFER_SIZE);
    log_time_buffers[1] = mk_mem_malloc_z(LOG_TIME_BUFFER_SIZE);


    /* Set the time once */
    time_t cur_time = time(NULL);

    if(cur_time != ((time_t)-1)) {
        mk_clock_log_set_time(cur_time);
        mk_clock_header_set_time(cur_time);
    }
}
