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

/* s_log status */
#ifndef MK_LOGGER_H
#define MK_LOGGER_H

#include "MKPlugin.h"
#include <stdio.h>

#define MK_LOGGER_PIPE_LIMIT 0.75
#define MK_LOGGER_TIMEOUT_DEFAULT 3

int mk_logger_timeout;

/* MasterLog variables */
char *mk_logger_master_path;
FILE *mk_logger_master_stdout;
FILE *mk_logger_master_stderr;

pthread_key_t cache_content_length;
pthread_key_t cache_status;
pthread_key_t cache_ip_str;

struct log_target
{
    /* Pipes */
    int fd_access[2];
    int fd_error[2];

    /* File paths */
    char *file_access;
    char *file_error;

    struct host *host;
    struct mk_list _head;
};

struct mk_list targets_list;


#endif
