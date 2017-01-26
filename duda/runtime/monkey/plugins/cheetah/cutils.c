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
 *  MA 02110-1301  USA
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pwd.h>
#include <stdarg.h>
#include <string.h>

#include "MKPlugin.h"
#include "cheetah.h"
#include "cutils.h"

void mk_cheetah_print_worker_memory_usage(pid_t pid)
{
    int s = 1024;
    char *buf;
    pid_t ppid;
    FILE *f;

    ppid = getpid();
    buf = mk_api->mem_alloc(s);
    sprintf(buf, MK_CHEETAH_PROC_TASK, ppid, pid);

    f = fopen(buf, "r");
    if (!f) {
        CHEETAH_WRITE("Cannot get details\n");
        return;
    }

    buf = fgets(buf, s, f);
    fclose(f);
    if (!buf) {
        CHEETAH_WRITE("Cannot format details\n");
        return;
    }

    CHEETAH_WRITE("\n");
    return;

/*
    int n, c;
    int init = 0;
    int last = 0;
    char *value;

    while ((n = mk_api->str_search(buf + last, " ", MK_STR_SENSITIVE)) > 0) {
        if (c == 23) {
            value = mk_api->str_copy_substr(buf, init, last + n);
            printf("%s\n", value);
            mk_mem_free(buf);
            mk_mem_free(value);
            return;
        }
        init = last + n + 1;
        last += n + 1;
        c++;
    }*/
}

void mk_cheetah_print_running_user()
{
    struct passwd pwd;
    struct passwd *result;
    char *buf;
    long bufsize;
    uid_t uid;

    bufsize = sysconf(_SC_GETPW_R_SIZE_MAX);
    if (bufsize == -1) {
        bufsize = 16384;
    }

    buf = mk_api->mem_alloc_z(bufsize);
    uid = getuid();
    getpwuid_r(uid, &pwd, buf, bufsize, &result);

    CHEETAH_WRITE("%s\n", pwd.pw_name);
    mk_api->mem_free(buf);
}

int mk_cheetah_write(const char *format, ...)
{
    int len = 0;
    char buf[1024];
    va_list ap;

    va_start(ap, format);
    len = vsprintf(buf, format, ap);

    if (listen_mode == LISTEN_STDIN) {
        len = fprintf(cheetah_output, buf, NULL);
    }
    else if (listen_mode == LISTEN_SERVER) {
        len = write(cheetah_socket, buf, len);
    }

    memset(buf, '\0', sizeof(buf));
    va_end(ap);

    return len;
}
