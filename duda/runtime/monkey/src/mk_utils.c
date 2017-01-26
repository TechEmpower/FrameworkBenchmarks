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
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include <ctype.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <inttypes.h>
#include <sys/prctl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/sendfile.h>

/* stacktrace */
#include <dlfcn.h>
#ifndef NO_BACKTRACE
#include <execinfo.h>
#endif

/* local headers */
#include "monkey.h"
#include "mk_memory.h"
#include "mk_utils.h"
#include "mk_file.h"
#include "mk_string.h"
#include "mk_config.h"
#include "mk_socket.h"
#include "mk_clock.h"
#include "mk_user.h"
#include "mk_cache.h"
#include "mk_macros.h"

/* Date helpers */
static const char mk_date_wd[][6]  = {"Sun, ", "Mon, ", "Tue, ", "Wed, ", "Thu, ", "Fri, ", "Sat, "};
static const char mk_date_ym[][5] = {"Jan ", "Feb ", "Mar ", "Apr ", "May ", "Jun ", "Jul ",
                                     "Aug ", "Sep ", "Oct ", "Nov ", "Dec "};

static int mk_utils_gmt_cache_get(char **data, time_t date)
{
    unsigned int i;
    struct mk_gmt_cache *gcache = mk_cache_get(mk_cache_utils_gmt_text);

    if (mk_unlikely(!gcache)) {
        return MK_FALSE;
    }

    for (i = 0; i < MK_GMT_CACHES; i++) {
        if (date == gcache[i].time) {
            memcpy(*data, gcache[i].text, 32);
            gcache[i].hits++;
            return MK_TRUE;
        }
    }

    return MK_FALSE;
}

static void mk_utils_gmt_cache_add(char *data, time_t time)
{
    unsigned int i, min = 0;
    struct mk_gmt_cache *gcache = mk_cache_get(mk_cache_utils_gmt_text);

    for (i = 1; i < MK_GMT_CACHES; i++) {
        if (gcache[i].hits < gcache[min].hits)
            min = i;
    }

    gcache[min].hits = 1;
    gcache[min].time = time;
    memcpy(gcache[min].text, data, 32);
}

/*
 *This function given a unix time, set in a mk_pointer
 * the date in the RFC1123 format like:
 *
 *    Wed, 23 Jun 2010 22:32:01 GMT
 *
 * it also adds a 'CRLF' at the end
 */
int mk_utils_utime2gmt(char **data, time_t date)
{
    const int size = 31;
    unsigned short year, mday, hour, min, sec;
    char *buf=0;
    struct tm *gtm;

    if (date == 0) {
        if ((date = time(NULL)) == -1) {
            return -1;
        }
    }
    else {
        /* Maybe it's converted already? */
        if (mk_utils_gmt_cache_get(data, date) == MK_TRUE) {
            return size;
        }
    }

    /* Convert unix time to struct tm */
    gtm = mk_cache_get(mk_cache_utils_gmtime);

    /* If this function was invoked from a non-thread context it should exit */
    mk_bug(!gtm);
    gtm = gmtime_r(&date, gtm);
    if (!gtm) {
        return -1;
    }

    /* struct tm -> tm_year counts number of years after 1900 */
    year = gtm->tm_year + 1900;

    /* Signed division is slow, by using unsigned we gain 25% speed */
    mday = gtm->tm_mday;
    hour = gtm->tm_hour;
    min = gtm->tm_min;
    sec = gtm->tm_sec;

    /* Compose template */
    buf = *data;

    /* Week day */
    memcpy(buf, mk_date_wd[gtm->tm_wday], 5);
    buf += 5;

    /* Day of the month */
    *buf++ = ('0' + (mday / 10));
    *buf++ = ('0' + (mday % 10));
    *buf++ = ' ';

    /* Month */
    memcpy(buf, mk_date_ym[gtm->tm_mon], 4);
    buf += 4;

    /* Year */
    *buf++ = ('0' + (year / 1000) % 10);
    *buf++ = ('0' + (year / 100) % 10);
    *buf++ = ('0' + (year / 10) % 10);
    *buf++ = ('0' + (year % 10));
    *buf++ = ' ';

    /* Hour */
    *buf++ = ('0' + (hour / 10));
    *buf++ = ('0' + (hour % 10));
    *buf++ = ':';

    /* Minutes */
    *buf++ = ('0' + (min / 10));
    *buf++ = ('0' + (min % 10));
    *buf++ = ':';

    /* Seconds */
    *buf++ = ('0' + (sec / 10));
    *buf++ = ('0' + (sec % 10));

    /* GMT Time zone + CRLF */
    memcpy(buf, " GMT\r\n\0", 7);

    /* Add new entry to the cache */
    mk_utils_gmt_cache_add(*data, date);

    /* Set mk_pointer data len */
    return size;
}

time_t mk_utils_gmt2utime(char *date)
{
    time_t new_unix_time;
    struct tm t_data;
    memset(&t_data, 0, sizeof(struct tm));

    if (!strptime(date, GMT_DATEFORMAT, (struct tm *) &t_data)) {
        return -1;
    }

    new_unix_time = timegm((struct tm *) &t_data);

    return (new_unix_time);
}

int mk_buffer_cat(mk_pointer *p, char *buf1, int len1, char *buf2, int len2)
{
    /* Validate lengths */
    if (mk_unlikely(len1 < 0 || len2 < 0)) {
         return -1;
    }

    /* alloc space */
    p->data = (char *) mk_mem_malloc(len1 + len2 + 1);

    /* copy data */
    memcpy(p->data, buf1, len1);
    memcpy(p->data + len1, buf2, len2);
    p->data[len1 + len2] = '\0';

    /* assign len */
    p->len = len1 + len2;

    return 0;
}

/* Convert hexadecimal to int */
int mk_utils_hex2int(char *hex, int len)
{
    int i = 0;
    int res = 0;
    char c;

    while ((c = *hex++) && i < len) {
        res *= 0x10;

        if (c >= 'a' && c <= 'f') {
            res += (c - 0x57);
        }
        else if (c >= 'A' && c <= 'F') {
            res += (c - 0x37);
        }
        else if (c >= '0' && c <= '9') {
            res += (c - 0x30);
        }
        else {
            return -1;
        }
        i++;
    }

    if (res < 0) {
        return -1;
    }

    return res;
}

/* If the URI contains hexa format characters it will return
 * convert the Hexa values to ASCII character
 */
char *mk_utils_url_decode(mk_pointer uri)
{
    int tmp, hex_result;
    unsigned int i;
    int buf_idx = 0;
    char *buf;
    char hex[3];

    if ((tmp = mk_string_char_search(uri.data, '%', uri.len)) < 0) {
        return NULL;
    }

    i = tmp;

    buf = mk_mem_malloc_z(uri.len);

    if (i > 0) {
        strncpy(buf, uri.data, i);
        buf_idx = i;
    }

    while (i < uri.len) {
        if (uri.data[i] == '%' && i + 2 < uri.len) {
            memset(hex, '\0', sizeof(hex));
            strncpy(hex, uri.data + i + 1, 2);
            hex[2] = '\0';

            hex_result = mk_utils_hex2int(hex, 2);

            if (hex_result != -1) {
                buf[buf_idx] = hex_result;
            }
            else {
                mk_mem_free(buf);
                return NULL;
            }
            i += 2;
        }
        else {
            buf[buf_idx] = uri.data[i];
        }
        i++;
        buf_idx++;
    }
    buf[buf_idx] = '\0';

    return buf;
}

/*robust get environment variable that also checks __secure_getenv() */
char *mk_utils_getenv(const char *arg)
{
#ifdef HAVE___SECURE_GETENV
    return __secure_getenv(arg);
#else
    return getenv(arg);
#endif
}

#ifdef TRACE
#include <sys/time.h>
void mk_utils_trace(const char *component, int color, const char *function,
                    char *file, int line, const char* format, ...)
{
    va_list args;
    char *color_function  = NULL;
    char *color_fileline  = NULL;
    char *color_component = NULL;

    char *reset_color   = ANSI_RESET;
    char *magenta_color = ANSI_RESET ANSI_BOLD_MAGENTA;
    char *red_color     = ANSI_RESET ANSI_BOLD_RED;
    char *cyan_color    = ANSI_RESET ANSI_CYAN;

    struct timeval tv;
    struct timezone tz;

    if (env_trace_filter) {
        if (!strstr(env_trace_filter, file)) {
            return;
        }
    }

    /* Mutex lock */
    pthread_mutex_lock(&mutex_trace);

    gettimeofday(&tv, &tz);

    /* Switch message color */
    char* bgcolortype = mk_utils_getenv("MK_TRACE_BACKGROUND");

    if (!bgcolortype) {
        bgcolortype = "dark";
    }

    if (!strcmp(bgcolortype, "light")) {
        switch(color) {
        case MK_TRACE_CORE:
            color_component = ANSI_BOLD_GREEN;
            color_function  = ANSI_BOLD_MAGENTA;
            color_fileline  = ANSI_GREEN;
            break;
        case MK_TRACE_PLUGIN:
            color_component = ANSI_BOLD_GREEN;
            color_function  = ANSI_BLUE;
            color_fileline  = ANSI_GREEN;
            break;
        }
    }
    else { /* covering 'dark' and garbage values defaulting to 'dark' cases */
        switch(color) {
        case MK_TRACE_CORE:
            color_component = ANSI_BOLD_GREEN;
            color_function  = ANSI_YELLOW;
            color_fileline  = ANSI_BOLD_WHITE;
            break;
        case MK_TRACE_PLUGIN:
            color_component = ANSI_BOLD_BLUE;
            color_function  = ANSI_BLUE;
            color_fileline  = ANSI_BOLD_WHITE;
            break;
        }
    }

    /* Only print colors to a terminal */
    if (!isatty(STDOUT_FILENO)) {
        color_function = "";
        color_fileline = "";
        reset_color    = "";
        magenta_color  = "";
        red_color      = "";
        cyan_color     = "";
    }

    va_start( args, format );

    printf("~ %s%2lu.%lu%s %s[%s%s%s|%s:%i%s] %s%s()%s ",
           cyan_color, (tv.tv_sec - monkey_init_time), tv.tv_usec, reset_color,
           magenta_color, color_component, component, color_fileline, file,
           line, magenta_color,
           color_function, function, red_color);
    vprintf(format, args );
    va_end(args);
    printf("%s\n", reset_color);
    fflush(stdout);

    /* Mutex unlock */
    pthread_mutex_unlock(&mutex_trace);

}

int mk_utils_print_errno(int n)
{
        switch(n) {
        case EAGAIN:
            MK_TRACE("EAGAIN");
            return -1;
        case EBADF:
            MK_TRACE("EBADF");
            return -1;
        case EFAULT:
            MK_TRACE("EFAULT");
            return -1;
        case EFBIG:
            MK_TRACE("EFBIG");
            return -1;
        case EINTR:
            MK_TRACE("EINTR");
            return -1;
        case EINVAL:
            MK_TRACE("EINVAL");
            return -1;
        case EPIPE:
            MK_TRACE("EPIPE");
            return -1;
        default:
            MK_TRACE("DONT KNOW");
            return 0;
        }

        return 0;
}

#endif

#ifndef SHAREDLIB

/* Run current process in background mode (daemon, evil Monkey >:) */
int mk_utils_set_daemon()
{
    pid_t pid;

    if ((pid = fork()) < 0){
		mk_err("Error: Failed creating to switch to daemon mode(fork failed)");
        exit(EXIT_FAILURE);
	}

    if (pid > 0) /* parent */
        exit(EXIT_SUCCESS);

    /* set files mask */
    umask(0);

    /* Create new session */
    setsid();

    if (chdir("/") < 0) { /* make sure we can unmount the inherited filesystem */
        mk_err("Error: Unable to unmount the inherited filesystem in the daemon process");
        exit(EXIT_FAILURE);
	}

    /* Our last STDOUT messages */
    mk_details();
    mk_info("Background mode ON");

    fclose(stderr);
    fclose(stdout);

    return 0;
}

/* Write Monkey's PID */
int mk_utils_register_pid()
{
    int fd;
    char pidstr[MK_MAX_PID_LEN];
    unsigned long len = 0;
    char *filepath = NULL;
    struct flock lock;
    struct stat sb;

    if (config->pid_status == MK_TRUE)
        return -1;

    mk_string_build(&filepath, &len, "%s.%d", config->pid_file_path, config->serverport);
    if (!stat(filepath, &sb)) {
        /* file exists, perhaps previously kepts by SIGKILL */
        unlink(filepath);
    }

    if ((fd = open(filepath, O_WRONLY | O_CREAT | O_CLOEXEC, 0444)) < 0) {
        mk_err("Error: I can't log pid of monkey");
        exit(EXIT_FAILURE);
    }

    /* create a write exclusive lock for the entire file */
    lock.l_type = F_WRLCK;
    lock.l_start = 0;
    lock.l_whence = SEEK_SET;
    lock.l_len = 0;

    if (fcntl(fd, F_SETLK, &lock) < 0) {
        close(fd);
        mk_err("Error: I cannot set the lock for the pid of monkey");
        exit(EXIT_FAILURE);
    }

    sprintf(pidstr, "%i", getpid());
    ssize_t write_len = strlen(pidstr);
    if (write(fd, pidstr, write_len) != write_len) {
        close(fd);
        mk_err("Error: I cannot write the lock for the pid of monkey");
        exit(EXIT_FAILURE);
    }

    mk_mem_free(filepath);
    config->pid_status = MK_TRUE;

    return 0;
}

/* Remove PID file */
int mk_utils_remove_pid()
{
    unsigned long len = 0;
    char *filepath = NULL;

    mk_string_build(&filepath, &len, "%s.%d", config->pid_file_path, config->serverport);
    mk_user_undo_uidgid();
    if (unlink(filepath)) {
        mk_warn("cannot delete pidfile\n");
    }
    mk_mem_free(filepath);
    config->pid_status = MK_FALSE;
    return 0;
}

#endif // !SHAREDLIB

void mk_print(int type, const char *format, ...)
{
    time_t now;
    struct tm *current;

    const char *header_color = NULL;
    const char *header_title = NULL;
    const char *bold_color = ANSI_BOLD;
    const char *reset_color = ANSI_RESET;
    const char *white_color = ANSI_WHITE;
    va_list args;

    va_start(args, format);

    switch (type) {
    case MK_INFO:
        header_title = "Info";
        header_color = ANSI_GREEN;
        break;
    case MK_ERR:
        header_title = "Error";
        header_color = ANSI_RED;
        break;
    case MK_WARN:
        header_title = "Warning";
        header_color = ANSI_YELLOW;
        break;
    case MK_BUG:
#ifdef DEBUG
        mk_utils_stacktrace();
#endif
        header_title = " BUG !";
        header_color = ANSI_BOLD ANSI_RED;
        break;
    }

    /* Only print colors to a terminal */
    if (!isatty(STDOUT_FILENO)) {
        header_color = "";
        bold_color = "";
        reset_color = "";
        white_color = "";
    }

    now = time(NULL);
    struct tm result;
    current = localtime_r(&now, &result);
    printf("%s[%s%i/%02i/%02i %02i:%02i:%02i%s]%s ",
           bold_color, reset_color,
           current->tm_year + 1900,
           current->tm_mon + 1,
           current->tm_mday,
           current->tm_hour,
           current->tm_min,
           current->tm_sec,
           bold_color, reset_color);

    printf("%s[%s%7s%s]%s ",
           bold_color, header_color, header_title, white_color, reset_color);

    vprintf(format, args);
    va_end(args);
    printf("%s\n", reset_color);
    fflush(stdout);
}

pthread_t mk_utils_worker_spawn(void (*func) (void *), void *arg)
{
    pthread_t tid;
    pthread_attr_t thread_attr;

    pthread_attr_init(&thread_attr);
    pthread_attr_setdetachstate(&thread_attr, PTHREAD_CREATE_DETACHED);
    if (pthread_create(&tid, &thread_attr, (void *) func, arg) < 0) {
        mk_libc_error("pthread_create");
        exit(EXIT_FAILURE);
    }

    return tid;
}

int mk_utils_worker_rename(const char *title)
{
    return prctl(PR_SET_NAME, title, 0, 0, 0);
}

#ifdef NO_BACKTRACE
void mk_utils_stacktrace(void) {}
#else
void mk_utils_stacktrace(void)
{
    unsigned int i;
    int ret;
    size_t size;
    void *arr[10];
    Dl_info d;

    printf("[stack trace]\n");
    size = backtrace(arr, 10);

    for (i = 1; i < size && i < 10; i++) {
      ret = dladdr(arr[i], &d);
      if (ret == 0 || !d.dli_sname) {
          printf(" #%i  0x%016" PRIxPTR " in \?\?\?\?\?\?\?()\n",
                 (i - 1), (uintptr_t) arr[i]);
          continue;
      }

      printf(" #%i  0x%016" PRIxPTR " in %s() from %s\n",
             (i - 1), (uintptr_t) arr[i], d.dli_sname, d.dli_fname);
    }
}
#endif



/*
 * This hash generation function is taken originally from Redis source code:
 *
 *  https://github.com/antirez/redis/blob/unstable/src/dict.c#L109
 *
 * ----
 * MurmurHash2, by Austin Appleby
 * Note - This code makes a few assumptions about how your machine behaves -
 * 1. We can read a 4-byte value from any address without crashing
 * 2. sizeof(int) == 4
 *
 * And it has a few limitations -
 *
 * 1. It will not work incrementally.
 * 2. It will not produce the same results on little-endian and big-endian
 *    machines.
 */
unsigned int mk_utils_gen_hash(const void *key, int len)
{
    /* 'm' and 'r' are mixing constants generated offline.
       They're not really 'magic', they just happen to work well.  */
    uint32_t seed = 5381;
    const uint32_t m = 0x5bd1e995;
    const int r = 24;

    /* Initialize the hash to a 'random' value */
    uint32_t h = seed ^ len;

    /* Mix 4 bytes at a time into the hash */
    const unsigned char *data = (const unsigned char *)key;

    while(len >= 4) {
        uint32_t k = *(uint32_t*) data;

        k *= m;
        k ^= k >> r;
        k *= m;

        h *= m;
        h ^= k;

        data += 4;
        len -= 4;
    }

    /* Handle the last few bytes of the input array  */
    switch(len) {
    case 3: h ^= data[2] << 16;
    case 2: h ^= data[1] << 8;
    case 1: h ^= data[0]; h *= m;
    };

    /* Do a few final mixes of the hash to ensure the last few
     * bytes are well-incorporated. */
    h ^= h >> 13;
    h *= m;
    h ^= h >> 15;

    return (unsigned int) h;
}
