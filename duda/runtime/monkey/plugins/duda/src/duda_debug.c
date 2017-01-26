/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Duda I/O
 *  --------
 *  Copyright (C) 2012-2014, Eduardo Silva P. <edsiper@gmail.com>
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

#include "MKPlugin.h"
#include "webservice.h"

#include <stdio.h>
#include <stdarg.h>
#include <time.h>

void _duda_debug_header(int type)
{
    time_t now;
    struct tm *current;

    char *header_color = NULL;
    char *header_title = NULL;

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
        header_title = " BUG !";
        header_color = ANSI_BOLD ANSI_RED;
    }

    now = time(NULL);
    current = localtime(&now);
    printf("%s[%s%i/%02i/%02i %02i:%02i:%02i%s]%s ",
           ANSI_BOLD, ANSI_RESET,
           current->tm_year + 1900,
           current->tm_mon + 1,
           current->tm_mday,
           current->tm_hour,
           current->tm_min,
           current->tm_sec,
           ANSI_BOLD, ANSI_RESET);

    printf("%s[%s%7s%s]%s ",
           ANSI_BOLD, header_color, header_title, ANSI_WHITE, ANSI_RESET);
}

void _duda_debug_footer()
{
    printf("%s\n", ANSI_RESET);
    fflush(stdout);
}

void duda_debug_info(const char *format, ...)
{
    va_list args;

    _duda_debug_header(MK_INFO);

    va_start(args, format);
    vprintf(format, args);
    va_end(args);

    _duda_debug_footer();
}

void duda_debug_warn(const char *format, ...)
{
    va_list args;

    _duda_debug_header(MK_WARN);

    va_start(args, format);
    vprintf(format, args);
    va_end(args);

    _duda_debug_footer();
}

void duda_debug_err(const char *format, ...)
{
    va_list args;

    _duda_debug_header(MK_ERR);

    va_start(args, format);
    vprintf(format, args);
    va_end(args);

    _duda_debug_footer();
}

void duda_debug_bug(const char *format, ...)
{
    va_list args;

    _duda_debug_header(MK_BUG);

    va_start(args, format);
    vprintf(format, args);
    va_end(args);

    _duda_debug_footer();
}

