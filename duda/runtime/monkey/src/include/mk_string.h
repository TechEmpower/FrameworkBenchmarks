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

#ifndef MK_STR_H
#define MK_STR_H

#include "memory.h"
#include "mk_list.h"
#include "mk_macros.h"

/* Case sensitive OFF */
#define MK_STR_SENSITIVE 0

/* Case sensitive ON */
#define MK_STR_INSENSITIVE 1

struct mk_string_line
{
    char *val;
    int len;

    struct mk_list _head;
};

/* Lookup char into string, return position */
int mk_string_char_search(const char *string, int c, int len);

/* Find char into string searching in reverse order, returns position */
int mk_string_char_search_r(const char *string, int c, int len);

/* Locate a substring, returns the position of the substring */
int mk_string_search(const char *haystack, const char *needle, int sensitive);

/* Locate a substring, compare the first n bytes of haystack */
int mk_string_search_n(const char *haystack, const char *needle, int sensitive, int len);

char *mk_string_remove_space(char *buf);
char *mk_string_casestr(char *heystack, char *needle);
char *mk_string_dup(const char *s);
struct mk_list *mk_string_split_line(const char *line);
void mk_string_split_free(struct mk_list *list);
int mk_string_trim(char **str);
char *mk_string_build(char **buffer, unsigned long *len,
                      const char *format, ...) PRINTF_WARNINGS(3,4);
int mk_string_itop(int n, mk_pointer *p);
char *mk_string_copy_substr(const char *string, int pos_init, int pos_end);

char *mk_string_tolower(const char *in);

#endif
