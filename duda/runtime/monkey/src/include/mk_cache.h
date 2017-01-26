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

#ifndef MK_CACHE_H
#define MK_CACHE_H

extern pthread_key_t mk_cache_iov_header;
extern pthread_key_t mk_cache_header_lm;
extern pthread_key_t mk_cache_header_cl;
extern pthread_key_t mk_cache_header_ka;
extern pthread_key_t mk_cache_header_ka_max;
extern pthread_key_t mk_cache_utils_gmtime;
extern pthread_key_t mk_cache_utils_gmt_text;

void mk_cache_thread_init(void);

static inline void *mk_cache_get(pthread_key_t key)
{
    return pthread_getspecific(key);
}

#endif
