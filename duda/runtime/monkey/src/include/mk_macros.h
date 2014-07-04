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

#ifndef MK_MACROS_H
#define MK_MACROS_H

#include <stdlib.h>
#include "mk_limits.h"

/* Boolean */
#define MK_FALSE 0
#define MK_TRUE  !MK_FALSE
#define MK_ERROR -1

/* Architecture */
#define INTSIZE sizeof(int)

/* Print macros */
#define MK_INFO     0x1000
#define MK_ERR      0X1001
#define MK_WARN     0x1002
#define MK_BUG      0x1003

#define mk_info(...)  mk_print(MK_INFO, __VA_ARGS__)
#define mk_err(...)   mk_print(MK_ERR, __VA_ARGS__)
#define mk_warn(...)  mk_print(MK_WARN, __VA_ARGS__)

/* ANSI Colors */
#define ANSI_RESET "\033[0m"
#define ANSI_BOLD "\033[1m"

#define ANSI_CYAN          "\033[36m"
#define ANSI_BOLD_CYAN     ANSI_BOLD ANSI_CYAN
#define ANSI_MAGENTA       "\033[35m"
#define ANSI_BOLD_MAGENTA  ANSI_BOLD ANSI_MAGENTA
#define ANSI_RED           "\033[31m"
#define ANSI_BOLD_RED      ANSI_BOLD ANSI_RED
#define ANSI_YELLOW        "\033[33m"
#define ANSI_BOLD_YELLOW   ANSI_BOLD ANSI_YELLOW
#define ANSI_BLUE          "\033[34m"
#define ANSI_BOLD_BLUE     ANSI_BOLD ANSI_BLUE
#define ANSI_GREEN         "\033[32m"
#define ANSI_BOLD_GREEN    ANSI_BOLD ANSI_GREEN
#define ANSI_WHITE         "\033[37m"
#define ANSI_BOLD_WHITE    ANSI_BOLD ANSI_WHITE

/* Transport type */
#define MK_TRANSPORT_HTTP  "http"
#define MK_TRANSPORT_HTTPS "https"

#ifndef ARRAY_SIZE
# define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))
#endif

#ifdef __GNUC__ /* GCC supports this since 2.3. */
 #define PRINTF_WARNINGS(a,b) __attribute__ ((format (printf, a, b)))
#else
 #define PRINTF_WARNINGS(a,b)
#endif

#ifdef __GNUC__ /* GCC supports this since 2.7. */
 #define UNUSED_PARAM __attribute__ ((unused))
#else
 #define UNUSED_PARAM
#endif

/*
 * Validation macros
 * -----------------
 * Based on article http://lwn.net/Articles/13183/
 *
 * ---
 * ChangeSet 1.803, 2002/10/18 16:28:57-07:00, torvalds@home.transmeta.com
 *
 *	Make a polite version of BUG_ON() - WARN_ON() which doesn't
 *	kill the machine.
 *
 *	Damn I hate people who kill the machine for no good reason.
 * ---
 *
 */

#define mk_unlikely(x) __builtin_expect((x),0)
#define mk_likely(x) __builtin_expect((x),1)
#define mk_prefetch(x, ...) __builtin_prefetch(x, __VA_ARGS__)

#define mk_is_bool(x) ((x == MK_TRUE || x == MK_FALSE) ? 1 : 0)

#define mk_bug(condition) do {                                          \
        if (mk_unlikely((condition)!=0)) {                              \
            mk_print(MK_BUG, "Bug found in %s() at %s:%d",              \
                     __FUNCTION__, __FILE__, __LINE__);                 \
            abort();                                                    \
        }                                                               \
    } while(0)

/*
 * Macros to calculate sub-net data using ip address and sub-net prefix
 */

#define MK_NET_IP_OCTECT(addr,pos) (addr >> (8 * pos) & 255)
#define MK_NET_NETMASK(addr,net) htonl((0xffffffff << (32 - net)))
#define MK_NET_BROADCAST(addr,net) (addr | ~MK_NET_NETMASK(addr,net))
#define MK_NET_NETWORK(addr,net) (addr & MK_NET_NETMASK(addr,net))
#define MK_NET_WILDCARD(addr,net) (MK_NET_BROADCAST(addr,net) ^ MK_NET_NETWORK(addr,net))
#define MK_NET_HOSTMIN(addr,net) net == 31 ? MK_NET_NETWORK(addr,net) : (MK_NET_NETWORK(addr,net) + 0x01000000)
#define MK_NET_HOSTMAX(addr,net) net == 31 ? MK_NET_BROADCAST(addr,net) : (MK_NET_BROADCAST(addr,net) - 0x01000000)

#if __GNUC__ >= 4
 #define MK_EXPORT __attribute__ ((visibility ("default")))
#else
 #define MK_EXPORT
#endif

/* Wrapper (mk_utils) libc error helpers */
#define mk_libc_error(c)    mk_utils_libc_error(c, __FILE__, __LINE__)
#define mk_libc_warning(c)  mk_utils_libc_warning(c, __FILE__, __LINE__)

#endif
