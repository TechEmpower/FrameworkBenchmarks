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

#ifdef LINUX_TRACE

#undef  TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER monkey

#if !defined(_MK_LINUXTRACE_PROVIDER_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _MK_LINUXTRACE_PROVIDER_H
#include <lttng/tracepoint.h>

/* Trace point for epoll(2) events */
TRACEPOINT_EVENT(
                 monkey,
                 epoll,
                 TP_ARGS(int, fd,
                         char *, text),
                 TP_FIELDS(
                           ctf_integer(int, fd, fd)
                           ctf_string(event, text)
                           )
                 )

TRACEPOINT_EVENT(
                 monkey,
                 epoll_state,
                 TP_ARGS(int, fd,
                         int, mode,
                         char *, text),
                 TP_FIELDS(
                           ctf_integer(int, fd, fd)
                           ctf_string(event, text)
                           )
                 )

TRACEPOINT_EVENT(
                 monkey,
                 scheduler,
                 TP_ARGS(int, fd,
                         char *, text),
                 TP_FIELDS(ctf_integer(int, fd, fd)
                           ctf_string(event, text))
                 )

#endif

#undef  TRACEPOINT_INCLUDE
#define TRACEPOINT_INCLUDE "./mk_linuxtrace.h"

#include <lttng/tracepoint-event.h>

/* Monkey Linux Trace helper macros */
#define MK_LT_EPOLL(fd, event) tracepoint(monkey, epoll, fd, event)
#define MK_LT_EPOLL_STATE(fd, mode, event) \
  tracepoint(monkey, epoll_state, fd, mode, event)
#define MK_LT_SCHED(fd, event) tracepoint(monkey, scheduler, fd, event)

#else /* NO LINUX_TRACE */

#define MK_LT_EPOLL(fd, event) do {} while(0)
#define MK_LT_EPOLL_STATE(fd, mode, event) do{} while(0)
#define MK_LT_SCHED(fd, event) do {} while(0)
#endif
