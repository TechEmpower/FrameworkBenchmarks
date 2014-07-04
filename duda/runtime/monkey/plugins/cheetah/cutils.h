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

#ifndef MK_CHEETAH_CUTILS_H
#define MK_CHEETAH_CUTILS_H

#include <stdio.h>

#define CHEETAH_WRITE(...) mk_cheetah_write(__VA_ARGS__);
#define CHEETAH_FLUSH() fflush(cheetah_output);fflush(cheetah_input);

void mk_cheetah_print_worker_memory_usage(pid_t pid);
void mk_cheetah_print_running_user();
int mk_cheetah_write(const char *format, ...);

#endif
