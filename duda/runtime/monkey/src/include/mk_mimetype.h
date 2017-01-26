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

#include "mk_memory.h"
#include "mk_list.h"
#include "mk_rbtree.h"

#ifndef MK_MIMETYPE_H
#define MK_MIMETYPE_H

#define MIMETYPE_DEFAULT_TYPE "text/plain\r\n"
#define MIMETYPE_DEFAULT_NAME "default"

struct mimetype
{
    const char *name;
    mk_pointer type;

    struct mk_list _head;
    struct rb_node _rb_head;
};

/* Head for RBT */
struct mk_list mimetype_list;
struct rb_root mimetype_rb_head;

extern struct mimetype *mimetype_default;

int mk_mimetype_add(char *name, const char *type);
void mk_mimetype_read_config(void);
struct mimetype *mk_mimetype_find(mk_pointer * filename);
struct mimetype *mk_mimetype_lookup(char *name);
void mk_mimearr_sort();

#endif
