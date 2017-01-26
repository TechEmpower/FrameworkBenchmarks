
/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Monkey HTTP Daemon
 *  ------------------
 *  Copyright (C) 2001-2009, Eduardo Silva P.
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

/* security.c */
#ifndef MK_SECURITY_H
#define MK_SECURITY_H

struct mk_secure_ip_t
{
    struct in_addr ip;

    /* if subnet is true, next fields are populated */
    int is_subnet;

    int network;
    int netmask;
    unsigned int hostmin;
    unsigned int hostmax;

    /* list head linker */
    struct mk_list _head;
};

struct mk_secure_url_t
{
    char *criteria;
    struct mk_list _head;
};

struct mk_secure_deny_hotlink_t
{
    char *criteria;
    struct mk_list _head;
};

struct mk_list mk_secure_ip;
struct mk_list mk_secure_url;
struct mk_list mk_secure_deny_hotlink;

#endif
