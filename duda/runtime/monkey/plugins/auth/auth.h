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

#ifndef MK_AUTH_H
#define MK_AUTH_H

#include "MKPlugin.h"

/* Header stuff */
#define MK_AUTH_HEADER_REQUEST   "Authorization:"
#define MK_AUTH_HEADER_BASIC     "Basic "
#define MK_AUTH_HEADER_TITLE     "WWW-Authenticate: Basic realm=\"%s\""

/* Credentials length */
#define MK_AUTH_CREDENTIALS_LEN 256

/*
 * The plugin hold one struct per virtual host and link to the
 * locations and users file associated:
 *
 *                    +---------------------------------+
 *      struct vhost  >            vhost (1:N)          |
 *                    |     +---------+----------+      |
 *                    |     |         |          |      |
 *   struct location  > location  location    location  |
 *                    |     |         |          |      |
 *                    |     +----+----+          +      |
 *                    |          |               |      |
 *      struct users  >        users           users    |
 *                    +---------------------------------+
 *
 */

/* List of virtual hosts to handle locations */
struct mk_list vhosts_list;

/* main index for locations under a virtualhost */
struct vhost {
    struct host *host;
    struct mk_list locations;
    struct mk_list _head;
};

/* 
 * A location restrict a filesystem path with a list
 * of allowed users
 */
struct location {
    mk_pointer path;
    mk_pointer title;
    mk_pointer auth_http_header;

    struct users_file *users;
    struct mk_list _head;
};

/* Head index for user files list */
struct mk_list users_file_list;

/* 
 * Represents a users file, each entry represents a physical
 * file and belongs to a node of the users_file_list list
 */
struct users_file {
    time_t last_updated;   /* last time this entry was modified */
    char *path;            /* file path */
    struct mk_list _users; /* list of users */
    struct mk_list _head;  /* head for main mk_list users_file_list */
};

/* 
 * a list of users, this list belongs to a  
 * struct location 
 */
struct user {
    char user[128];
    char passwd_raw[256];
    unsigned char *passwd_decoded;

    struct mk_list _head;
};

struct mk_list users_file_list;

/* Thread key */
mk_pointer auth_header_request;
mk_pointer auth_header_basic;

#define SHA1_DIGEST_LEN 20

#endif
