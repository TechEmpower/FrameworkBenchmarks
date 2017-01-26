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

#include "mk_list.h"
#include "mk_config.h"
#include "mk_request.h"

#ifndef MK_VHOST_H
#define MK_VHOST_H

/* Custom error page */
struct error_page {
    short int status;
    char *file;
    char *real_path;
    struct mk_list _head;
};

struct host
{
    char *file;                   /* configuration file */
    struct mk_list server_names;  /* host names (a b c...) */

    mk_pointer documentroot;

    char *host_signature;
    mk_pointer header_host_signature;

    /* source configuration */
    struct mk_config *config;

    /* custom error pages */
    struct mk_list error_pages;

    /* link node */
    struct mk_list _head;
};

struct host_alias
{
    char *name;
    unsigned int len;

    struct mk_list _head;
};


#define VHOST_FDT_HASHTABLE_SIZE   64
#define VHOST_FDT_HASHTABLE_CHAINS  8

struct vhost_fdt_hash_chain {
    int fd;
    int readers;
    unsigned int hash;
};

struct vhost_fdt_hash_table {
    int av_slots;
    struct vhost_fdt_hash_chain chain[VHOST_FDT_HASHTABLE_CHAINS];
};

struct vhost_fdt_host {
    struct host *host;
    struct vhost_fdt_hash_table hash_table[VHOST_FDT_HASHTABLE_SIZE];
    struct mk_list _head;
};

//pthread_key_t mk_vhost_fdt_key;
pthread_mutex_t mk_vhost_fdt_mutex;

struct host *mk_vhost_read(char *path);
int mk_vhost_get(mk_pointer host, struct host **vhost, struct host_alias **alias);
void mk_vhost_init(char *path);
int mk_vhost_fdt_worker_init();
int mk_vhost_open(struct session_request *sr);
int mk_vhost_close(struct session_request *sr);

#ifdef SAFE_FREE
void mk_vhost_free_all();
#endif

#endif
