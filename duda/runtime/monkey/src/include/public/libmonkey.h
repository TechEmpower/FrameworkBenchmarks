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

#ifndef MK_MONKEYLIB_H
#define MK_MONKEYLIB_H

#ifdef __cplusplus
extern "C" {
#endif

#include <pthread.h>

#if __GNUC__ >= 4
 #define NULL_TERMINATED __attribute__ ((sentinel))
#else
 #define NULL_TERMINATED
#endif

/* For the internal build */
#ifdef SHAREDLIB
 #include <mk_macros.h>
#else
 #ifndef MK_EXPORT
 #define MK_EXPORT
 #endif
#endif

#define MONKEY__            1
#define MONKEY_MINOR__      4
#define MONKEY_PATCHLEVEL__ 0

/* ---------------------------------
 *               Data
 * --------------------------------- */

/* Opaque pointer, not for use for the apps */
typedef struct mklib_ctx_t* mklib_ctx;

enum {
    MKLIB_FALSE = 0,
    MKLIB_TRUE = 1
};

struct mklib_vhost {
    const char *name;
    const char *document_root;
    const char *server_names;
};

struct mklib_worker_info {
    unsigned long long active_connections;
    int pid;
};

struct mklib_mime {
    const char *name;
    const char *type;
};

/* Supported plugins, OR'ed in the init call */
enum {
    MKLIB_LIANA = 0x1,
    MKLIB_LIANA_SSL = 0x2
};

/* Config options for the main config call */
enum mklib_mkc {
    MKC_WORKERS = 1,
    MKC_TIMEOUT,
    MKC_USERDIR,
    MKC_INDEXFILE,
    MKC_HIDEVERSION,
    MKC_RESUME,
    MKC_KEEPALIVE,
    MKC_KEEPALIVETIMEOUT,
    MKC_MAXKEEPALIVEREQUEST,
    MKC_MAXREQUESTSIZE,
    MKC_SYMLINK,
    MKC_DEFAULTMIMETYPE
};

/* Config options for the vhost config call */
enum mklib_mkv {
    MKV_SERVERNAME = 1,
    MKV_DOCUMENTROOT
};

/* Callbacks */
enum mklib_cb {
    MKCB_IPCHECK = 1,
    MKCB_URLCHECK,
    MKCB_DATA,
    MKCB_CLOSE
};

/* struct session_request need not be exposed */
typedef void mklib_session;

/* Called when a new connection arrives. Return MKLIB_FALSE to reject this connection. */
typedef int (*cb_ipcheck)(const char *ip);

/* Called when the URL is known. Return MKLIB_FALSE to reject this connection. */
typedef int (*cb_urlcheck)(const char *url);

/* The data callback. Return MKLIB_FALSE if you don't want to handle this URL
 * (it will be checked against real files at this vhost's DocumentRoot).
 *
 * Set *content to point to the content memory. It must
 * stay available until the close callback is called.
 *
 * *header has static storage of 256 bytes for any custom headers. */
typedef int (*cb_data)(const mklib_session *, const char *vhost, const char *url,
                       const char *get, unsigned long get_len, const char *post, unsigned long post_len,
                       unsigned int *status, const char **content, unsigned long *clen,
                       char *header);

/* This will be called after the content has been served. If you allocated
 * any memory, you can match that memory to the mklib_session pointer and free
 * it in this callback. */
typedef void (*cb_close)(const mklib_session *);


/* ---------------------------------
 *                API
 * --------------------------------- */

/* Returns NULL on error. All pointer arguments may be NULL and the port/plugins
 * may be 0 for the defaults in each case.
 *
 * With no address, bind to all.
 * With no port, use 2001.
 * With no plugins, default to MKLIB_LIANA only.
 * With no documentroot, the default vhost won't access files.
 */
mklib_ctx MK_EXPORT mklib_init(const char *address, const unsigned int port,
                               const unsigned int plugins, const char *documentroot);

/* Set the callbacks. */
int MK_EXPORT mklib_callback_set(mklib_ctx, const enum mklib_cb, void *);

/* NULL-terminated config call, consisting of pairs of config item and argument.
 * Returns MKLIB_FALSE on failure. */
int MK_EXPORT mklib_config(mklib_ctx, ...) NULL_TERMINATED;

/* NULL-terminated config call creating a vhost with *name. Returns MKLIB_FALSE
 * on failure. */
int MK_EXPORT mklib_vhost_config(mklib_ctx, const char *name, ...) NULL_TERMINATED;

/* Start the server. */
int MK_EXPORT mklib_start(mklib_ctx);

/* Stop the server and free mklib_ctx. */
int MK_EXPORT mklib_stop(mklib_ctx);

/* Return a list of existing vhosts */
struct mklib_vhost MK_EXPORT **mklib_vhost_list(mklib_ctx);

/* Return a list of the workers */
struct mklib_worker_info MK_EXPORT **mklib_scheduler_worker_info(mklib_ctx);

/* Return a list of all mimetypes */
struct mklib_mime MK_EXPORT **mklib_mimetype_list(mklib_ctx);

/* Add a new mimetype */
int MK_EXPORT mklib_mimetype_add(mklib_ctx, char *, const char *);

/* Get the value of a http header.
 *
 * The return value is 0 on successful execution else -1.
 *
 * If a key is found and it has a value set, the data will be duplicated
 * and the value argument will be pointed at it.
 * If the key isn't fond or the value is a zero length string the value
 * argument will point at a NULL pointer.
 *
 * Please free *value when it isn't needed anymore.
 */
int mklib_get_request_header(const mklib_session *ms, const char *key, char **value);

#define mklib_vhost_foreach(cur, list) for(cur = *list++; cur; cur = *list++)
#define mklib_worker_info_foreach(cur, list) mklib_vhost_foreach(cur, list)
#define mklib_mimetype_foreach(cur, list) mklib_vhost_foreach(cur, list)

#ifdef __cplusplus
}
#endif

#endif
