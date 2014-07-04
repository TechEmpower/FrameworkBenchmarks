/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Duda I/O
 *  --------
 *  Copyright (C) 2012-2014, Eduardo Silva P. <edsiper@gmail.com>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef DUDA_PACKAGE_SSLS_H
#define DUDA_PACKAGE_SSLS_H

#include "duda_api.h"
#include "webservice.h"

#include <polarssl/version.h>
#include <polarssl/error.h>
#include <polarssl/net.h>
#include <polarssl/ssl.h>
#include <polarssl/bignum.h>
#include <polarssl/entropy.h>
#include <polarssl/ctr_drbg.h>
#include <polarssl/certs.h>
#include <polarssl/x509.h>

#ifdef POLARSSL_SSL_CACHE_C
#include <polarssl/ssl_cache.h>
#endif

#ifndef POLAR_DEBUG_LEVEL
#define POLAR_DEBUG_LEVEL 0
#endif


/* Represents a SSL connection into the server */
typedef struct {
    int fd;
    ssl_context ssl_ctx;
#if (POLARSSL_VERSION_NUMBER < 0x01020000)
    ssl_session session;
#endif

    struct mk_list _head;
} ssls_conn_t;

/* A SSL-Server context */
struct ssls_ctx {
    int fd;                   /* socket server listening from incoming connections */
    int efd;                  /* epoll file descriptor */
    struct mk_list conns;     /* active connections */

    /* SSL specific data */
    x509_cert cacert;
    x509_cert srvcert;
    dhm_context dhm;
    rsa_context rsa;

#ifdef POLARSSL_SSL_CACHE_C
    ssl_cache_context cache;
#endif
    entropy_context entropy;
    ctr_drbg_context ctr_drbg;

    /* callback hooks */
    void (*cb_accepted)(struct ssls_ctx *, ssls_conn_t *, int);
    void (*cb_read)    (struct ssls_ctx *, ssls_conn_t *, int, unsigned char *, int);
    void (*cb_write)   (struct ssls_ctx *, ssls_conn_t *, int);
    void (*cb_close)   (struct ssls_ctx *, ssls_conn_t *, int);
    void (*cb_timeout) (struct ssls_ctx *, ssls_conn_t *, int);
};

typedef struct ssls_ctx ssls_ctx_t;

/* API structure */
struct duda_api_ssls {
    int (*write) (ssls_conn_t *, unsigned char *, int);
    int (*event_mod) (int, int, int);
    int (*event_add) (int, int);
    int (*event_del) (int, int);
    int (*load_dh_param) (ssls_ctx_t *, char *);
    int (*load_ca_root_cert) (ssls_ctx_t *, char *);
    int (*load_cert) (ssls_ctx_t *, char *);
    int (*load_key)  (ssls_ctx_t *, char *);
    int (*socket_server)  (int, char *);
    void (*set_callbacks) (ssls_ctx_t *,
                           void (*cb_accepted)(struct ssls_ctx *, ssls_conn_t *, int),
                           void (*cb_read)    (struct ssls_ctx *, ssls_conn_t *,
                                               int, unsigned char *, int),
                           void (*cb_write)   (struct ssls_ctx *, ssls_conn_t *, int),
                           void (*cb_close)   (struct ssls_ctx *, ssls_conn_t *,
                                               int),
                           void (*cb_timeout) (struct ssls_ctx *, ssls_conn_t *,
                                               int));
    void (*server_loop) (ssls_ctx_t *);
    ssls_ctx_t *(*init) (int, char *);
};

/* functions */
int ssls_write(ssls_conn_t *conn, unsigned char *buf, int size);
int ssls_event_mod(int efd, int fd, int mode);
int ssls_event_add(int efd, int fd);
int ssls_event_del(int efd, int fd);
int ssls_load_dh_param(ssls_ctx_t *ctx, char *dh_file);
int ssls_load_ca_root_cert(ssls_ctx_t *ctx, char *cert_file);
int ssls_load_cert(ssls_ctx_t *ctx, char *cert_file);
int ssls_load_key(ssls_ctx_t *ctx, char *key_file);
int ssls_socket_server(int port, char *listen_addr);
void ssls_set_callbacks(ssls_ctx_t *ctx,
                        void (*cb_accepted)(struct ssls_ctx *, ssls_conn_t *, int),
                        void (*cb_read)    (struct ssls_ctx *, ssls_conn_t *,
                                            int, unsigned char *, int),
                        void (*cb_write)   (struct ssls_ctx *, ssls_conn_t *, int),
                        void (*cb_close)   (struct ssls_ctx *, ssls_conn_t *,
                                            int),
                        void (*cb_timeout) (struct ssls_ctx *, ssls_conn_t *,
                                            int));
void ssls_server_loop(ssls_ctx_t *ctx);
ssls_ctx_t *ssls_init(int port, char *listen_addr);

typedef struct duda_api_ssls ssls_object_t;
ssls_object_t *ssls;

#endif
