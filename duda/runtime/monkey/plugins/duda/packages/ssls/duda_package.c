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

#include "duda_api.h"
#include "duda_package.h"

#include "ssls.h"

#include <polarssl/version.h>
#include <polarssl/error.h>
#include <polarssl/net.h>
#include <polarssl/ssl.h>
#include <polarssl/bignum.h>
#include <polarssl/entropy.h>
#include <polarssl/ctr_drbg.h>

#include <polarssl/certs.h>
#include <polarssl/x509.h>


struct duda_api_ssls *get_ssls_api()
{
    struct duda_api_ssls *s;

    /* Alloc object */
    s = malloc(sizeof(struct duda_api_ssls));
    s->write     = ssls_write;
    s->event_mod = ssls_event_mod;
    s->event_add = ssls_event_add;
    s->event_del = ssls_event_del;
    s->load_dh_param = ssls_load_dh_param;
    s->load_ca_root_cert = ssls_load_ca_root_cert;
    s->load_cert = ssls_load_cert;
    s->load_key = ssls_load_key;
    s->socket_server = ssls_socket_server;
    s->set_callbacks = ssls_set_callbacks;
    s->server_loop = ssls_server_loop;
    s->init = ssls_init;

    return s;
}

duda_package_t *duda_package_main()
{
    duda_package_t *dpkg;

    /* Package object */
    dpkg = monkey->mem_alloc(sizeof(duda_package_t));
    dpkg->name    = "ssls";
    dpkg->version = "0.1";
    dpkg->api     = get_ssls_api();

    return dpkg;
}
