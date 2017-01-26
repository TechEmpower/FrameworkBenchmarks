/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Duda I/O
 *  --------
 *  Copyright (C) 2012-2014, Eduardo Silva P. <edsiper@gmail.com>
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

#include "duda_api.h"


#ifndef MK_DUDA_CONF_H
#define MK_DUDA_CONF_H

#define WS_ROOT_URI_LEN   64

char *services_root;  /* Location of web services   */
char *packages_root;  /* Duda packages path         */
mk_pointer document_root;  /* Duda Document Root (/ddr)  */

/*
 * List that contains memory references to each service
 * associated to a virtual host.
 */
struct mk_list services_list;

/*
 * List that contains memory references to all web services
 * loaded.
 */
struct mk_list services_loaded;

struct vhost_services {
    struct host *host;                 /* virtual host reference with Monkey */
    struct web_service *root_service;  /* optional root service              */
    struct mk_list services;           /* list of web services under this VH */
    struct mk_list _head;              /* head for services_loaded HEAD      */
};

int duda_conf_set_confdir(struct web_service *ws, const char *dir);
int duda_conf_set_datadir(struct web_service *ws, const char *dir);

int duda_conf_main_init(const char *confdir);
int duda_conf_vhost_init();
void duda_conf_messages_to(struct web_service *ws);

/* Object API */

struct duda_api_conf {
    #define force_redirect()  _force_redirect(self)
    void (*_force_redirect) (struct web_service *);

    #define bind_messages() _bind_messages(self)
    void (*_bind_messages) (struct web_service *);

    #define service_name(n) _service_name(self, n)
    void (*_service_name) (struct web_service *, const char *);

    #define service_root(n) _service_root(self)
    void (*_service_root) (struct web_service *);
};

struct duda_api_conf *duda_conf_object();

#endif
