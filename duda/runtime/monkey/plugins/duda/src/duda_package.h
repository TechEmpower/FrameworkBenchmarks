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

#ifndef DUDA_PACKAGE_H
#define DUDA_PACKAGE_H

#include <stdlib.h>
#include <sys/syscall.h>

#include "MKPlugin.h"
#include "duda_api.h"
#include "duda_map.h"
#include "duda_global.h"
#include "duda_package.h"
#include "duda_param.h"
#include "duda_session.h"
#include "duda_cookie.h"
#include "duda_data.h"
#include "duda_conf.h"
#include "duda_xtime.h"
#include "duda_console.h"
#include "duda_log.h"
#include "duda_gc.h"
#include "duda_objects.h"
#include "duda_fconf.h"
#include "duda_qs.h"

struct duda_package {
    char *name;
    char *version;
    void *api;
    void *handler;

    struct mk_list _head;
};

/* Data type */
typedef struct duda_package duda_package_t;

/* Hook defines for packages */
duda_package_t MK_EXPORT *_duda_package_main();

/* Reference and set Duda API object */
#define duda_package_main()                                             \
    _duda_package_bootstrap(struct duda_api_objects *dapi,              \
                            struct web_service *ws) {                   \
        monkey   = dapi->monkey;                                        \
        map      = dapi->map;                                           \
        msg      = dapi->msg;                                           \
        request  = dapi->request;                                       \
        response = dapi->response;                                      \
        debug    = dapi->debug;                                         \
        event    = dapi->event;                                         \
        gc       = dapi->gc;                                            \
        mem      = dapi->mem;                                           \
        console  = dapi->console;                                       \
        logger   = dapi->logger;                                        \
        param    = dapi->param;                                         \
        session  = dapi->session;                                       \
        cookie   = dapi->cookie;                                        \
        global   = dapi->global;                                        \
        qs       = dapi->qs;                                            \
        fconf    = dapi->fconf;                                         \
        conf     = dapi->conf;                                          \
        data     = dapi->data;                                          \
        worker   = dapi->worker;                                        \
        xtime    = dapi->xtime;                                         \
        mk_list_init(&duda_map_interfaces);                             \
        mk_list_init(&duda_map_urls);                                   \
        mk_list_init(&duda_global_dist);                                \
        mk_list_init(&duda_ws_packages);                                \
        mk_list_init(&duda_worker_list);                                \
        mk_list_init(&duda_logger_main_list);                           \
        mk_list_init(&duda_logger_worker_list);                         \
                                                                        \
        self = ws;                                                      \
                                                                        \
        return _duda_package_main();                                    \
    }                                                                   \
    duda_package_t *_duda_package_main()


/* Define package loader */
duda_package_t *duda_package_load(const char *pkgname,
                                  struct duda_api_objects *api,
                                  struct web_service *ws);

/*
#define duda_load_package(obj, pkg)                             \
    duda_package_t *p = duda_package_load(pkg, dapi, self); \
    obj = p->api;
*/
#endif
