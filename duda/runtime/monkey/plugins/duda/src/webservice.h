/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Duda I/O
 *  ------------------
 *  Copyright (C) 2012, Eduardo Silva P. <edsiper@gmail.com>
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

#ifndef DUDA_WEBSERVICE_H
#define DUDA_WEBSERVICE_H

/* System headers */
#include <sys/types.h>
#include <sys/syscall.h>

/* Monkey specifics */
#include "MKPlugin.h"
#include "duda_api.h"
#include "duda_map.h"
#include "duda_mem.h"
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

int __ws_loaded;
struct mk_list *__ws_head;

struct duda_webservice ws_info;
duda_package_t *pkg_temp;

/* Duda Macros */
#define DUDA_REGISTER(app_name, app_path)                 \
    struct duda_webservice ws_info = {app_name, app_path}


#define duda_load_package(object, package)                          \
    /* Check if this package was already loaded */                  \
    __ws_loaded = MK_FALSE;                                         \
                                                                    \
    mk_list_foreach(__ws_head, &duda_ws_packages) {                 \
        pkg_temp = mk_list_entry(__ws_head, duda_package_t, _head); \
        if (strcmp(pkg_temp->name, package) == 0) {                 \
            __ws_loaded = MK_TRUE;                                  \
            break;                                                  \
        }                                                           \
    }                                                               \
                                                                    \
    if (__ws_loaded == MK_FALSE) {                                  \
        pkg_temp = dapi->duda->package_load(package, dapi, self);   \
        mk_list_add(&pkg_temp->_head, &duda_ws_packages);           \
        object = pkg_temp->api;                                     \
    }

#define duda_service_add_interface(iface) do {              \
        mk_list_add(&iface->_head,  &duda_map_interfaces);  \
    } while(0);


#define duda_map_add_interface(iface) mk_list_add(&iface->_head,  duda_map_interfaces)

/* Invalid object messages */
#undef mk_info
#undef mk_warn
#undef mk_err
#undef mk_bug
#define _invalid_call     " is invalid, use msg->x() object instead"
#define mk_info(a, ...)   msg->err("mk_info()" _invalid_call)
#define mk_warn(a, ...)   msg->err("mk_warn()" _invalid_call)
#define mk_err(a, ...)    msg->err("mk_err()" _invalid_call)
#define mk_bug(a, ...)    msg->err("mk_bug()" _invalid_call)


struct duda_api_objects *duda_new_api_objects();


/* We declare the hidden _duda_main() function to avoid some warnings */
int _duda_main(struct duda_api_objects *dapi);

/*
 * This is the tricky initialization for the web service in question,
 * Duda core will locate the _duda_bootstrap() symbol and invoke the
 * function to set the global API objects and perform some basic data
 * initialization, then it invoke the end-user routine under _duda_main()
 */
#define duda_main()                                                     \
    _duda_bootstrap_main(struct duda_api_objects *dapi,                 \
                         struct web_service *ws) {                      \
        /* API Objects */                                               \
        monkey   = dapi->monkey;                                        \
        map      = dapi->map;                                           \
        msg      = dapi->msg;                                           \
        request  = dapi->request;                                       \
        response = dapi->response;                                      \
        debug    = dapi->debug;                                         \
        event    = dapi->event;                                         \
        console  = dapi->console;                                       \
        logger   = dapi->logger;                                        \
        gc       = dapi->gc;                                            \
        mem      = dapi->mem;                                           \
        param    = dapi->param;                                         \
        session  = dapi->session;                                       \
        cookie   = dapi->cookie;                                        \
        global   = dapi->global;                                        \
        qs       = dapi->qs;                                            \
        data     = dapi->data;                                          \
        conf     = dapi->conf;                                          \
        fconf    = dapi->fconf;                                         \
        worker   = dapi->worker;                                        \
        xtime    = dapi->xtime;                                         \
                                                                        \
        /* Reference to this web service */                             \
        self = ws;                                                      \
                                                                        \
        /* Setup initialization */                                      \
        _setup.event_signal_cb = NULL;                                  \
                                                                        \
        /* Initialize global linked lists */                            \
        mk_list_init(&duda_map_interfaces);                             \
        mk_list_init(&duda_map_urls);                                   \
        mk_list_init(&duda_global_dist);                                \
        mk_list_init(&duda_ws_packages);                                \
        mk_list_init(&duda_worker_list);                                \
                                                                        \
        /* logger main list: logger keys defined in duda_main() */      \
        mk_list_init(&duda_logger_main_list);                           \
                                                                        \
        /* logger worker: logger keys initialized through worker CBs */ \
        mk_list_init(&duda_logger_worker_list);                         \
                                                                        \
        /*                                                              \
         * re-map functions that depends on webservice or               \
         * package definitions (local data)                             \
         */                                                             \
        global->init = duda_global_init;                                \
                                                                        \
        /* Invoke end-user main routine */                              \
        return _duda_main(dapi);                                        \
    }                                                                   \
    int _duda_main(struct duda_api_objects *dapi)

#endif
