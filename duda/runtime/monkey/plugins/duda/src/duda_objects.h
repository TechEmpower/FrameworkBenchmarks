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

/*
 * Duda objects are struct passed by references to third party components of
 * the framework, like web services or packages.
 */

#include "mk_macros.h"
#include "duda_global.h"

#ifndef DUDA_OBJECTS_H
#define DUDA_OBJECTS_H

struct mk_list MK_EXPORT duda_map_interfaces;
struct mk_list MK_EXPORT duda_map_urls;
struct mk_list MK_EXPORT duda_global_dist;
struct mk_list MK_EXPORT duda_ws_packages;
struct mk_list MK_EXPORT duda_worker_list;
struct mk_list MK_EXPORT duda_logger_main_list;
struct mk_list MK_EXPORT duda_logger_worker_list;

/*
 * the _setup structure holds function references and flags
 * that modify the behavior of the web service
 */
struct duda_setup _setup;

/* Objects exported to the web service */
struct plugin_api *monkey;
struct duda_api_map *map;
struct duda_api_msg *msg;
struct duda_api_request *request;
struct duda_api_response *response;
struct duda_api_debug *debug;
struct duda_api_event *event;
struct duda_api_console *console;
struct duda_api_logger *logger;
struct duda_api_gc *gc;
struct duda_api_mem *mem;
struct duda_api_param *param;
struct duda_api_session *session;
struct duda_api_cookie *cookie;
struct duda_api_qs *qs;
struct duda_api_data *data;
struct duda_api_conf *conf;
struct duda_api_fconf *fconf;
struct duda_api_global *global;
struct duda_api_worker *worker;
struct duda_api_xtime *xtime;
struct web_service *self;

/* system headers */
#define _GNU_SOURCE         /* See feature_test_macros(7) */
#include <unistd.h>
#include <sys/syscall.h>   /* For SYS_xxx definitions */

/* function that depends on webservice or package specific data */
static inline void duda_global_init(duda_global_t *global,
                                    void *(*callback)(void *),
                                    void *data)
{
    pthread_key_create(&global->key, NULL);
    global->callback = callback;
    global->data     = data;
    mk_list_add(&global->_head, &duda_global_dist);
}

#endif
