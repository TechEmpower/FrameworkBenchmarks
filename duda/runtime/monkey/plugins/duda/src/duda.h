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

#include "MKPlugin.h"

#include "duda_webservice.h"
#include "duda_conf.h"
#include "duda_gc_map.h"
#include "duda_qs_map.h"

#ifndef DUDA_MAIN_H
#define DUDA_MAIN_H

#define MAP_WS_APP_NAME   0X00
#define MAP_WS_INTERFACE  0X10
#define MAP_WS_METHOD     0X20
#define MAP_WS_PARAM      0X30
#define MAP_WS_END        0X40

/* Max number of parameters allowed in Duda URI */
#define MAP_WS_MAX_PARAMS 8

/*
 * This struct represent the web service request, as well it contains detailed
 * information about the response type and buffers associated
 */
typedef struct duda_request {

    /* web service details */
    struct web_service *ws_root;

    mk_pointer appname;
    mk_pointer interface;
    mk_pointer method;
    mk_pointer params[MAP_WS_MAX_PARAMS];
    short int n_params;

    /* Monkey request data: plugin, client_session & session_request */
    int socket;
    struct plugin *plugin;
    struct client_session *cs;
    struct session_request *sr;

    /* Static map */

    /* Method structure */
    struct duda_method *_method;

    /* Callback functions */
    void (*end_callback) (struct duda_request *);

    /* Internal statuses */
    long _st_http_content_length;         /* Fixed content length size     */
    unsigned int _st_http_headers_sent;   /* HTTP headers sent ?           */
    unsigned int _st_http_headers_off;    /* should we send headers ?      */
    unsigned int _st_body_writes;         /* Number of body_writes invoked */

    /* Query string */
    struct duda_qs_map qs;

    /* Gargabe collector */
    struct duda_gc_map gc;

    /* Data queues */
    struct mk_list queue_out;

    /* Lists linked to (events)*/
    struct mk_list _head_events_write;

    /* Head to red-black tree list that holds all DRs */
    struct rb_node _rb_head;

} duda_request_t;


/* self identifier for the plugin context inside Monkey internals */
struct plugin *duda_plugin;

pthread_key_t duda_global_events_write;
pthread_key_t duda_global_dr_list;
pthread_mutex_t duda_mutex_thctx;

mk_pointer dd_iov_none;

void *duda_load_library(const char *path);
void *duda_load_symbol(void *handle, const char *symbol);
int duda_service_end(duda_request_t *dr);

duda_request_t *duda_dr_list_get(int socket);

#endif
