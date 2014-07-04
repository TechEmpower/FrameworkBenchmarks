/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Duda I/O
 *  --------
 *  Copyright (C) 2012-2014, Eduardo Silva P. <edsiper@gmail.com>.
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
#include "duda_objects.h"
#include "duda_package.h"

#include "websocket.h"
#include "broadcast.h"
#include "request.h"
#include "callbacks.h"

/* API object */
struct duda_api_websockets *get_websockets_api()
{
    struct duda_api_websockets *ws;

    /* Alloc object */
    ws = monkey->mem_alloc(sizeof(struct duda_api_websockets));
    ws->handshake     = ws_handshake;
    ws->write         = ws_write;
    ws->broadcast     = ws_broadcast;
    ws->broadcast_all = ws_broadcast_all;
    ws->broadcaster   = ws_broadcaster;
    ws->set_callback  = ws_set_callback;

    return ws;
}

duda_package_t *duda_package_main()
{
    duda_package_t *dpkg;

    /* Package default configuration */
    ws_config = monkey->mem_alloc(sizeof(struct ws_config_t));
    ws_config->is_broadcast = MK_FALSE;

    /* Initialize callbacks */
    ws_callbacks = monkey->mem_alloc(sizeof(struct ws_callbacks_t));
    memset(ws_callbacks, '\0', sizeof(struct ws_callbacks_t));

    /* Package internals */
    global->init(&ws_request_list, cb_request_list_init, NULL);

    /* Package object */
    dpkg = monkey->mem_alloc(sizeof(duda_package_t));
    dpkg->name = "websocket";
    dpkg->version = "0.1";
    dpkg->api = get_websockets_api();

    return dpkg;
}
