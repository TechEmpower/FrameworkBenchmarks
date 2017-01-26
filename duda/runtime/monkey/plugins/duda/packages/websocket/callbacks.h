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
#include "request.h"

#ifndef WEBSOCKET_CALLBACKS_H
#define WEBSOCKET_CALLBACKS_H

/* Event callback codes */
#define WS_ON_OPEN           0x0
#define WS_ON_MESSAGE        0x1
#define WS_ON_ERROR          0x2
#define WS_ON_CLOSE          0x3
#define WS_ON_TIMEOUT        0x4

/* Struct to set the callbacks from duda_main() */
struct ws_callbacks_t {
    void (*on_open)   (duda_request_t *, ws_request_t *);
    void (*on_message)(duda_request_t *, ws_request_t *);
    void (*on_error)  (duda_request_t *, ws_request_t *);
    void (*on_close)  (duda_request_t *, ws_request_t *);
    void (*on_timeout)(duda_request_t *, ws_request_t *);
};

int ws_set_callback(int type, void (*callback) (duda_request_t *, struct ws_request *));
struct ws_callbacks_t *ws_callbacks;

#endif
