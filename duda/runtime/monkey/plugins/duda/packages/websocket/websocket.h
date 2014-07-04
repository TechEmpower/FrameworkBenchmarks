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

#ifndef DUDA_PACKAGE_WEBSOCKETS_H
#define DUDA_PACKAGE_WEBSOCKETS_H

#include "duda_api.h"
#include "request.h"
#include "callbacks.h"

struct ws_config_t {
    int is_broadcast;
};

struct duda_api_websockets {
    int (*handshake) (duda_request_t *, int);
    int (*write) (struct ws_request *, unsigned int, unsigned char *, uint64_t);
    int (*broadcast)     (ws_request_t *, unsigned char *, uint64_t, int, int);
    int (*broadcast_all) (unsigned char *, uint64_t, int, int);
    int (*broadcaster) ();
    int (*set_callback) (int type, void (*callback) (duda_request_t *, ws_request_t *));
};

int ws_handshake(duda_request_t *dr, int channel);

int ws_send_data(int sockfd,
                unsigned int fin,
                unsigned int rsv1,
                unsigned int rsv2,
                unsigned int rsv3,
                unsigned int opcode,
                uint64_t payload_len,
                unsigned char *payload_data);

int ws_write(struct ws_request *wr, unsigned int code, unsigned char *data, uint64_t len);

/* API Object */
struct duda_api_websockets *websocket;
struct ws_config_t *ws_config;

#endif

