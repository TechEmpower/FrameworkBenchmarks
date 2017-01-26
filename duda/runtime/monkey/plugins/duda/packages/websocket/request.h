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

#ifndef WEBSOCKET_REQUEST_H
#define WEBSOCKET_REQUEST_H

#include <stdint.h>
#include "duda_api.h"
#include "mk_macros.h"
#include "mk_list.h"
#include "protocol.h"

struct ws_request
{
    int socket;
    int channel;
    struct duda_request *dr;

    /* callbacks */
    void (*cb_on_open)    (duda_request_t *, struct ws_request *);
    void (*cb_on_message) (duda_request_t *, struct ws_request *);
    void (*cb_on_error)   (duda_request_t *, struct ws_request *);
    void (*cb_on_close)   (duda_request_t *, struct ws_request *);
    void (*cb_on_timeout) (duda_request_t *, struct ws_request *);

    /* Protocol specifics */
    unsigned int  opcode;
    unsigned int  mask;
    unsigned char masking_key[WS_FRAME_MASK_LEN];
    unsigned char *payload;
    uint64_t payload_len;

    /* Client request data */
    struct client_session *cs;
    struct session_request *sr;

    struct mk_list _head;
};

typedef struct ws_request  ws_request_t;

/*
 * We use the Duda internals global object to handle the request list
 * at worker level
 */
duda_global_t ws_request_list;

/* Functions */
void *cb_request_list_init();
void ws_request_init();
struct ws_request *ws_request_create(int socket_fd,
                                     int channel,
                                     struct duda_request *dr,
                                     void (*on_open)   (duda_request_t *, ws_request_t *),
                                     void (*on_message)(duda_request_t *, ws_request_t *),
                                     void (*on_error)  (duda_request_t *, ws_request_t *),
                                     void (*on_close)  (duda_request_t *, ws_request_t *),
                                     void (*on_timeout)(duda_request_t *, ws_request_t *));
void ws_request_add(ws_request_t *pr);
ws_request_t *ws_request_get(int socket);
void ws_request_update(int socket, ws_request_t *wr);
int ws_request_delete(int socket);
void ws_free_request(int sockfd);

#endif
