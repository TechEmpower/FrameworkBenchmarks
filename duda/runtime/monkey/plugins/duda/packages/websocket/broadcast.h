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

#include "duda_package.h"

#ifndef WEBSOCKET_BROADCAST_H
#define WEBSOCKET_BROADCAST_H

#define BROADCAST_BUFFER    1024  /* 1Kb */

int ws_broadcast_count;

pthread_mutex_t ws_spawn_mutex;

struct ws_broadcast_worker {
    pid_t pid;
    int channel;
    struct mk_list *conn_list;
};

struct ws_broadcast_t {
    int wid;
    int pipe[2];
    struct mk_list *request_list;
    struct mk_list _head;
};

struct ws_broadcast_frame {
    int source;                            /* origin                 */
    uint64_t len;                          /* data length            */
    int type;                              /* opcode TEXT or BINARY  */
    int channel;                           /* channel                */
    unsigned char data[BROADCAST_BUFFER];  /* data to be broadcasted */
};

struct mk_list ws_broadcast_channels;

void ws_broadcast_worker(void *args);
int ws_broadcast(ws_request_t *wr, unsigned char *data,
                 uint64_t len, int msg_type, int channel);
int ws_broadcast_all(unsigned char *data, uint64_t len,
                     int msg_type, int channel);

int ws_broadcaster();

#endif
