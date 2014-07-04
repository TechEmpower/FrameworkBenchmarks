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

#ifndef DUDA_PACKAGE_REDIS_H
#define DUDA_PACKAGE_REDIS_H

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include "hiredis.h"
#include "async.h"

#include "duda_api.h"
#include "webservice.h"

pthread_key_t redis_key;

typedef struct duda_redis {
    
    redisAsyncContext *rc;
    duda_request_t *dr;
    struct mk_list _head_redis_fd;
    
} duda_redis_t;

struct duda_api_redis {

    /* redis functions */
    redisAsyncContext *(*connect) (const char *, int, 
                                   duda_request_t *);
    void (*disconnect) (redisAsyncContext *);
    int (*attach) (redisAsyncContext *, duda_request_t *);
    int (*setConnectCallback) (redisAsyncContext *,
                               void (*)(const redisAsyncContext *, int));
    int (*setDisconnectCallback) (redisAsyncContext *,
                                  void (*)(const redisAsyncContext *, int));
    int (*command) (redisAsyncContext *, 
                    void (*) (redisAsyncContext*, void*, void*), void *, 
                    const char *,...);
    void (*free) (redisAsyncContext *);
    duda_request_t * (*getDudarequest) (const redisAsyncContext *);
};

typedef struct duda_api_redis redis_object_t;

redis_object_t *redis;

redisAsyncContext * redis_connect(const char *ip, int port, duda_request_t *dr);
void redis_disconnect(redisAsyncContext *rc);
int redis_attach(redisAsyncContext *rc, duda_request_t *dr);
int redis_init();
duda_request_t * redis_request_map(const redisAsyncContext *rc);
void redis_free(redisAsyncContext *rc);

void redisAddRead(void *privdata);
void redisDel(void *privdata);
void redisAddWrite(void *privdata);

int redis_read(int fd, struct duda_request *dr);
int redis_write(int fd, struct duda_request *dr);
int redis_error(int fd, struct duda_request *dr);
int redis_close(int fd, struct duda_request *dr);
int redis_timeout(int fd, struct duda_request *dr);

#endif
