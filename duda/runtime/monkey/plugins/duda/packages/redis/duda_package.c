/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Duda I/O
 *  --------
 *  Copyright (C) 2012, Sourabh Chandak <sourabh3934@gmail.com>
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
#include "duda_package.h"
#include "redis.h"
#include "hiredis.h"
#include "async.h"


struct duda_api_redis *get_redis_api()
{
    struct duda_api_redis *redis;

    /* Alloc object */
    redis = malloc(sizeof(struct duda_api_redis));

    /* Map API calls */
    redis->connect               = redis_connect;
    redis->disconnect            = redis_disconnect;
    redis->attach                = redis_attach;
    redis->setConnectCallback    = redisAsyncSetConnectCallback;
    redis->setDisconnectCallback = redisAsyncSetDisconnectCallback;
    redis->command               = redisAsyncCommand;
    redis->getDudarequest        = redis_request_map;
    redis->free                  = redis_free;

    return redis;
}

duda_package_t *duda_package_main(struct duda_api_objects *api)
{
    duda_package_t *dpkg;

    /* Initialize package internals */
    duda_package_init();

    /* Init redis*/
    redis_init();

    /* Package object */
    dpkg = monkey->mem_alloc(sizeof(duda_package_t));
    dpkg->name    = "redis";
    dpkg->version = "0.1";
    dpkg->api     = get_redis_api();

    return dpkg;
}
