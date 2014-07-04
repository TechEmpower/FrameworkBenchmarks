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
#include "duda_global.h"

#include "webservice.h"


/*
 * @OBJ_NAME: global
 * @OBJ_MENU: Global Worker
 * @OBJ_DESC: Duda stack works in threaded mode depending of the number of workers
 * spawn when started. The global object aims to provide interfaces to save references
 * for data which must be in the scope of the worker. The most common example
 * would be a database connection context.
 */

/*
 * @METHOD_NAME: init
 * @METHOD_DESC: Initialize a specific global key that will be used later in the
 * callbacks. This call MUST be used ONLY inside duda_main().
 * @METHOD_PROTO: void init(duda_global_t *key, void *callback(void) )
 * @METHOD_PARAM: key the global key variable that must be declared globally.
 * @METHOD_PARAM: callback this optional parameter points to a callback function
 * that will be invoked once duda_main() returns and before to enter the main
 * server loop. This callback is useful if you want to initialize some data in the
 * global key before the events start arriving.
 * @METHOD_RETURN: Do not return anything.
 */

/* REF: duda_global_init() is defined inside duda_object.h */

/*
 * @METHOD_NAME: set
 * @METHOD_DESC: Add a new value to the global key.
 * @METHOD_PROTO: int set(duda_global_t key, const void *data)
 * @METHOD_PARAM: key the global key previously initialized by the init() method.
 * @METHOD_PARAM: data the data you want to associate
 * @METHOD_RETURN: Upon successful completion, it returns zero. On error it returns
 * a negative number.
 */

int duda_global_set(duda_global_t global, const void *data)
{
    return pthread_setspecific(global.key, data);
}


/*
 * @METHOD_NAME: get
 * @METHOD_DESC: Retrieve the memory reference associated to the global key
 * @METHOD_PROTO: void *get(duda_global_t key)
 * @METHOD_PARAM: key the global key previously initialized by the init() method.
 * @METHOD_RETURN: Upon successful completion, it returns the memory address associated
 * with the global key. On error it returns NULL.
 */

void *duda_global_get(duda_global_t global)
{
    return pthread_getspecific(global.key);
}

struct duda_api_global *duda_global_object()
{
    struct duda_api_global *obj;

    obj = mk_api->mem_alloc(sizeof(struct duda_api_global));
    obj->init  = duda_global_init;
    obj->set   = duda_global_set;
    obj->get   = duda_global_get;

    return obj;
}
