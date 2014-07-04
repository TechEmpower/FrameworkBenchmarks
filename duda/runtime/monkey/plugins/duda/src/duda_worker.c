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

#include <pthread.h>

#include "duda.h"
#include "duda_api.h"
#include "duda_worker.h"

/* --- Local functions --- */

/*
 * This function is a middle step before to invoke the target function, the
 * goal is to be able to initialize some data in the worker before to release
 * the control of it. Originally created to create Logger instances per customs
 * workers that are not related to the main server workers.
 */
static void *duda_worker_step(void *arg)
{
    void *ret;
    struct duda_worker *wk = (struct duda_worker *) arg;

    /* initialize same data as done for server workers */
    _mkp_core_thctx();

    /* call the target function */
    ret = wk->func(wk->arg);
    mk_warn("User defined worker thread #%lu has ended",
            syscall(__NR_gettid));

    /* We do not handle threads exit, just put the thread to sleep */
    while (1) {
        sleep(60);
    }

    return ret;
}

/* Spawn each registered worker */
int duda_worker_spawn_all(struct mk_list *list)
{
    pthread_t tid;
    pthread_attr_t thread_attr;
    struct mk_list *head;
    struct duda_worker *wk;

    pthread_attr_init(&thread_attr);
    pthread_attr_setdetachstate(&thread_attr, PTHREAD_CREATE_DETACHED);

    /* spawn each worker */
    mk_list_foreach(head, list) {
        wk = mk_list_entry(head, struct duda_worker, _head);

        if (pthread_create(&tid, &thread_attr, duda_worker_step, (void *) wk) < 0) {
            perror("pthread_create");
            exit(EXIT_FAILURE);
        }
    }
    return tid;
}


/* --- API --- */

/*
 * @OBJ_NAME: worker
 * @OBJ_MENU: Workers
 * @OBJ_DESC: This object export different methods to spawn and manage
 * workers (posix threads).
 */

struct duda_api_worker *duda_worker_object()
{
    struct duda_api_worker *wk;

    wk = mk_api->mem_alloc(sizeof(struct duda_api_worker));
    wk->_spawn = duda_worker_spawn;

    return wk;
}


/*
 * @METHOD_NAME: spawn
 * @METHOD_DESC: It spawn a new worker thread. This method must be invoked from
 * duda_main().
 * @METHOD_PROTO: int spawn(void *(start_routine) (void *), void *arg)
 * @METHOD_PARAM: start_routine the routine or function that will be triggered under a thread context.
 * @METHOD_PARAM: arg a reference to the argument that will be passed to the function
 * @METHOD_RETURN: Upon successful completion it returns the worker id, on error
 * returns -1.
 */
int duda_worker_spawn(void *(start_routine) (void *), void *arg, struct mk_list *list)
{
    struct duda_worker *wk;

    /* alloc node */
    wk = mk_api->mem_alloc(sizeof(struct duda_worker));
    wk->id   = 0;
    wk->func = start_routine;
    wk->arg  = arg;

    /* link to main list */
    mk_list_add(&wk->_head, list);

    return 0;
}
