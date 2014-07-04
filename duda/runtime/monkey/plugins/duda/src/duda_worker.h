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

#ifndef DUDA_API_WORKER_H
#define DUDA_API_WORKER_H

/*
 * A simple struct/node to store the information of a working
 * thread that must me run at once duda_main() have been finished
 */
struct duda_worker {
    int id;
    void *(*func) (void *);
    void *arg;

    struct mk_list _head;
};

/* Worker object: worker->x() */
struct duda_api_worker {
    int (*_spawn) (void *(start_routine) (void *), void *, struct mk_list *);
};

int duda_worker_spawn_all(struct mk_list *list);
int duda_worker_spawn(void *(start_routine) (void *), void *arg, struct mk_list *list);
struct duda_api_worker *duda_worker_object();

#define spawn(routine, arg) _spawn(routine, arg, &duda_worker_list)

#endif
