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

#ifndef DUDA_GLOBAL_H
#define DUDA_GLOBAL_H

#include "MKPlugin.h"
#include <pthread.h>

typedef struct {
    pthread_key_t key;          /* Pthread key unique identifier */
    void *(*callback) (void *); /* Return the value assigned to the global variable */
    void *data;                 /* the optional data passed to the callback */
    struct mk_list _head;
} duda_global_t;

struct duda_global_dist_t {
    duda_global_t *key;
    void *(*callback) ();

    struct mk_list _head;
};

#define DUDA_GLOBAL_EXCEPTION "You can only define globals inside duda_init() or duda_package_main()"

/* Global data (thread scope) */
struct duda_api_global {
    void  (*init) (duda_global_t *, void *(*callback)(void *), void *data);
    int   (*set)   (duda_global_t, const void *);
    void *(*get)   (duda_global_t);
};

/* This list FIXME! */
struct mk_list duda_global_pkg;

int duda_global_set(duda_global_t key, const void *data);
void *duda_global_get(duda_global_t key);
struct duda_api_global *duda_global_object();

#endif
