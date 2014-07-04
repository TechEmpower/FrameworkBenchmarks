/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Duda I/O
 *  --------
 *  Copyright (C) 2012, Eduardo Silva P. <edsiper@gmail.com>
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

#include "duda.h"

#ifndef DUDA_GC_H
#define DUDA_GC_H

/* Garbage Collector object: gc->x() */
struct duda_api_gc {
    int (*add) (duda_request_t *dr, void *p);
};

/* Exported functions */
int duda_gc_init(duda_request_t *dr);
int duda_gc_add(duda_request_t *dr, void *p);
void *duda_gc_alloc(duda_request_t *dr, const size_t size);
int duda_gc_free_content(duda_request_t *dr);
int duda_gc_free(duda_request_t *dr);
struct duda_api_gc *duda_gc_object();

#endif
