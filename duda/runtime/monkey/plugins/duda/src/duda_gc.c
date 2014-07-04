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

#include "duda.h"
#include "duda_api.h"
#include "duda_gc.h"

/*
 * @OBJ_NAME: gc
 * @OBJ_MENU: Garbage Collector
 * @OBJ_DESC: The Garbage Collector is an internal interface to register and free
 * allocated chunks of memory that needs to be released once a specific request
 * context has ended. Due to the nature of callbacks in the events handler, a request
 * can finalize on any moment, if some memory was allocated in a callback and the
 * response depends of this memory, the Garbage Collector must take care of it to avoid
 * memory leaks.
 */

int duda_gc_init(duda_request_t *dr)
{
    size_t size = (sizeof(struct duda_gc_entry) * DUDA_GC_ENTRIES);

    dr->gc.used = 0;
    dr->gc.size = DUDA_GC_ENTRIES;
    dr->gc.cells = mk_api->mem_alloc_z(size);

    if (!dr->gc.cells) {
        return -1;
    }

    return 0;
}

/*
 * @METHOD_NAME: add
 * @METHOD_DESC: It register a reference of a memory address that the Garbage Collector
 * must free once the main request context ends.
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: p  pointer to the target memory reference
 * @METHOD_RETURN: On success it returns zero, on error -1.
 */

int duda_gc_add(duda_request_t *dr, void *p)
{
    int i;
    int new_size;
    struct duda_gc_entry *tmp;

    /* add more cells if we are running out of space */
    if (dr->gc.used >= dr->gc.size) {
        new_size = (dr->gc.size * DUDA_GC_ENTRIES +
                    (sizeof(struct duda_gc_entry) * DUDA_GC_CHUNK));

        tmp = mk_api->mem_realloc(dr->gc.cells, new_size);
        if (tmp) {
            dr->gc.cells = tmp;
            dr->gc.size  = dr->gc.size + DUDA_GC_CHUNK;
        }
        else {
            return -1;
        }
    }

    /* register new entry */
    for (i = 0; i < dr->gc.size; i++) {
        if (dr->gc.cells[i].status == 0) {
            dr->gc.cells[i].p      = p;
            dr->gc.cells[i].status = 1;
            dr->gc.used++;
            return 0;
        }
    }


    /* something went really wrong */
    return -1;
}

void *duda_gc_alloc(duda_request_t *dr, const size_t size)
{
    void *p = NULL;


    p = mk_api->mem_alloc(size);
    if (!p) {
        return NULL;
    }

    duda_gc_add(dr, p);
    return p;
}

int duda_gc_free_content(duda_request_t *dr)
{
    int i;
    int freed = 0;

    /* free all registered entries in the GC array */
    for (i = 0; i < dr->gc.size && dr->gc.used > 0; i++) {
        if (dr->gc.cells[i].status == 1) {
            mk_api->mem_free(dr->gc.cells[i].p);
            dr->gc.used--;
            dr->gc.cells[i].p = NULL;
            dr->gc.cells[i].status = 0;
            freed++;
        }
    }

    return freed;
}

int duda_gc_free(duda_request_t *dr)
{
    mk_api->mem_free(dr->gc.cells);
    return 0;
}

struct duda_api_gc *duda_gc_object()
{
    struct duda_api_gc *obj;

    obj = mk_api->mem_alloc(sizeof(struct duda_api_gc));
    obj->add = duda_gc_add;

    return obj;
}
