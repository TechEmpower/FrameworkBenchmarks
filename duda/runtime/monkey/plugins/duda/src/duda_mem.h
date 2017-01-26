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

#ifndef DUDA_MEM_H
#define DUDA_MEM_H

#include "mk_memory.h"

struct duda_api_mem {
    void *(*alloc)   (const size_t);
    void *(*alloc_z) (const size_t);
    void *(*realloc) (void *, const size_t);
    void  (*free)    (void *);
};

struct duda_api_mem *duda_mem_object();

#endif
