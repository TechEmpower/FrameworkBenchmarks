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

#ifndef DUDA_QS_MAP_H
#define DUDA_QS_MAP_H

/*
 * We initially define that we will get no more than QS_ENTRIES_SIZE variables
 * in the query string. This is a fixed value to reduce the risk of a memory
 * consumption attack.
 */
#define QS_ENTRIES_SIZE 32

struct duda_qs_entry {
    mk_pointer key;
    mk_pointer value;
};

struct duda_qs_map {
    int count;         /* number of key/values in the query string */
    struct duda_qs_entry entries[QS_ENTRIES_SIZE];
};


#endif
