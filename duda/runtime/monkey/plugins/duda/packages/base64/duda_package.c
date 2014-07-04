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

#include <stdlib.h>

#include "duda_package.h"
#include "base64.h"

struct duda_api_base64 *get_base64_api()
{
    struct duda_api_base64 *base64;

    /* Alloc object */
    base64 = monkey->mem_alloc(sizeof(struct duda_api_base64));

    /* Map API calls */
    base64->encode = base64_encode;
    base64->decode = base64_decode;

    return base64;
}

duda_package_t *duda_package_main()
{
    duda_package_t *dpkg;

    /* Package object */
    dpkg = monkey->mem_alloc(sizeof(duda_package_t));
    dpkg->name = "base64";
    dpkg->version = "0.1";
    dpkg->api = get_base64_api();

    return dpkg;
}
