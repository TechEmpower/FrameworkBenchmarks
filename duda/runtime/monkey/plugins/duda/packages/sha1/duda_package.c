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

/*
 * @OBJ_NAME: sha1
 * @OBJ_MENU: SHA1
 * @OBJ_DESC: The SHA1 package provides the cryptographic hash function to encode
 * any data with the SHA1 algorithm.
 * @PKG_HEADER: #include "packages/sha1/sha1.h"
 * @PKG_INIT: duda_load_package(sha1, "sha1");
 */

#include <stdlib.h>

#include "duda_package.h"
#include "sha1.h"

/*
 * @METHOD_NAME: encode
 * @METHOD_DESC: It encodes an input data stream of specified length with SHA1 
 * algorithm and writes the output to data_out buffer. 
 * @METHOD_PROTO: void encode(const void *data_in, unsigned char *data_out, unsigned long length)
 * @METHOD_PARAM: data_in the source data to be encoded
 * @METHOD_PARAM: data_out the buffer where the encoded data is written
 * @METHOD_PARAM: length the length of data_in
 * @METHOD_RETURN: None
 */
static void sha1_encode (const void *data_in, unsigned char *data_out,
                         unsigned long length)
{
    SHA_CTX sha;
    SHA1_Init(&sha);
    SHA1_Update(&sha, data_in, length);
    SHA1_Final(data_out, &sha);
}

struct duda_api_sha1 *get_sha1_api()
{
    struct duda_api_sha1 *sha1;

    /* Alloc object */
    sha1 = monkey->mem_alloc(sizeof(struct duda_api_sha1));

    /* Map API calls */
    sha1->encode = sha1_encode;

    return sha1;
}

duda_package_t *duda_package_main()
{
    duda_package_t *dpkg;

    /* Package object */
    dpkg = monkey->mem_alloc(sizeof(duda_package_t));
    dpkg->name = "sha1";
    dpkg->version = "0.1";
    dpkg->api = get_sha1_api();

    return dpkg;
}
