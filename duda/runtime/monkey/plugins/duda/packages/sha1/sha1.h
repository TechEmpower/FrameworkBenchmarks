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

#ifndef DUDA_PACKAGE_SHA1_H
#define DUDA_PACKAGE_SHA1_H
#include <stddef.h>

#define SHA_DIGEST_LENGTH 20

struct duda_api_sha1 {
    void (*encode) (const void *, unsigned char *, unsigned long);
};

typedef struct duda_api_sha1 sha1_object_t;
sha1_object_t *sha1;

typedef struct {
	unsigned long long size;
	unsigned int H[5];
	unsigned int W[16];
} blk_SHA_CTX;

void blk_SHA1_Init(blk_SHA_CTX *ctx);
void blk_SHA1_Update(blk_SHA_CTX *ctx, const void *dataIn, unsigned long len);
void blk_SHA1_Final(unsigned char hashout[20], blk_SHA_CTX *ctx);

#define SHA_CTX	    blk_SHA_CTX
#define SHA1_Init	blk_SHA1_Init
#define SHA1_Update	blk_SHA1_Update
#define SHA1_Final	blk_SHA1_Final

#endif // DUDA_PACKAGE_SHA1_H
