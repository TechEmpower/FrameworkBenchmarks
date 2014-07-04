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

#ifndef DUDA_PACKAGE_BASE64_H
#define DUDA_PACKAGE_BASE64_H
#include <stddef.h>

struct duda_api_base64 {
    unsigned char *(*encode) (const unsigned char *, size_t, size_t *);
    unsigned char *(*decode) (const unsigned char *, size_t, size_t *);
};

typedef struct duda_api_base64 base64_object_t;
base64_object_t *base64;

unsigned char *base64_encode(const unsigned char *src, size_t len,
                             size_t *out_len);
unsigned char *base64_decode(const unsigned char *src, size_t len,
                           size_t *out_len);
#endif
