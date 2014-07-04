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

#ifndef DUDA_COOKIE_H
#define DUDA_COOKIE_H

#include "duda.h"

#define COOKIE_CRLF          "\r\n"
#define COOKIE_EQUAL         "="
#define COOKIE_SET           "Set-Cookie: "
#define COOKIE_HEADER        "Cookie:"
#define COOKIE_EXPIRE        "; expires="
#define COOKIE_PATH          "; path=/"
#define COOKIE_SEMICOLON     "; "
#define COOKIE_DELETED       "deleted"
#define COOKIE_EXPIRE_TIME   337606980
#define COOKIE_MAX_DATE_LEN  32

mk_pointer dd_cookie_crlf;
mk_pointer dd_cookie_equal;
mk_pointer dd_cookie_set  ;
mk_pointer dd_cookie_expire;
mk_pointer dd_cookie_expire_value;
mk_pointer dd_cookie_path;
mk_pointer dd_cookie_semicolon;

struct duda_api_cookie {
    int (*set) (duda_request_t *, char *, int, char *, int, int);
    int (*get) (duda_request_t *, char *, char **, int *);
    int (*cmp) (duda_request_t *, char *, char *);
    int (*destroy) (duda_request_t *, char *, int);
};

struct duda_api_cookie *duda_cookie_object();
int duda_cookie_set(duda_request_t *dr, char *key, int key_len,
                    char *val, int val_len, int expires);
int duda_cookie_get(duda_request_t *dr, char *key, char **val, int *val_len);
int duda_cookie_cmp(duda_request_t *dr, char *key, char *cmp);
int duda_cookie_destroy(duda_request_t *dr, char *key, int key_len);
#endif
