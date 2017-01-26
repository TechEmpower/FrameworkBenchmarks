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

#ifndef DUDA_QS_H
#define DUDA_QS_H

struct duda_api_qs {
    int (*count)    (duda_request_t *);
    char *(*get)    (duda_request_t *, const char *);
    char *(*get_id) (duda_request_t *, int);
    int (*cmp) (duda_request_t *, const char *, const char *);
};


int duda_qs_parse(duda_request_t *dr);
int duda_qs_count(duda_request_t *dr);
char *duda_qs_get(duda_request_t *dr, const char *key);
char *duda_qs_get_id(duda_request_t *dr, int idx);
int duda_qs_cmp(duda_request_t *dr, const char *key, const char *value);

struct duda_api_qs *duda_qs_object();

#endif
