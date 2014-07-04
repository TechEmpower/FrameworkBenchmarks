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

#ifndef DUDA_PARAM_H
#define DUDA_PARAM_H

#include "duda.h"

struct duda_api_param {
    char *(*get)       (duda_request_t *, short int);
    int   (*get_number)(duda_request_t *, short int, long *);
    short int (*count) (duda_request_t *);
    short int (*len)   (duda_request_t *, short int);
};

struct duda_api_param *duda_param_object();
char *duda_param_get(duda_request_t *dr, short int i);
int duda_param_get_number(duda_request_t *dr, short int idx, long *res);
short int duda_param_count(duda_request_t *dr);
short int duda_param_len(duda_request_t *dr, short int idx);

#endif
