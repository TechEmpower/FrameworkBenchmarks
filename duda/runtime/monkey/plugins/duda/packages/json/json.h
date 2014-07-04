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

#ifndef DUDA_PACKAGE_JSON_H
#define DUDA_PACKAGE_JSON_H

#include "duda_package.h"
#include "cJSON.h"

typedef cJSON json_t;

struct duda_api_json {
    /* create item types */
    json_t *(*create_null) ();
    json_t *(*create_true) ();
    json_t *(*create_false) ();
    json_t *(*create_bool) ();
    json_t *(*create_number) ();
    json_t *(*create_string) ();
    json_t *(*create_array) ();
    json_t *(*create_object) ();

    /* add to */
    void (*add_to_array) (json_t *, json_t *);
    void (*add_to_object) (json_t *, const char *, json_t *);

    json_t *(*parse) (const char *);
    char   *(*print) (json_t *);
    char   *(*print_gc) (duda_request_t *, json_t *);
    char   *(*print_unformatted) (json_t *);
    char   *(*print_unformatted_gc) (duda_request_t *, json_t *);
    void    (*delete) (json_t *);
    int     (*get_array_size) (json_t *);
    json_t *(*get_array_item) (json_t *, int);
    json_t *(*get_object_item) (json_t *, const char *);
    const char *(*get_error) (void);

    /* Local methods */
    json_t *(*parse_data) (duda_request_t *);
};

typedef struct duda_api_json json_object_t;
json_object_t *json;

#endif
