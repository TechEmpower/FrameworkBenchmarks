/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Duda I/O
 *  --------
 *  Copyright (C) 2012, Eduardo Silva P. <edsiper@gmail.com>
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

#ifndef DUDA_API_MAP_H
#define DUDA_API_MAP_H

#include "duda.h"
#include "duda_objects.h"

/*
 * A static map entry, it allows to maps static URLs with
 * callbacks.
 */
struct duda_map_static_cb {
    char *path;
    int  path_len;

    char *cb_name;
    void (*callback) (duda_request_t *);

    struct mk_list _head;
};

/* Interfaces of the web service */
struct duda_interface {
    char *uid;
    unsigned int   uid_len;

    /* interface methods */
    struct mk_list methods;

    /* mk_list */
    struct mk_list _head;
};

/* Methods associated to an interface */
struct duda_method {
    char *uid;
    unsigned int uid_len;

    short int num_params;
    char *callback;
    void (*cb_webservice) (duda_request_t *);
    void (*cb_builtin)    (duda_request_t *);

    struct mk_list params;

    /* mk_list */
    struct mk_list _head;
};

/* Parameters: each method supports N parameters */
struct duda_param {
    char *name;
    short int max_len;

    /* mk_list */
    struct mk_list _head;
};


struct duda_api_map {
    /* Static stuff */
    #define static_add(p, cb) _static_add(p, cb, &duda_map_urls)
    #define static_add_ref(p, cb) _static_add_ref(p, cb, &duda_map_urls)
    #define static_root(cb) _static_root(self, cb)

    int (*_static_add) (const char *, const char *, struct mk_list *);
    int (*_static_add_ref) (const char *, void (*) (duda_request_t *), struct mk_list *);
    int (*_static_root) (struct web_service *, const char *);

    /* interface_ */
    #define add_interface(i)  _add_interface(i, &duda_map_interfaces)
    void (*_add_interface) (struct duda_interface *, struct mk_list *);

    struct duda_interface *(*interface_new) (char *);
    void (*interface_add_method) (struct duda_method *, struct duda_interface *);

    /* method_ */
    struct duda_method *(*method_new) (char *, char *, int);
    struct duda_method *(*method_builtin_new) (char *,
                                               void (*cb_builtin) (struct duda_request *),
                                               int n_params);

    void (*method_add_param) (struct duda_param *, struct duda_method *);

    /* param_ */
    struct duda_param *(*param_new) (char *, short int);
};

/* define some data types */
typedef struct duda_interface duda_interface_t;
typedef struct duda_method duda_method_t;
typedef struct duda_param duda_param_t;

duda_interface_t *duda_map_interface_new(char *uid);
duda_method_t *duda_map_method_new(char *uid, char *callback, int n_params);
duda_method_t *duda_map_method_builtin_new(char *uid,
                                       void (*cb_builtin) (duda_request_t *),
                                       int n_params);
duda_param_t *duda_map_param_new(char *uid, short int max_len);

void duda_map_interface_add_method(duda_method_t *method, duda_interface_t *iface);
void duda_map_method_add_param(duda_param_t *param, duda_method_t *method);
int duda_map_static_check(duda_request_t *dr);

struct duda_api_map *duda_map_object();

#endif
