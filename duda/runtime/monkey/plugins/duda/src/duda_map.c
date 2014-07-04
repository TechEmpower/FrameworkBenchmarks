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

/*
 * @OBJ_NAME: map
 * @OBJ_MENU: Map Routing
 * @OBJ_DESC: The map object provides a set of methods to specify the URL's map
 * and callbacks associated.
 */

#include "duda_map.h"
#include "duda_gc.h"

#define DUDA_MAP_REDIR_SIZE  64

int duda_map_static_check(duda_request_t *dr)
{
    int len;
    int offset = 0;
    int port_redirect = 0;
    int redirect_size;
    char *buf;
    char *host;
    char *location = 0;
    struct mk_list *head;
    struct session_request *sr = dr->sr;
    struct duda_map_static_cb *st;

    mk_list_foreach(head, dr->ws_root->map_urls) {
        st = mk_list_entry(head, struct duda_map_static_cb, _head);

        if (!sr->uri_processed.data) {
            continue;
        }

        /* Check URI offset for name matching */
        if (dr->ws_root->is_root == MK_FALSE) {
            offset = dr->ws_root->name.len + 1;
        }

        if (strncmp(sr->uri_processed.data + offset,
                    st->path, st->path_len) == 0) {

            /*
             * Check if the web service request force redirection for
             * URLs not ending with a slash
             */
            if (!sr->query_string.data &&
                sr->uri_processed.data[sr->uri_processed.len - 1] != '/' &&
                dr->ws_root->url_force_redirect) {

                duda_response_http_status(dr, 301);

                if (dr->sr->host.data && sr->port > 0) {
                    if (dr->sr->port != mk_api->config->standard_port) {
                        port_redirect = dr->sr->port;
                    }
                }

                redirect_size = (DUDA_MAP_REDIR_SIZE + dr->sr->host.len +
                                 dr->sr->uri_processed.len + 2);

                buf = duda_gc_alloc(dr, redirect_size);
                host = mk_api->pointer_to_buf(dr->sr->host);
                duda_gc_add(dr, host);

                /*
                 * Add ending slash to the location string
                 */
                location = (char *) duda_gc_alloc(dr, dr->sr->uri_processed.len + 2);
                if (!location) {
                    /* FIXME: Need to raise memory problem message somewhere */
                    exit(EXIT_FAILURE);
                }

                memcpy(location, dr->sr->uri_processed.data, dr->sr->uri_processed.len);
                location[dr->sr->uri_processed.len]     = '/';
                location[dr->sr->uri_processed.len + 1] = '\0';

                if (port_redirect > 0) {
                    len = snprintf(buf, redirect_size,
                                   "Location: %s://%s:%i%s",
                                   mk_api->config->transport, host, port_redirect, location);
                }
                else {
                    len = snprintf(buf, redirect_size,
                                   "Location: %s://%s%s",
                                   mk_api->config->transport, host, location);
                }

                duda_response_http_header_n(dr, buf, len);
                duda_response_end(dr, NULL);
                return 0;
            }
            st->callback(dr);
            return 0;
        }
    }

    return -1;
}

/*
 * @METHOD_NAME: static_add
 * @METHOD_DESC: It maps a static URL address to a specific callback function
 * @METHOD_PARAM: path    the URL path, e.g: "/something".
 * @METHOD_PARAM: cb_name the callback function name, e.g: "cb_something"
 * @METHOD_PROTO: int static_add(const char *path, const char *cb_name)
 * @METHOD_RETURN: Upon successful completion it returns 0, on error returns -1.
 */
int duda_map_static_add(const char *path,  const char *cb_name, struct mk_list *list)
{
    struct duda_map_static_cb *st;

    st = mk_api->mem_alloc(sizeof(struct duda_map_static_cb));
    if (!st) {
        return -1;
    }

    st->path     = mk_api->str_dup(path);
    st->path_len = strlen(path);
    st->cb_name  = mk_api->str_dup(cb_name);
    st->callback = NULL;

    mk_list_add(&st->_head, list);

    return 0;
}

/*
 * @METHOD_NAME: static_root
 * @METHOD_DESC: It maps the root URL to a specific callback function
 * @METHOD_PARAM: cb_name the callback function name, e.g: "cb_something"
 * @METHOD_RETURN: Upon successful completion it returns 0, on error returns -1.
 */
int duda_map_static_root(struct web_service *ws, const char *cb_name)
{
    if (ws->map_root_name) {
        monkey->mem_free(ws->map_root_name);
    }
    ws->map_root_name = mk_api->str_dup(cb_name);
    return 0;
}

/*
 * @METHOD_NAME: static_add_ref
 * @METHOD_DESC: It maps a static URL address to a specific callback function by reference
 * @METHOD_PARAM: path    the URL path, e.g: "/something".
 * @METHOD_PARAM: callback the callback function reference
 * @METHOD_PROTO: int static_add_ref(const char *path, void (*callback) (duda_request_t *dr))
 * @METHOD_RETURN: Upon successful completion it returns 0, on error returns -1.
 */
int duda_map_static_add_ref(const char *path,  void (*cb) (duda_request_t *dr),
                            struct mk_list *list)
{
    struct duda_map_static_cb *st;

    st = mk_api->mem_alloc(sizeof(struct duda_map_static_cb));
    if (!st) {
        return -1;
    }

    st->path     = mk_api->str_dup(path);
    st->path_len = strlen(path);
    st->callback = cb;

    mk_list_add(&st->_head, list);

    return 0;
}

/*
 * @METHOD_NAME: interface_new
 * @METHOD_DESC: Creates a new interface map (non-static).
 * @METHOD_PARAM: uid the interface name
 * @METHOD_PROTO: duda_interface_t *interface_new(char *uid)
 * @METHOD_RETURN: Upon successful completion it returns the interface struct, on error returns NULL.
 */
duda_interface_t *duda_map_interface_new(char *uid)
{
    duda_interface_t *iface;

    iface = mk_api->mem_alloc(sizeof(duda_interface_t));
    if (!iface) {
        return NULL;
    }

    iface->uid     = uid;
    iface->uid_len = strlen(uid);
    mk_list_init(&iface->methods);

    return iface;
}

/*
 * @METHOD_NAME: interface_add_method
 * @METHOD_DESC: It adds a method to an interface map
 * @METHOD_PARAM: method the method to link
 * @METHOD_PARAM: iface  the root interface
 * @METHOD_PROTO: void interface_add_method(duda_method_t *method, duda_interface_t *iface)
 * @METHOD_RETURN: None
 */
void duda_map_interface_add_method(duda_method_t *method,
                                   duda_interface_t *iface)
{
    mk_list_add(&method->_head, &iface->methods);
}


/* Creates a new method */
duda_method_t *_duda_map_method_new(char *uid, char *cb_webservice,
                                    void (*cb_builtin)(duda_request_t *),
                                    int n_params)
{
    duda_method_t *method;

    method = mk_api->mem_alloc(sizeof(duda_method_t));
    method->uid     = uid;
    method->uid_len = strlen(uid);
    method->num_params = n_params;

    if (cb_webservice) {
        method->callback = cb_webservice;
        method->cb_webservice = NULL;
        method->cb_builtin = NULL;
    }
    else {
        method->callback = NULL;
        method->cb_webservice = NULL;
        method->cb_builtin = cb_builtin;
    }

    mk_list_init(&method->params);
    return method;
}

/*
 * @METHOD_NAME: method_new
 * @METHOD_DESC: It creates a new interface method
 * @METHOD_PARAM: uid the method name
 * @METHOD_PARAM: callback the callback function name
 * @METHOD_PARAM: n_params number of parameters allowed
 * @METHOD_PROTO: duda_method_t *method_new(char *uid, char *callback, int n_params)
 * @METHOD_RETURN: Returns the new method structure.
 */
duda_method_t *duda_map_method_new(char *uid, char *callback, int n_params)
{
    return _duda_map_method_new(uid, callback, NULL, n_params);
}

/* Creates a new method */
duda_method_t *duda_map_method_builtin_new(char *uid,
                                           void (*cb_builtin) (duda_request_t *),
                                           int n_params)
{
    return _duda_map_method_new(uid, NULL, cb_builtin, n_params);
}

/*
 * @METHOD_NAME: method_add_param
 * @METHOD_DESC: It adds a parameter to a method
 * @METHOD_PARAM: param the parameter struct
 * @METHOD_PARAM: method the method where the parameter will be linked
 * @METHOD_PROTO: void method_add_param(duda_param_t *param, duda_method_t *method)
 * @METHOD_RETURN: None
 */
void duda_map_method_add_param(duda_param_t *param, duda_method_t *method)
{
    mk_list_add(&param->_head, &method->params);
}

/*
 * @METHOD_NAME: param_new
 * @METHOD_DESC: It creates a new method parameter
 * @METHOD_PARAM: uid the parameter name
 * @METHOD_PARAM: size the maximum size allowed for the parameter value
 * @METHOD_PROTO: void param_new(char *uid, short int size)
 * @METHOD_RETURN: It returns the parameter struct
 */
duda_param_t *duda_map_param_new(char *uid, short int size)
{
    duda_param_t *param;

    param = mk_api->mem_alloc(sizeof(duda_param_t));
    param->name = uid;
    param->max_len = size;

    return param;
}

/*
 * @METHOD_NAME: add_interface
 * @METHOD_DESC: It register an interface tree with the system maps
 * @METHOD_PARAM: iface the root interface
 * @METHOD_PROTO: void add_interface(duda_interface_t *iface)
 * @METHOD_RETURN: None
 */
void duda_map_add_interface(duda_interface_t *iface, struct mk_list *list)
{
    mk_list_add(&iface->_head, list);
}

struct duda_api_map *duda_map_object()
{
    struct duda_api_map *obj;

    obj = mk_api->mem_alloc(sizeof(struct duda_api_map));
    obj->_static_add      = duda_map_static_add;
    obj->_static_add_ref  = duda_map_static_add_ref;
    obj->_static_root     = duda_map_static_root;
    obj->_add_interface = duda_map_add_interface;
    obj->interface_new = duda_map_interface_new;
    obj->interface_add_method = duda_map_interface_add_method;
    obj->method_new = duda_map_method_new;
    obj->method_builtin_new = duda_map_method_builtin_new;
    obj->method_add_param = duda_map_method_add_param;
    obj->param_new = duda_map_param_new;

    return obj;
}
