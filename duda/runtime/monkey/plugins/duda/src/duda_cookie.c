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

#include <stdio.h>

#include "MKPlugin.h"
#include "duda_cookie.h"
#include "duda.h"

/*
 * @OBJ_NAME: cookie
 * @OBJ_MENU: Cookies
 * @OBJ_DESC: The cookie object provides a set of methods to handle HTTP cookies
 */

struct duda_api_cookie *duda_cookie_object()
{
    struct duda_api_cookie *c;

    c = mk_api->mem_alloc(sizeof(struct duda_api_cookie));
    c->set     = duda_cookie_set;
    c->get     = duda_cookie_get;
    c->cmp     = duda_cookie_cmp;
    c->destroy = duda_cookie_destroy;

    return c;
}

/*
 * @METHOD_NAME: set
 * @METHOD_DESC: It creates a new cookie
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: key the cookie name
 * @METHOD_PARAM: key_len the key string length
 * @METHOD_PARAM: val the cookie value
 * @METHOD_PARAM: val_len the value string length
 * @METHOD_PARAM: expires defines the expiration time in unix time seconds. Use value 0 if
 * you dont want to make the cookie expire
 * @METHOD_RETURN: Upon successful completion it returns 0, on error it returns -1
 */
int duda_cookie_set(duda_request_t *dr, char *key, int key_len,
                    char *val, int val_len, int expires)
{
    mk_pointer exp;

    /*
     * Ugly hack: every session_request.headers contains a _extra_rows mk_iov
     * entry to add customized HTTP response headers. This is mostly used from
     * the plugins, as well by Duda when sending a response->header(...).
     *
     * We are in a very specific case, where the developer would like to set
     * some HTTP cookies, we want to avoid memory allocations if possible when
     * working with strings, so mk_api->header_add(..) cannot be used here as a
     * Cookie is composed by a few components like: key, value, expiration, etc.
     *
     * So here we will do a similar routine as mk_plugin_header_add(), if _extra_rows
     * has not been allocated, we will do it by our selfs and add the required iov
     * entries to compose the Cookie row.
     */
    if (!dr->sr->headers._extra_rows) {
        dr->sr->headers._extra_rows = mk_api->iov_create(MK_PLUGIN_HEADER_EXTRA_ROWS * 2, 0);
    }

    /* Add 'Set-Cookie: ' */
    mk_api->iov_add_entry(dr->sr->headers._extra_rows, COOKIE_SET, sizeof(COOKIE_SET) -1,
                          dd_iov_none, MK_IOV_NOT_FREE_BUF);

    /* Append 'KEY=' */
    mk_api->iov_add_entry(dr->sr->headers._extra_rows, key, key_len,
                          dd_cookie_equal, MK_IOV_NOT_FREE_BUF);

    /* Append 'VALUE; path=' */
    mk_api->iov_add_entry(dr->sr->headers._extra_rows, val, val_len,
                          dd_cookie_path, MK_IOV_NOT_FREE_BUF);


    /*
     * Append 'PATH' and ending string depending of the expiration type
     *
     * Destroy a cookie
     */
    if (expires == COOKIE_EXPIRE_TIME) {
        mk_api->iov_add_entry(dr->sr->headers._extra_rows, dr->appname.data, dr->appname.len,
                              dd_cookie_expire, MK_IOV_NOT_FREE_BUF);
        mk_api->iov_add_entry(dr->sr->headers._extra_rows,
                              dd_cookie_expire_value.data,
                              dd_cookie_expire_value.len,
                              dd_iov_none, MK_IOV_NOT_FREE_BUF);
        return 0;
    }

    /* If the expire time was set */
    if (expires > 0) {
        exp.data = mk_api->mem_alloc(COOKIE_MAX_DATE_LEN);
        exp.len = mk_api->time_to_gmt(&exp.data, expires);

        mk_api->iov_add_entry(dr->sr->headers._extra_rows, dr->appname.data, dr->appname.len,
                              dd_cookie_expire, MK_IOV_NOT_FREE_BUF);
        mk_api->iov_add_entry(dr->sr->headers._extra_rows,
                              exp.data, exp.len, dd_iov_none, MK_IOV_FREE_BUF);

        return 0;
    }

    mk_api->iov_add_entry(dr->sr->headers._extra_rows, dr->appname.data, dr->appname.len,
                          dd_cookie_crlf, MK_IOV_NOT_FREE_BUF);
    return 0;
}

/*
 * @METHOD_NAME: destroy
 * @METHOD_DESC: It destroy a cookie. This method tells the HTTP client to invalidate
 * the cookie setting up a new expire time based in a past date. The changes should take
 * effect in the next request.
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: key the cookie name
 * @METHOD_PARAM: key_len the key string length
 * @METHOD_RETURN: Upon successful completion it returns 0, on error it returns -1.
 */
int duda_cookie_destroy(duda_request_t *dr, char *key, int key_len)
{
    return duda_cookie_set(dr, key, key_len, COOKIE_DELETED, sizeof(COOKIE_DELETED) -1,
                           COOKIE_EXPIRE_TIME);
}

/*
 * @METHOD_NAME: get
 * @METHOD_DESC: Retrieve a specific cookie value sent by the HTTP client.
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: key the cookie name
 * @METHOD_PARAM: val stores the cookie value
 * @METHOD_PARAM: val_len stores the string length of the cookie value
 * @METHOD_RETURN: Upon successful completion it returns 0, on error it returns -1.
 */
int duda_cookie_get(duda_request_t *dr, char *key, char **val, int *val_len)
{
    int i, offset;
    int pos_key, pos_val;
    int length;
    int header_len = sizeof(COOKIE_HEADER) - 1;
    char *cookie;
    struct headers_toc *toc;

    /* Get headers table-of-content (TOC) */
    toc = &dr->sr->headers_toc;
    for (i=0; i < toc->length; i++) {
        /* Compare header title */
        if (strncmp(toc->rows[i].init, COOKIE_HEADER, header_len) == 0) {
            break;
        }
    }

    if (i == toc->length) {
        return -1;
    }

    /* Look for 'cookie key' */
    /* FIXME:
     * we must handle the case where the user set two cookies with transversal name/values,
     * like:
     *
     *   Set-Cookie: name=bob
     *   Set-Cookie: bob=name
     *
     * That code will generate this in the browser:
     *
     *   Cookie: name=bob; bob=name;
     *
     * If someone try to lookup the key 'bob' it will not be found.
     *
     */
    cookie = toc->rows[i].init + header_len;
    pos_key = mk_api->str_search(cookie, key, MK_STR_SENSITIVE);
    if (pos_key == -1) {
        return -1;
    }
    length = (toc->rows[i].end - toc->rows[i].init) - header_len;

    /* Get value position */
    pos_val = mk_api->str_search_n(cookie + pos_key, "=", MK_STR_SENSITIVE, length - pos_key);
    if (pos_val == -1) {
        return -1;
    }
    pos_val++;

    /* Get the value ending position */
    offset = pos_key + pos_val;
    for (i = offset; i < length; i++) {
        if (cookie[i] == ';') {
            break;
        }
    }

    /* Set results */
    *val     = (cookie + offset);
    *val_len = (i - offset);

    return 0;
}

/*
 * @METHOD_NAME: cmp
 * @METHOD_DESC: Compares a cookie value with the given string
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: key the cookie name
 * @METHOD_PARAM: cmp the comparisson string
 * @METHOD_RETURN: If the cookie value is equal to the given string, it returns 0. Otherwise
 * it returns -1.
 */
int duda_cookie_cmp(duda_request_t *dr, char *key, char *cmp)
{
    int ret;
    int val_len;
    int cmp_len;
    char *value;

    ret = duda_cookie_get(dr, key, &value, &val_len);
    if (ret == -1) {
        return -1;
    }

    cmp_len = strlen(cmp);
    if (cmp_len != val_len) {
        return -1;
    }

    if (strncmp(value, cmp, val_len) == 0) {
        return 0;
    }

    return -1;
}
