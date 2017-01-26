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

#include "unqlite.h"

#ifndef DUDA_PACKAGE_KV_H
#define DUDA_PACKAGE_KV_H

#include "duda_api.h"
#include "webservice.h"

/* fetch return values */
#define DUDA_KV_OK         UNQLITE_OK
#define DUDA_KV_BUSY       UNQLITE_BUSY
#define DUDA_KV_NOTFOUND   UNQLITE_NOTFOUND
#define DUDA_KV_IOERR      UNQLITE_IOERR
#define DUDA_KV_NOMEM      UNQLITE_NOMEM

#define KV_CURSOR_FOREACH(c) \
    for (kv->cursor_first(c); c && kv->cursor_valid(c); kv->cursor_next(c))

typedef unqlite           kv_conn_t;
typedef unqlite_kv_cursor kv_cursor_t;
typedef unqlite_int64     kv_int_t;

struct duda_api_kv {
    int (*init)  (unqlite **);
    int (*close) (unqlite *);
    int (*store) (unqlite *, const void *, int, const void *, unqlite_int64);
    int (*store_fmt) (unqlite *, const void *, int, const char *, ...);
    int (*append) (unqlite *, const void *, int, const void *, unqlite_int64);
    int (*append_fmt) (unqlite *, const void *, int, const char *, ...);
    int (*fetch) (unqlite *, const void *, int, void *, unqlite_int64 *);
    int (*fetch_callback) (unqlite *, const void *, int,
                           int (*xConsumer)(const void *pData, unsigned int iDatalen,\
                                            void *pUserData),
                           void *);
    int (*delete) (unqlite *, const void *, int);

    /* Cursor / Iterators */
    int (*cursor_init)    (unqlite *pDb, unqlite_kv_cursor **ppOut);
    int (*cursor_release) (unqlite *pDb, unqlite_kv_cursor *pCur);
    int (*cursor_reset)   (unqlite_kv_cursor *pCursor);
    int (*cursor_valid)   (unqlite_kv_cursor *pCursor);
    int (*cursor_first)   (unqlite_kv_cursor *pCursor);
    int (*cursor_last)    (unqlite_kv_cursor *pCursor);
    int (*cursor_next)    (unqlite_kv_cursor *pCursor);
    int (*cursor_prev)    (unqlite_kv_cursor *pCursor);
    int (*cursor_key)     (unqlite_kv_cursor *pCursor, void *pBuf, int *pnByte);
    int (*cursor_data)    (unqlite_kv_cursor *pCursor, void *pBuf, unqlite_int64 *pnData);
};

typedef struct duda_api_kv kv_object_t;

kv_object_t *kv;

/* functions */
int kv_init();

#endif
