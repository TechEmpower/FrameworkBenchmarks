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

#include "duda_api.h"
#include "duda_package.h"
#include "unqlite.h"
#include "kv.h"


/*
 * @OBJ_NAME: kv
 * @OBJ_MENU: Key/Value Store
 * @OBJ_DESC: This object provides an in memory Key/Value (KV) store engine. It allow
 * the creation of many instances across the service, is thread safe and can be used
 * inside any callback or customized worker. This package is a wrapper of the third party
 * Unqlite KV store which is distributed with Duda I/O stack.
 * @PKG_HEADER: #include "packages/kv/kv.h"
 * @PKG_INIT: duda_load_package(kv, "kv");
 */


/*
 * @METHOD_NAME: init
 * @METHOD_DESC: It initialize a KV store in memory. This method must be invoked from
 * duda_main().
 * @METHOD_PROTO: int init(kv_conn_t *store);
 * @METHOD_PARAM: store a pointer of the KV store connection type. It must be passed
 * as a reference.
 * @METHOD_RETURN: On success it returns DUDA_KV_OK, any other return value means an
 * error allocating memory for the store (unlikely).
 */

int kv_init(unqlite **conx)
{
    return unqlite_open(conx, ":mem:", UNQLITE_OPEN_IN_MEMORY);
}

/*
 * @METHOD_NAME: close
 * @METHOD_DESC: It release all resources associated to a KV store and perform a clean
 * close.
 * @METHOD_PROTO: int close(kv_conn_t *store);
 * @METHOD_PARAM: store a pointer of the KV store connection type.
 * @METHOD_RETURN: On success it returns DUDA_KV_OK or: DUDA_KV_BUSY when the store is busy
 * due to a previous lock, DUDA_KV_IOERR due to specific OS error or DUDA_KV_ABORT when
 * another thread have already released the store handle.
 */

/*
 * @METHOD_NAME: store
 * @METHOD_DESC: Create a new record into the database, if it already exist, the new data
 * will overwrite the old one.
 * @METHOD_PROTO: int store(kv_conn_t *store, void *key, int key_len, void *data, kv_int_t data_len)
 * @METHOD_PARAM: store    a pointer of the KV store connection type.
 * @METHOD_PARAM: key      the key name. It cannot be NULL.
 * @METHOD_PARAM: key_len  the key string length, if is unknown a -1 can be passed.
 * @METHOD_PARAM: data     the data that will be associated to the key.
 * @METHOD_PARAM: data_len the data length in bytes.
 * @METHOD_RETURN: On success it returns DUDA_KV_OK or: DUDA_KV_BUSY when the store is busy
 * due to a previous lock, DUDA_KV_IOERR due to specific OS error or DUDA_KV_ABORT when
 * another thread have already released the store handle.
 */

/*
 * @METHOD_NAME: store_fmt
 * @METHOD_DESC: Create a new record into the database, if it already exist, the new data
 * will overwrite the old one. Very similar to store() method but this uses a printf-like
 * style to format a data string.
 * @METHOD_PROTO: int store_fmt(kv_conn_t *store, void *key, int key_len, char *format, ...)
 * @METHOD_PARAM: store    a pointer of the KV store connection type.
 * @METHOD_PARAM: key      the key name. It cannot be NULL.
 * @METHOD_PARAM: key_len  the key string length, if is unknown a -1 can be passed.
 * @METHOD_PARAM: format   the format string, similar to printf.
 * @METHOD_PARAM: ...      the format arguments.
 * @METHOD_RETURN: On success it returns DUDA_KV_OK or: DUDA_KV_BUSY when the store is busy
 * due to a previous lock, DUDA_KV_IOERR due to specific OS error or DUDA_KV_ABORT when
 * another thread have already released the store handle.
 */

/*
 * @METHOD_NAME: append
 * @METHOD_DESC: It appends a bytes of data to the given key. If the key do not exists it's
 * created.
 * @METHOD_PROTO: int append(kv_conn_t *store, void *key, int key_len, char *format, ...)
 * @METHOD_PARAM: store    a pointer of the KV store connection type.
 * @METHOD_PARAM: key      the key name. It cannot be NULL.
 * @METHOD_PARAM: key_len  the key string length, if is unknown a -1 can be passed.
 * @METHOD_PARAM: data     the data that will be associated to the key.
 * @METHOD_PARAM: data_len the data length in bytes.
 * @METHOD_RETURN: On success it returns DUDA_KV_OK or: DUDA_KV_BUSY when the store is busy
 * due to a previous lock, DUDA_KV_IOERR due to specific OS error or DUDA_KV_ABORT when
 * another thread have already released the store handle.
 */

/*
 * @METHOD_NAME: append_fmt
 * @METHOD_DESC: It appends a bytes of data to the given key. If the key do not exists it's
 * created. Very similar to append() method, but this uses a printf-like style to format a
 * data string.
 * @METHOD_PROTO: int append_fmt(kv_conn_t *store, void *key, int key_len, char *format, ...)
 * @METHOD_PARAM: store    a pointer of the KV store connection type.
 * @METHOD_PARAM: key      the key name. It cannot be NULL.
 * @METHOD_PARAM: key_len  the key string length, if is unknown a -1 can be passed.
 * @METHOD_PARAM: format   the format string, similar to printf.
 * @METHOD_PARAM: ...      the format arguments.
 * @METHOD_RETURN: On success it returns DUDA_KV_OK or: DUDA_KV_BUSY when the store is busy
 * due to a previous lock, DUDA_KV_IOERR due to specific OS error or DUDA_KV_ABORT when
 * another thread have already released the store handle.
 */

/*
 * @METHOD_NAME: fetch
 * @METHOD_DESC: It appends a bytes of data to the given key. If the key do not exists it's
 * created. Very similar to append() method, but this uses a printf-like style to format a
 * data string.
 * @METHOD_PROTO: int append_fmt(kv_conn_t *store, void *key, int key_len, char *format, ...)
 * @METHOD_PARAM: store    a pointer of the KV store connection type.
 * @METHOD_PARAM: key      the key name. It cannot be NULL.
 * @METHOD_PARAM: key_len  the key string length, if is unknown a -1 can be passed.
 * @METHOD_PARAM: format   the format string, similar to printf.
 * @METHOD_PARAM: ...      the format arguments.
 * @METHOD_RETURN: On success it returns DUDA_KV_OK or: DUDA_KV_BUSY when the store is busy
 * due to a previous lock, DUDA_KV_IOERR due to specific OS error or DUDA_KV_ABORT when
 * another thread have already released the store handle.
 */

struct duda_api_kv *get_kv_api()
{
    struct duda_api_kv *kv;

    /* Alloc object */
    kv = monkey->mem_alloc(sizeof(struct duda_api_kv));
    kv->init           = kv_init;
    kv->close          = unqlite_close;
    kv->store          = unqlite_kv_store;
    kv->store_fmt      = unqlite_kv_store_fmt;
    kv->append         = unqlite_kv_append;
    kv->append_fmt     = unqlite_kv_append_fmt;
    kv->fetch          = unqlite_kv_fetch;
    kv->fetch_callback = unqlite_kv_fetch_callback;
    kv->delete         = unqlite_kv_delete;

    /* Cursor / Iterator */
    kv->cursor_init    = unqlite_kv_cursor_init;
    kv->cursor_release = unqlite_kv_cursor_release;
    kv->cursor_reset   = unqlite_kv_cursor_reset;
    kv->cursor_valid   = unqlite_kv_cursor_valid_entry;
    kv->cursor_first   = unqlite_kv_cursor_first_entry;
    kv->cursor_last    = unqlite_kv_cursor_last_entry;
    kv->cursor_next    = unqlite_kv_cursor_next_entry;
    kv->cursor_prev    = unqlite_kv_cursor_prev_entry;
    kv->cursor_key     = unqlite_kv_cursor_key;
    kv->cursor_data    = unqlite_kv_cursor_data;

    return kv;
}

duda_package_t *duda_package_main()
{
    duda_package_t *dpkg;

    /* Package object */
    dpkg = monkey->mem_alloc(sizeof(duda_package_t));
    dpkg->name    = "kv";
    dpkg->version = "0.1";
    dpkg->api     = get_kv_api();

    return dpkg;
}
