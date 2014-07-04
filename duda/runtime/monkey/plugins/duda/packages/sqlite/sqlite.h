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

#ifndef DUDA_PACKAGE_SQLITE_H
#define DUDA_PACKAGE_SQLITE_H

#include <sqlite3.h>
#include "duda_api.h"

struct sqlite_cb_data {
    duda_request_t *dr;
    void *data;
};

struct duda_api_sqlite {
    sqlite3 *(*open) (const char *);
    int (*dump)    (sqlite3 *, const char *, sqlite3_stmt **);
    int (*step)    (sqlite3_stmt *);

    /* retrieve fields from row */
    int (*get_int) (sqlite3_stmt *, int);
    double (*get_double) (sqlite3_stmt *, int);
    const unsigned char *(*get_text) (sqlite3_stmt *, int);

    int (*done)  (sqlite3_stmt *);
    int (*exec)  (duda_request_t *, sqlite3 *, const char *,
                  int (*) (void *, int, char **, char **), void *);
    int (*close) (sqlite3 *);
};

typedef struct duda_api_sqlite sqlite_object_t;
typedef sqlite3 sqlite_db_t;
typedef sqlite3_stmt sqlite_handle_t;

sqlite_object_t *sqlite;

int sql_init();

sqlite3 *sql_open(const char *path);
int sql_dump(sqlite3 *db, const char *query, sqlite3_stmt **handle);
int sql_exec(duda_request_t *dr, sqlite3 *db, const char *query,
             int (*callback) (void *, int, char **, char **), void *data);
int sql_step(sqlite3_stmt *handle);
int sql_done(sqlite3_stmt *handle);
int sql_close(sqlite3 *db);

#define SQLITE_FOREACH(handle) while (sqlite->step(handle) == SQLITE_ROW)

/*
 * @METHOD_NAME: sqlite_foreach
 * @METHOD_DESC: This is a helper macro which allow to walk through a result
 * set of a handler.
 * @METHOD_PROTO: sqlite_foreach(sqlite3_stmt *handle) {...}
 * @METHOD_PARAM: handler the result sets handler
 * @METHOD_RETURN: This is not a function who return some value, its a loop-like
 * macro.
 */
#define sqlite_foreach(handle) SQLITE_FOREACH(handle)
#endif
