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

#include <stdio.h>
#include <stdlib.h>
#include <sqlite3.h>

#include "sqlite.h"
#include "duda_api.h"

/* just in case we need some specific setup in the future */
int sql_init()
{
    return 0;
}

/*
 * @METHOD_NAME: open
 * @METHOD_DESC: It open a connection to a SQLite database file. This function
 * must be invoked from duda_main() as the connection is persistent as long as
 * the service runs.
 * @METHOD_PROTO: sqlite3 open(const char *path)
 * @METHOD_PARAM: path the SQLite database file path
 * @METHOD_RETURN: On success it returns the connection handler. On
 * error it perform an explicit exit.
 */
sqlite3 *sql_open(const char *path)
{
    int ret;
    sqlite3 *db;

    ret = sqlite3_open(path, &db);
    if (ret != SQLITE_OK) {
        printf("SQLITE: Can't open database: %s\n", path);
        sqlite3_close(db);
        exit(EXIT_FAILURE);
    }

    /* Try to use asynchronous mode */
    sql_exec(NULL, db, "PRAGMA synchronous = OFF;", NULL, NULL);
    return db;
}

/*
 * @METHOD_NAME: dump
 * @METHOD_DESC: It prepares an execution context to be used later with the
 * step() method to walk on each returned row.
 * @METHOD_PROTO: int dump(sqlite3 *db, const char *query, sqlite3_stmt **handle)
 * @METHOD_PARAM: db the database connection handler
 * @METHOD_PARAM: query the SQL query
 * @METHOD_PARAM: handle the handler for the query results
 * @METHOD_RETURN: On success it returns zero, otherwise it returns -1 on error
 */
int sql_dump(sqlite3 *db, const char *query, sqlite3_stmt **handle)
{
    int ret;

    ret = sqlite3_prepare(db, query, -1, handle, NULL);
    if (ret != SQLITE_OK || !handle) {
        printf("Error: sql_dump()=%d %s\n", ret, sqlite3_errmsg(db));
        return -1;
    }

    return ret;
}

/*
 * @METHOD_NAME: exec
 * @METHOD_DESC: Executes a SQL query and set a callback function to be invoked
 * per row retrieved.
 * @METHOD_PROTO: int exec(duda_request_t *dr, sqlite3 *db, const char *query,
 *          int (*callback) (void *, int, char **, char **), void *data)
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: db the database connection handler
 * @METHOD_PARAM: query the SQL query
 * @METHOD_PARAM: callback the callback function to be called when a row is fetched
 * @METHOD_PARAM: data user data to be passed to the callback function. Once
 * the callback is invoked, the data that you refered is packaged in a new
 * structure of type 'struct sqlite_cb_data' that contains reference fields named
 * 'dr' and 'data' respectively.
 * @METHOD_RETURN: On success it returns zero, otherwise it returns -1 on error
 */
int sql_exec(duda_request_t *dr, sqlite3 *db, const char *query,
             int (*callback) (void *, int, char **, char **), void *data)
{
    int ret;
    char *err;
    struct sqlite_cb_data cb_data;

    cb_data.dr   = dr;
    cb_data.data = data;

    ret = sqlite3_exec(db, query, callback, (void *) &cb_data, &err);
    if (ret != SQLITE_OK) {
        printf("SQLITE: SQL error: %s\n", err);
        return -1;
    }

    return 0;
}

/*
 * @METHOD_NAME: step
 * @METHOD_DESC: It evaluate a result set from a handler. This function is
 * exported in case the developer wanted to perform a manual evaluation.
 * In order to walk through a specific results set you can use the foreach()
 * method.
 * @METHOD_PROTO: int step(sqlite3_stmt *handle)
 * @METHOD_PARAM: handle the query handler, created previously by dump()
 * @METHOD_RETURN: If no more rows exists in the handle it returns 0, otherwise
 * it returns SQLITE_ROW. On error it returns -1.
 */
int sql_step(sqlite3_stmt *handle)
{
    int ret;

    ret = sqlite3_step(handle);
    if (ret == SQLITE_OK || ret == SQLITE_DONE) {
        return 0;
    }
    else if (ret == SQLITE_ROW) {
        return SQLITE_ROW;
    }

    return -1;
}

/*
 * @METHOD_NAME: done
 * @METHOD_DESC: It finalize a prepared query statement made with dump(). Once
 * you finish to use the prepared statement is mandatory to invoke done()
 * @METHOD_PROTO: int done(sqlite3_stmt *handle)
 * @METHOD_PARAM: handle the query handler, created previously by dump()
 * @METHOD_RETURN: On success it returns zero, otherwise it returns -1 on error
 */
int sql_done(sqlite3_stmt *handle)
{
    return sqlite3_finalize(handle);
}

/*
 * @METHOD_NAME: close
 * @METHOD_DESC: It closes a database connection instance
 * @METHOD_PROTO: int close(sqlite3 *db)
 * @METHOD_PARAM: db the database connection
 * @METHOD_RETURN: On success it returns zero, otherwise it returns -1 on error
 */
int sql_close(sqlite3 *db)
{
    return sqlite3_close(db);
}
