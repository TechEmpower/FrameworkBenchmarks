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

/*
 * @OBJ_NAME: sqlite
 * @OBJ_MENU: SQLite
 * @OBJ_DESC: The SQLite package expose a set of wrapper methods to use the
 * SQLite database.
 * @PKG_HEADER: #include "packages/sqlite/sqlite.h"
 * @PKG_INIT: duda_load_package(sqlite, "sqlite");
 */

#include "duda_api.h"
#include "duda_package.h"
#include "sqlite.h"

struct duda_api_sqlite *get_sqlite_api()
{
    struct duda_api_sqlite *sqlite;

    /* Alloc object */
    sqlite = malloc(sizeof(struct duda_api_sqlite));

    /* Map API calls */
    sqlite->open       = sql_open;
    sqlite->dump       = sql_dump;
    sqlite->step       = sql_step;
    sqlite->get_int    = sqlite3_column_int;
    sqlite->get_double = sqlite3_column_double;
    sqlite->get_text   = sqlite3_column_text;

    sqlite->done       = sql_done;
    sqlite->exec       = sql_exec;
    sqlite->close      = sql_close;

    return sqlite;
}

duda_package_t *duda_package_main()
{
    duda_package_t *dpkg;

    /* Init SQLite */
    sql_init();

    /* Package object */
    dpkg = malloc(sizeof(duda_package_t));
    dpkg->name    = "sqlite";
    dpkg->version = "0.1";
    dpkg->api     = get_sqlite_api();

    return dpkg;
}
