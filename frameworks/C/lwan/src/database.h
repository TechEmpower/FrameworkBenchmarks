/*
 * lwan - simple web server
 * Copyright (c) 2014 Leandro A. F. Pereira <leandro@hardinfo.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

#pragma once

#include <stdbool.h>

struct db;
struct db_stmt;

struct db_row {
    union {
        char *s;
        int i;
    } u;
    char kind; /* 's' = string, 'i' = 'int', '\0' = last */
    size_t buffer_length;
};

bool db_stmt_bind(const struct db_stmt *stmt, struct db_row *rows, size_t n_rows);
bool db_stmt_step(const struct db_stmt *stmt, struct db_row *row);
void db_stmt_finalize(struct db_stmt *stmt);
void db_disconnect(struct db *db);
struct db_stmt *db_prepare_stmt(const struct db *db, const char *sql,
    const size_t sql_len);

struct db *db_connect_sqlite(const char *path, bool read_only, const char *pragmas[]);
struct db *db_connect_mysql(const char *host, const char *user, const char *pass, const char *database);

