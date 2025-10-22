/*
 * lwan - simple web server
 * Copyright (c) 2022 L. A. F. Pereira <l@tia.mat.br>
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 */

#include <string.h>
#include <mysql.h>
#include <sqlite3.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>

#include "database.h"
#include "lwan-status.h"

struct db_stmt {
    bool (*bind)(const struct db_stmt *stmt,
                 struct db_row *rows,
                 size_t n_rows);
    bool (*step)(const struct db_stmt *stmt, const char *signature, va_list ap);
    void (*finalize)(struct db_stmt *stmt);
};

struct db {
    void (*disconnect)(struct db *db);
    struct db_stmt *(*prepare)(const struct db *db,
                               const char *sql,
                               const size_t sql_len);
};

/* MySQL */

struct db_mysql {
    struct db base;
    MYSQL *con;
};

struct db_stmt_mysql {
    struct db_stmt base;
    MYSQL_STMT *stmt;
    MYSQL_BIND *param_bind;
    MYSQL_BIND *result_bind;
    bool must_execute_again;
};

static bool db_stmt_bind_mysql(const struct db_stmt *stmt,
                               struct db_row *rows,
                               size_t n_rows)
{
    struct db_stmt_mysql *stmt_mysql = (struct db_stmt_mysql *)stmt;

    stmt_mysql->must_execute_again = true;

    if (!stmt_mysql->param_bind) {
        stmt_mysql->param_bind = calloc(n_rows, sizeof(MYSQL_BIND));
        if (!stmt_mysql->param_bind)
            return false;
    } else {
        mysql_stmt_reset(stmt_mysql->stmt);
    }

    for (size_t row = 0; row < n_rows && rows[row].kind; row++) {
        MYSQL_BIND *param = &stmt_mysql->param_bind[row];

        switch (rows[row].kind) {
        case 's':
            param->buffer_type = MYSQL_TYPE_STRING;
            param->buffer = rows[row].u.s;
            break;
        case 'i':
            param->buffer_type = MYSQL_TYPE_LONG;
            param->buffer = &rows[row].u.i;
            break;
        default:
            return false;
        }

        param->is_null = false;
        param->length = 0;
    }

    return !mysql_stmt_bind_param(stmt_mysql->stmt, stmt_mysql->param_bind);
}

static bool db_stmt_step_mysql(const struct db_stmt *stmt,
                               const char *signature,
                               va_list ap)
{
    struct db_stmt_mysql *stmt_mysql = (struct db_stmt_mysql *)stmt;

    if (stmt_mysql->must_execute_again) {
        stmt_mysql->must_execute_again = false;
        if (mysql_stmt_execute(stmt_mysql->stmt))
            return false;
    }

    if (!stmt_mysql->result_bind) {
        if (*signature == '\0')
            return false;

        stmt_mysql->result_bind =
            calloc(strlen(signature), sizeof(*stmt_mysql->result_bind));
        if (!stmt_mysql->result_bind)
            return false;

        free(stmt_mysql->param_bind);
        stmt_mysql->param_bind = NULL;

        MYSQL_BIND *result = stmt_mysql->result_bind;
        for (size_t r = 0; signature[r]; r++) {
            switch (signature[r]) {
            case 's':
                result[r].buffer_type = MYSQL_TYPE_STRING;
                result[r].buffer = va_arg(ap, char *);
                result[r].buffer_length = va_arg(ap, size_t);
                break;
            case 'i':
                result[r].buffer_type = MYSQL_TYPE_LONG;
                result[r].buffer = va_arg(ap, long *);
                result[r].buffer_length = 0;
                break;
            default:
                goto out;
            }

            result[r].is_null = false;
        }

        if (mysql_stmt_bind_result(stmt_mysql->stmt, result))
            goto out;
    }

    return mysql_stmt_fetch(stmt_mysql->stmt) == 0;

out:
    free(stmt_mysql->result_bind);
    stmt_mysql->result_bind = NULL;

    return false;
}

static void db_stmt_finalize_mysql(struct db_stmt *stmt)
{
    struct db_stmt_mysql *stmt_mysql = (struct db_stmt_mysql *)stmt;

    mysql_stmt_close(stmt_mysql->stmt);
    free(stmt_mysql->result_bind);
    free(stmt_mysql->param_bind);
    free(stmt_mysql);
}

static struct db_stmt *
db_prepare_mysql(const struct db *db, const char *sql, const size_t sql_len)
{
    const struct db_mysql *db_mysql = (const struct db_mysql *)db;
    struct db_stmt_mysql *stmt_mysql = malloc(sizeof(*stmt_mysql));

    if (!stmt_mysql)
        return NULL;

    stmt_mysql->stmt = mysql_stmt_init(db_mysql->con);
    if (!stmt_mysql->stmt)
        goto out_free_stmt;

    if (mysql_stmt_prepare(stmt_mysql->stmt, sql, sql_len))
        goto out_close_stmt;

    stmt_mysql->base.bind = db_stmt_bind_mysql;
    stmt_mysql->base.step = db_stmt_step_mysql;
    stmt_mysql->base.finalize = db_stmt_finalize_mysql;
    stmt_mysql->result_bind = NULL;
    stmt_mysql->param_bind = NULL;
    stmt_mysql->must_execute_again = true;

    return (struct db_stmt *)stmt_mysql;

out_close_stmt:
    mysql_stmt_close(stmt_mysql->stmt);
out_free_stmt:
    free(stmt_mysql);

    return NULL;
}

static void db_disconnect_mysql(struct db *db)
{
    struct db_mysql *db_mysql = (struct db_mysql *)db;

    mysql_close(db_mysql->con);
    free(db);
}

struct db *db_connect_mysql(const char *host,
                            const char *user,
                            const char *pass,
                            const char *database)
{
    struct db_mysql *db_mysql = malloc(sizeof(*db_mysql));

    if (!db_mysql)
        return NULL;

    db_mysql->con = mysql_init(NULL);
    if (!db_mysql->con) {
        free(db_mysql);
        return NULL;
    }

    if (!mysql_real_connect(db_mysql->con, host, user, pass, database, 0, NULL,
                            0))
        goto error;

    if (mysql_set_character_set(db_mysql->con, "utf8"))
        goto error;

    db_mysql->base.disconnect = db_disconnect_mysql;
    db_mysql->base.prepare = db_prepare_mysql;

    return (struct db *)db_mysql;

error:
    mysql_close(db_mysql->con);
    free(db_mysql);
    return NULL;
}

/* SQLite */

struct db_sqlite {
    struct db base;
    sqlite3 *sqlite;
};

struct db_stmt_sqlite {
    struct db_stmt base;
    sqlite3_stmt *sqlite;
};

static bool db_stmt_bind_sqlite(const struct db_stmt *stmt,
                                struct db_row *rows,
                                size_t n_rows)
{
    const struct db_stmt_sqlite *stmt_sqlite =
        (const struct db_stmt_sqlite *)stmt;

    sqlite3_reset(stmt_sqlite->sqlite);
    sqlite3_clear_bindings(stmt_sqlite->sqlite);

    for (size_t row = 1; row <= n_rows; row++) {
        const struct db_row *r = &rows[row - 1];
        int ret;

        switch (r->kind) {
        case 's':
            ret = sqlite3_bind_text(stmt_sqlite->sqlite, (int)row, r->u.s, -1,
                                    NULL);
            break;
        case 'i':
            ret = sqlite3_bind_int(stmt_sqlite->sqlite, (int)row, r->u.i);
            break;
        default:
            return false;
        }

        if (ret != SQLITE_OK)
            return false;
    }

    return true;
}

static bool db_stmt_step_sqlite(const struct db_stmt *stmt,
                                const char *signature,
                                va_list ap)
{
    const struct db_stmt_sqlite *stmt_sqlite =
        (const struct db_stmt_sqlite *)stmt;

    if (sqlite3_step(stmt_sqlite->sqlite) != SQLITE_ROW)
        return false;

    for (int r = 0; signature[r]; r++) {
        switch (signature[r]) {
        case 'i':
            *va_arg(ap, long *) = sqlite3_column_int(stmt_sqlite->sqlite, r);
            break;
        case 's': {
            char *out = va_arg(ap, char *);
            size_t bufsize = va_arg(ap, size_t);

            strncpy(out,
                    (const char *)sqlite3_column_text(stmt_sqlite->sqlite, r),
                    bufsize);

            break;
        }
        default:
            return false;
        }
    }

    return true;
}

static void db_stmt_finalize_sqlite(struct db_stmt *stmt)
{
    struct db_stmt_sqlite *stmt_sqlite = (struct db_stmt_sqlite *)stmt;

    sqlite3_finalize(stmt_sqlite->sqlite);
    free(stmt_sqlite);
}

static struct db_stmt *
db_prepare_sqlite(const struct db *db, const char *sql, const size_t sql_len)
{
    const struct db_sqlite *db_sqlite = (const struct db_sqlite *)db;
    struct db_stmt_sqlite *stmt_sqlite = malloc(sizeof(*stmt_sqlite));

    if (!stmt_sqlite)
        return NULL;

    int ret = sqlite3_prepare_v2(db_sqlite->sqlite, sql, (int)sql_len,
                                 &stmt_sqlite->sqlite, NULL);
    if (ret != SQLITE_OK) {
        free(stmt_sqlite);
        return NULL;
    }

    stmt_sqlite->base.bind = db_stmt_bind_sqlite;
    stmt_sqlite->base.step = db_stmt_step_sqlite;
    stmt_sqlite->base.finalize = db_stmt_finalize_sqlite;

    return (struct db_stmt *)stmt_sqlite;
}

static void db_disconnect_sqlite(struct db *db)
{
    struct db_sqlite *db_sqlite = (struct db_sqlite *)db;

    sqlite3_close(db_sqlite->sqlite);
    free(db);
}

struct db *
db_connect_sqlite(const char *path, bool read_only, const char *pragmas[])
{
    struct db_sqlite *db_sqlite = malloc(sizeof(*db_sqlite));

    if (!db_sqlite)
        return NULL;

    int flags = read_only ? SQLITE_OPEN_READONLY : SQLITE_OPEN_READWRITE;
    int ret = sqlite3_open_v2(path, &db_sqlite->sqlite, flags, NULL);
    if (ret != SQLITE_OK) {
        free(db_sqlite);
        return NULL;
    }

    if (pragmas) {
        for (size_t p = 0; pragmas[p]; p++)
            sqlite3_exec(db_sqlite->sqlite, pragmas[p], NULL, NULL, NULL);
    }

    db_sqlite->base.disconnect = db_disconnect_sqlite;
    db_sqlite->base.prepare = db_prepare_sqlite;

    return (struct db *)db_sqlite;
}

/* Generic */

inline bool
db_stmt_bind(const struct db_stmt *stmt, struct db_row *rows, size_t n_rows)
{
    return stmt->bind(stmt, rows, n_rows);
}

inline bool
db_stmt_step(const struct db_stmt *stmt, const char *signature, ...)
{
    va_list ap;
    bool ret;

    va_start(ap, signature);
    ret = stmt->step(stmt, signature, ap);
    va_end(ap);

    return ret;
}

inline void db_stmt_finalize(struct db_stmt *stmt) { stmt->finalize(stmt); }

inline void db_disconnect(struct db *db) { db->disconnect(db); }

inline struct db_stmt *
db_prepare_stmt(const struct db *db, const char *sql, const size_t sql_len)
{
    return db->prepare(db, sql, sql_len);
}
