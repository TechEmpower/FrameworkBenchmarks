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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 */

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "lwan.h"
#include "lwan-config.h"
#include "lwan-template.h"

#include "database.h"
#include "json.h"

enum db_connect_type { DB_CONN_MYSQL, DB_CONN_SQLITE };

struct db_connection_params {
    enum db_connect_type type;
    union {
        struct {
            const char *user;
            const char *password;
            const char *database;
            const char *hostname;
        } mysql;
        struct {
            const char *path;
            const char **pragmas;
        } sqlite;
    };
};

static struct db_connection_params db_connection_params;

static struct db *get_db(void)
{
    static __thread struct db *database;

    if (!database) {
        switch (db_connection_params.type) {
        case DB_CONN_MYSQL:
            database = db_connect_mysql(db_connection_params.mysql.hostname,
                                        db_connection_params.mysql.user,
                                        db_connection_params.mysql.password,
                                        db_connection_params.mysql.database);
            break;
        case DB_CONN_SQLITE:
            database = db_connect_sqlite(db_connection_params.sqlite.path, true,
                                         db_connection_params.sqlite.pragmas);
            break;
        }
        if (!database)
            lwan_status_critical("Could not connect to the database");
    }

    return database;
}

static const char hello_world[] = "Hello, World!";
static const char random_number_query[] =
    "SELECT randomNumber FROM world WHERE id=?";

struct Fortune {
    struct {
        coro_function_t generator;

        int id;
        char *message;
    } item;
};

DEFINE_ARRAY_TYPE_INLINEFIRST(fortune_array, struct Fortune)

static const char fortunes_template_str[] =
    "<!DOCTYPE html>"
    "<html>"
    "<head><title>Fortunes</title></head>"
    "<body>"
    "<table>"
    "<tr><th>id</th><th>message</th></tr>"
    "{{#item}}"
    "<tr><td>{{item.id}}</td><td>{{item.message}}</td></tr>"
    "{{/item}}"
    "</table>"
    "</body>"
    "</html>";

static int fortune_list_generator(struct coro *coro, void *data);

#undef TPL_STRUCT
#define TPL_STRUCT struct Fortune
static const struct lwan_var_descriptor fortune_item_desc[] = {
    TPL_VAR_INT(item.id),
    TPL_VAR_STR_ESCAPE(item.message),
    TPL_VAR_SENTINEL,
};

static const struct lwan_var_descriptor fortune_desc[] = {
    TPL_VAR_SEQUENCE(item, fortune_list_generator, fortune_item_desc),
    TPL_VAR_SENTINEL,
};

static struct lwan_tpl *fortune_tpl;

struct hello_world_json {
    const char *message;
};
static const struct json_obj_descr hello_world_json_desc[] = {
    JSON_OBJ_DESCR_PRIM(struct hello_world_json, message, JSON_TOK_STRING),
};

struct db_json {
    int id;
    int randomNumber;
};
static const struct json_obj_descr db_json_desc[] = {
    JSON_OBJ_DESCR_PRIM(struct db_json, id, JSON_TOK_NUMBER),
    JSON_OBJ_DESCR_PRIM(struct db_json, randomNumber, JSON_TOK_NUMBER),
};

struct queries_json {
    struct db_json queries[500];
    size_t queries_len;
};
static const struct json_obj_descr queries_json_desc[] = {
    JSON_OBJ_DESCR_OBJ_ARRAY(struct queries_json,
                             queries,
                             500,
                             queries_len,
                             db_json_desc,
                             N_ELEMENTS(db_json_desc)),
};

static int append_to_strbuf(const char *bytes, size_t len, void *data)
{
    struct lwan_strbuf *strbuf = data;

    return lwan_strbuf_append_str(strbuf, bytes, len) ? 0 : -EINVAL;
}

static enum lwan_http_status json_response(struct lwan_response *response,
                                           const struct json_obj_descr *descr,
                                           size_t descr_len,
                                           const void *data)
{
    lwan_strbuf_grow_to(response->buffer, 128);

    if (json_obj_encode(descr, descr_len, data, append_to_strbuf,
                        response->buffer) < 0)
        return HTTP_INTERNAL_ERROR;

    response->mime_type = "application/json";
    return HTTP_OK;
}

LWAN_HANDLER(json)
{
    struct hello_world_json j = {.message = hello_world};

    return json_response(response, hello_world_json_desc,
                         N_ELEMENTS(hello_world_json_desc), &j);
}

static bool db_query(struct db_stmt *stmt,
                     struct db_row rows[],
                     struct db_row results[],
                     struct db_json *out)
{
    int id = rand() % 10000;

    rows[0].u.i = id;

    if (UNLIKELY(!db_stmt_bind(stmt, rows, 1)))
        return false;

    if (UNLIKELY(!db_stmt_step(stmt, results)))
        return false;

    out->id = id;
    out->randomNumber = results[0].u.i;

    return true;
}

LWAN_HANDLER(db)
{
    struct db_row rows[1] = {{.kind = 'i'}};
    struct db_row results[] = {{.kind = 'i'}, {.kind = '\0'}};
    struct db_stmt *stmt = db_prepare_stmt(get_db(), random_number_query,
                                           sizeof(random_number_query) - 1);
    struct db_json db_json;

    if (UNLIKELY(!stmt)) {
        lwan_status_debug("preparing stmt failed");
        return HTTP_INTERNAL_ERROR;
    }

    bool queried = db_query(stmt, rows, results, &db_json);

    db_stmt_finalize(stmt);

    if (!queried)
        return HTTP_INTERNAL_ERROR;

    return json_response(response, db_json_desc, N_ELEMENTS(db_json_desc),
                         &db_json);
}

LWAN_HANDLER(queries)
{
    enum lwan_http_status ret = HTTP_INTERNAL_ERROR;
    const char *queries_str = lwan_request_get_query_param(request, "queries");
    long queries;

    if (LIKELY(queries_str)) {
        queries = parse_long(queries_str, -1);
        if (UNLIKELY(queries <= 0))
            queries = 1;
        else if (UNLIKELY(queries > 500))
            queries = 500;
    } else {
        queries = 1;
    }

    struct db_stmt *stmt = db_prepare_stmt(get_db(), random_number_query,
                                           sizeof(random_number_query) - 1);
    if (UNLIKELY(!stmt))
        return HTTP_INTERNAL_ERROR;

    struct queries_json qj = {.queries_len = (size_t)queries};
    struct db_row rows[1] = {{.kind = 'i'}};
    struct db_row results[] = {{.kind = 'i'}, {.kind = '\0'}};
    for (long i = 0; i < queries; i++) {
        if (!db_query(stmt, rows, results, &qj.queries[i]))
            goto out;
    }

    ret = json_response(response, queries_json_desc,
                        N_ELEMENTS(queries_json_desc), &qj);

out:
    db_stmt_finalize(stmt);

    return ret;
}

LWAN_HANDLER(plaintext)
{
    lwan_strbuf_set_static(response->buffer, hello_world,
                           sizeof(hello_world) - 1);

    response->mime_type = "text/plain";
    return HTTP_OK;
}

static int fortune_compare(const void *a, const void *b)
{
    const struct Fortune *fortune_a = (const struct Fortune *)a;
    const struct Fortune *fortune_b = (const struct Fortune *)b;
    size_t a_len = strlen(fortune_a->item.message);
    size_t b_len = strlen(fortune_b->item.message);

    if (!a_len || !b_len)
        return a_len > b_len;

    size_t min_len = a_len < b_len ? a_len : b_len;

    int cmp = memcmp(fortune_a->item.message, fortune_b->item.message, min_len);
    return cmp == 0 ? -(int)(ssize_t)min_len : cmp;
}

static bool append_fortune(struct coro *coro,
                           struct fortune_array *fortunes,
                           int id,
                           const char *message)
{
    struct Fortune *fortune;
    char *message_copy;

    message_copy = coro_strdup(coro, message);
    if (UNLIKELY(!message_copy))
        return false;

    fortune = fortune_array_append(fortunes);
    if (UNLIKELY(!fortune))
        return false;

    fortune->item.id = id;
    fortune->item.message = message_copy;

    return true;
}

static int fortune_list_generator(struct coro *coro, void *data)
{
    static const char fortune_query[] = "SELECT * FROM Fortune";
    char fortune_buffer[256];
    struct Fortune *fortune = data;
    struct fortune_array fortunes;
    struct db_stmt *stmt;

    stmt = db_prepare_stmt(get_db(), fortune_query, sizeof(fortune_query) - 1);
    if (UNLIKELY(!stmt))
        return 0;

    fortune_array_init(&fortunes);

    struct db_row results[] = {{.kind = 'i'},
                               {.kind = 's',
                                .u.s = fortune_buffer,
                                .buffer_length = sizeof(fortune_buffer)},
                               {.kind = '\0'}};
    while (db_stmt_step(stmt, results)) {
        if (!append_fortune(coro, &fortunes, results[0].u.i, results[1].u.s))
            goto out;
    }

    if (!append_fortune(coro, &fortunes, 0,
                        "Additional fortune added at request time."))
        goto out;

    fortune_array_sort(&fortunes, fortune_compare);

    struct Fortune *iter;
    LWAN_ARRAY_FOREACH (&fortunes, iter) {
        fortune->item.id = iter->item.id;
        fortune->item.message = iter->item.message;
        coro_yield(coro, 1);
    }

out:
    fortune_array_reset(&fortunes);
    db_stmt_finalize(stmt);
    return 0;
}

LWAN_HANDLER(fortunes)
{
    struct Fortune fortune;

    lwan_strbuf_grow_to(response->buffer, 1500);

    if (UNLIKELY(!lwan_tpl_apply_with_buffer(fortune_tpl, response->buffer,
                                             &fortune)))
        return HTTP_INTERNAL_ERROR;

    response->mime_type = "text/html; charset=UTF-8";
    return HTTP_OK;
}

int main(void)
{
    static const struct lwan_url_map url_map[] = {
        {.prefix = "/json", .handler = LWAN_HANDLER_REF(json)},
        {.prefix = "/db", .handler = LWAN_HANDLER_REF(db)},
        {.prefix = "/queries", .handler = LWAN_HANDLER_REF(queries)},
        {.prefix = "/plaintext", .handler = LWAN_HANDLER_REF(plaintext)},
        {.prefix = "/fortunes", .handler = LWAN_HANDLER_REF(fortunes)},
        {.prefix = NULL},
    };
    struct lwan l;

    lwan_init(&l);

    srand((unsigned int)time(NULL));

    if (getenv("USE_MYSQL")) {
        db_connection_params = (struct db_connection_params) {
            .type = DB_CONN_MYSQL,
            .mysql.user = getenv("MYSQL_USER"),
            .mysql.password = getenv("MYSQL_PASS"),
            .mysql.hostname = getenv("MYSQL_HOST"),
            .mysql.database = getenv("MYSQL_DB")
        };

        if (!db_connection_params.mysql.user)
            lwan_status_critical("No MySQL user provided");
        if (!db_connection_params.mysql.password)
            lwan_status_critical("No MySQL password provided");
        if (!db_connection_params.mysql.hostname)
            lwan_status_critical("No MySQL hostname provided");
        if (!db_connection_params.mysql.database)
            lwan_status_critical("No MySQL database provided");
    } else {
        static const char *pragmas[] = {"PRAGMA mmap_size=44040192",
                                        "PRAGMA journal_mode=OFF",
                                        "PRAGMA locking_mode=EXCLUSIVE", NULL};
        db_connection_params = (struct db_connection_params) {
            .type = DB_CONN_SQLITE,
            .sqlite.path = "techempower.db",
            .sqlite.pragmas = pragmas,
        };
    }

    fortune_tpl = lwan_tpl_compile_string_full(
        fortunes_template_str, fortune_desc, LWAN_TPL_FLAG_CONST_TEMPLATE);
    if (!fortune_tpl)
        lwan_status_critical("Could not compile fortune templates");

    lwan_set_url_map(&l, url_map);
    lwan_main_loop(&l);

    lwan_tpl_free(fortune_tpl);
    lwan_shutdown(&l);

    return 0;
}
