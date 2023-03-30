/*
 Copyright (c) 2016 Anton Valentinov Kirilov

 Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 associated documentation files (the "Software"), to deal in the Software without restriction,
 including without limitation the rights to use, copy, modify, merge, publish, distribute,
 sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all copies or
 substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#ifndef DATABASE_H_

#define DATABASE_H_

#include <h2o.h>
#include <stdint.h>
#include <postgresql/libpq-fe.h>

#include "list.h"

#define DB_ERROR "database error\n"
#define DB_REQ_ERROR "too many concurrent database requests\n"
#define DB_TIMEOUT_ERROR "database timeout\n"
#define IS_PREPARED 1

typedef enum {
	SUCCESS,
	DONE,
} result_return_t;

struct config_t;
struct db_query_param_t;

typedef result_return_t (*on_result_t)(struct db_query_param_t *, PGresult *);

typedef struct db_query_param_t {
	list_t l;
	void (*on_error)(struct db_query_param_t *, const char *);
	on_result_t on_result;
	void (*on_timeout)(struct db_query_param_t *);
	const char *command;
	const char * const *paramValues;
	const int *paramLengths;
	const int *paramFormats;
	const Oid *paramTypes;
	size_t nParams;
	uint_fast32_t flags;
	int resultFormat;
} db_query_param_t;

typedef struct {
	const struct config_t *config;
	list_t *conn;
	const char *conninfo;
	h2o_loop_t *loop;
	const list_t *prepared_statements;
	// We use a FIFO queue instead of a simpler stack, otherwise the earlier queries may wait
	// an unbounded amount of time to be executed.
	queue_t queries;
	size_t conn_num;
	size_t query_num;
} db_conn_pool_t;

void add_prepared_statement(const char *name, const char *query, list_t **prepared_statements);
int execute_database_query(db_conn_pool_t *pool, db_query_param_t *param);
void free_database_connection_pool(db_conn_pool_t *pool);
void initialize_database_connection_pool(const char *conninfo,
                                         const struct config_t *config,
                                         const list_t *prepared_statements,
                                         h2o_loop_t *loop,
                                         db_conn_pool_t *pool);
void remove_prepared_statements(list_t *prepared_statements);

#endif // DATABASE_H_
