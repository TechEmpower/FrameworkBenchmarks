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

#include "global_data.h"
#include "list.h"

#define DB_ERROR "database error\n"
#define DB_REQ_ERROR "too many concurrent database requests\n"
#define DB_TIMEOUT_ERROR "database timeout\n"
#define IS_PREPARED 1
#define IS_SINGLE_ROW 2

typedef enum {
	SUCCESS,
	DONE,
	WANT_WRITE
} result_return_t;

typedef struct thread_context_t thread_context_t;

typedef struct db_query_param_t db_query_param_t;

typedef result_return_t (*on_result_t)(db_query_param_t *, PGresult *);

struct db_query_param_t {
	list_t l;
	void (*on_error)(db_query_param_t *, const char *);
	on_result_t on_result;
	void (*on_timeout)(db_query_param_t *);
	int (*on_write_ready)(db_query_param_t *, PGconn *);
	const char *command;
	const char * const *paramValues;
	const int *paramLengths;
	const int *paramFormats;
	size_t nParams;
	uint_fast32_t flags;
	int resultFormat;
};

typedef struct {
	list_t *db_conn;
	// We use a FIFO queue instead of a simpler stack, otherwise the earlier queries may wait
	// an unbounded amount of time to be executed.
	queue_t queries;
	size_t db_conn_num;
	size_t free_db_conn_num;
	size_t query_num;
	h2o_timeout_t h2o_timeout;
} db_state_t;

void add_prepared_statement(const char *name, const char *query, list_t **prepared_statements);
int execute_query(thread_context_t *ctx, db_query_param_t *param);
void free_database_state(h2o_loop_t *loop, db_state_t *db_state);
void initialize_database_state(h2o_loop_t *loop, db_state_t *db_state);
void remove_prepared_statements(list_t *prepared_statements);

#endif // DATABASE_H_
