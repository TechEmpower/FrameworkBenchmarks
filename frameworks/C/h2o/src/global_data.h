/*
 Copyright (c) 2019 Anton Valentinov Kirilov

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

#ifndef GLOBAL_DATA_H_

#define GLOBAL_DATA_H_

#include <h2o.h>
#include <stddef.h>
#include <stdint.h>
#include <openssl/ssl.h>

#include "list.h"
#include "handlers/request_handler_data.h"

struct global_thread_data_t;
struct thread_context_t;

typedef struct {
	const char *bind_address;
	const char *cert;
	const char *db_host;
	const char *key;
	const char *log;
	const char *root;
	const char *template_path;
	size_t max_accept;
	size_t max_db_conn_num;
	size_t max_json_generator;
	size_t max_query_num;
	size_t thread_num;
	uint16_t https_port;
	uint16_t port;
} config_t;

typedef struct {
	h2o_logger_t *file_logger;
	struct global_thread_data_t *global_thread_data;
	list_t *postinitialization_tasks;
	list_t *prepared_statements;
	h2o_socket_t *signals;
	SSL_CTX *ssl_ctx;
	size_t memory_alignment;
	int signal_fd;
	h2o_globalconf_t h2o_config;
	request_handler_data_t request_handler_data;
} global_data_t;

void add_postinitialization_task(void (*task)(struct thread_context_t *, void *),
                                 void *arg,
                                 list_t **postinitialization_tasks);

#endif // GLOBAL_DATA_H_
