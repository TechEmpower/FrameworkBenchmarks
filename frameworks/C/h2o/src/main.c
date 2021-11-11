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

#include <errno.h>
#include <h2o.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <h2o/serverutil.h>
#include <sys/resource.h>
#include <sys/signalfd.h>
#include <sys/time.h>

#include "database.h"
#include "error.h"
#include "event_loop.h"
#include "global_data.h"
#include "list.h"
#include "request_handler.h"
#include "thread.h"
#include "tls.h"
#include "utility.h"

#define USAGE_MESSAGE \
	"Usage:\n%s [-a <max connections accepted simultaneously>] [-b <bind address>] " \
	"[-c <certificate file>] [-d <database connection string>] [-f template file path] " \
	"[-j <max reused JSON generators>] [-k <private key file>] [-l <log path>] " \
	"[-m <max database connections per thread>] [-p <port>] " \
	"[-q <max enqueued database queries per thread>] [-r <root directory>] " \
	"[-s <HTTPS port>] [-t <thread number>]\n"

typedef struct {
	list_t l;
	void *arg;
	void (*task)(thread_context_t *, void *);
} task_t;

static void free_global_data(global_data_t *global_data);
static int initialize_global_data(const config_t *config, global_data_t *global_data);
static int parse_options(int argc, char *argv[], config_t *config);
static void run_postinitialization_tasks(list_t **tasks, thread_context_t *ctx);
static void set_default_options(config_t *config);
static void setup_process(void);

static void free_global_data(global_data_t *global_data)
{
	if (global_data->global_thread_data) {
		for (size_t i = global_data->global_thread_data->config->thread_num - 1; i > 0; i--)
			CHECK_ERROR(pthread_join, global_data->global_thread_data[i].thread, NULL);

		free(global_data->global_thread_data);
	}

	if (global_data->file_logger)
		global_data->file_logger->dispose(global_data->file_logger);

	cleanup_request_handlers(global_data);
	remove_prepared_statements(global_data->prepared_statements);
	h2o_config_dispose(&global_data->h2o_config);

	if (global_data->ssl_ctx)
		cleanup_openssl(global_data);
}

static int initialize_global_data(const config_t *config, global_data_t *global_data)
{
	sigset_t signals;

	memset(global_data, 0, sizeof(*global_data));
	global_data->memory_alignment = get_maximum_cache_line_size();
	CHECK_ERRNO(sigemptyset, &signals);
#ifdef NDEBUG
	CHECK_ERRNO(sigaddset, &signals, SIGINT);
#endif // NDEBUG
	CHECK_ERRNO(sigaddset, &signals, SIGTERM);
	CHECK_ERRNO_RETURN(global_data->signal_fd, signalfd, -1, &signals, SFD_NONBLOCK | SFD_CLOEXEC);
	h2o_config_init(&global_data->h2o_config);

	if (config->cert && config->key)
		initialize_openssl(config, global_data);

	const h2o_iovec_t host = h2o_iovec_init(H2O_STRLIT("default"));
	h2o_hostconf_t * const hostconf = h2o_config_register_host(&global_data->h2o_config,
	                                                           host,
	                                                           config->port);
	h2o_access_log_filehandle_t *log_handle = NULL;

	if (config->log) {
		log_handle = h2o_access_log_open_handle(config->log, NULL, H2O_LOGCONF_ESCAPE_APACHE);

		if (!log_handle)
			goto error;
	}

	initialize_request_handlers(config, global_data, hostconf, log_handle);

	// Must be registered after the rest of the request handlers.
	if (config->root) {
		h2o_pathconf_t * const pathconf = h2o_config_register_path(hostconf, "/", 0);
		h2o_file_register(pathconf, config->root, NULL, NULL, 0);

		if (log_handle)
			global_data->file_logger = h2o_access_log_register(pathconf, log_handle);
	}

	global_data->global_thread_data = initialize_global_thread_data(config, global_data);

	if (global_data->global_thread_data) {
		printf("Number of processors: %zu\nMaximum cache line size: %zu\n",
		       h2o_numproc(),
		       global_data->memory_alignment);
		return 0;
	}

error:
	close(global_data->signal_fd);
	free_global_data(global_data);
	return 1;
}

static int parse_options(int argc, char *argv[], config_t *config)
{
	memset(config, 0, sizeof(*config));
	// Need to set the default value here because 0 is a valid input value.
	config->max_json_generator = 32;
	opterr = 0;

	while (1) {
		const int opt = getopt(argc, argv, "?a:b:c:d:f:j:k:l:m:p:q:r:s:t:");

		if (opt == -1)
			break;

		switch (opt) {

#define PARSE_NUMBER(out) \
	do { \
		errno = 0; \
		\
		const long long n = strtoll(optarg, NULL, 10); \
		\
		if (errno) { \
			print_library_error(__FILE__, __LINE__, "strtoll", errno); \
			return 1; \
		} \
		\
		(out) = n; \
	} while(0)

			case 'a':
				PARSE_NUMBER(config->max_accept);
				break;
			case 'b':
				config->bind_address = optarg;
				break;
			case 'c':
				config->cert = optarg;
				break;
			case 'd':
				config->db_host = optarg;
				break;
			case 'f':
				config->template_path = optarg;
				break;
			case 'j':
				PARSE_NUMBER(config->max_json_generator);
				break;
			case 'k':
				config->key = optarg;
				break;
			case 'l':
				config->log = optarg;
				break;
			case 'm':
				PARSE_NUMBER(config->max_db_conn_num);
				break;
			case 'p':
				PARSE_NUMBER(config->port);
				break;
			case 'q':
				PARSE_NUMBER(config->max_query_num);
				break;
			case 'r':
				config->root = optarg;
				break;
			case 's':
				PARSE_NUMBER(config->https_port);
				break;
			case 't':
				PARSE_NUMBER(config->thread_num);
				break;
			default:
				fprintf(stderr, USAGE_MESSAGE, *argv);
				return 1;

#undef PARSE_NUMBER
		}
	}

	set_default_options(config);
	return 0;
}

static void run_postinitialization_tasks(list_t **tasks, thread_context_t *ctx)
{
	if (*tasks)
		do {
			task_t * const t = H2O_STRUCT_FROM_MEMBER(task_t, l, *tasks);

			*tasks = (*tasks)->next;
			t->task(ctx, t->arg);
			free(t);
		} while (*tasks);
}

static void set_default_options(config_t *config)
{
	if (!config->max_accept)
		config->max_accept = 10;

	if (!config->max_db_conn_num)
		config->max_db_conn_num = 10;

	if (!config->max_query_num)
		config->max_query_num = 10000;

	if (!config->port)
		config->port = 8080;

	if (!config->thread_num)
		config->thread_num = h2o_numproc();

	if (!config->https_port)
		config->https_port = 4443;
}

static void setup_process(void)
{
	sigset_t signals;

	CHECK_ERRNO(sigfillset, &signals);
	CHECK_ERRNO(sigdelset, &signals, SIGBUS);
	CHECK_ERRNO(sigdelset, &signals, SIGFPE);
	CHECK_ERRNO(sigdelset, &signals, SIGILL);
	CHECK_ERRNO(sigdelset, &signals, SIGSEGV);
#ifndef NDEBUG
	CHECK_ERRNO(sigdelset, &signals, SIGINT);
#endif // NDEBUG
	CHECK_ERROR(pthread_sigmask, SIG_BLOCK, &signals, NULL);

	struct rlimit rlim = {.rlim_cur = 0};

	CHECK_ERRNO(getrlimit, RLIMIT_NOFILE, &rlim);
	rlim.rlim_cur = rlim.rlim_max;
	CHECK_ERRNO(setrlimit, RLIMIT_NOFILE, &rlim);
}

void add_postinitialization_task(void (*task)(struct thread_context_t *, void *),
                                 void *arg,
                                 list_t **postinitialization_tasks)
{
	task_t * const t = h2o_mem_alloc(sizeof(*t));

	memset(t, 0, sizeof(*t));
	t->l.next = *postinitialization_tasks;
	t->arg = arg;
	t->task = task;
	*postinitialization_tasks = &t->l;
}

int main(int argc, char *argv[])
{
	config_t config;
	int rc = EXIT_FAILURE;

	if (!parse_options(argc, argv, &config)) {
		global_data_t global_data;

		if (!initialize_global_data(&config, &global_data)) {
			thread_context_t ctx;

			setup_process();
			start_threads(global_data.global_thread_data);
			initialize_thread_context(global_data.global_thread_data, true, &ctx);
			run_postinitialization_tasks(&global_data.postinitialization_tasks, &ctx);
			event_loop(&ctx);
			// Even though this is global data, we need to close
			// it before the associated event loop is cleaned up.
			h2o_socket_close(global_data.signals);
			free_thread_context(&ctx);
			free_global_data(&global_data);
			rc = EXIT_SUCCESS;
		}
	}

	return rc;
}
