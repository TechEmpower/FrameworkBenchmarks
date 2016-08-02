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

#include <assert.h>
#include <errno.h>
#include <h2o.h>
#include <mustache.h>
#include <netdb.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <h2o/serverutil.h>
#include <netinet/tcp.h>
#include <sys/resource.h>
#include <sys/signalfd.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>

#include "error.h"
#include "event_loop.h"
#include "request_handler.h"
#include "template.h"
#include "thread.h"
#include "tls.h"
#include "utility.h"

#define DEFAULT_TCP_FASTOPEN_QUEUE_LEN 4096
#define USAGE_MESSAGE \
	"Usage:\n%s [-a <max connections accepted simultaneously>] [-b <bind address>] " \
	"[-c <certificate file>] [-d <database connection string>] [-f fortunes template file path] " \
	"[-k <private key file>] [-l <log path>] " \
	"[-m <max database connections per thread>] [-p <port>] " \
	"[-q <max enqueued database queries per thread>] [-r <root directory>] " \
	"[-t <thread number>]\n"

static void free_global_data(global_data_t *global_data);
static int get_listener_socket(const config_t *config);
static size_t get_maximum_cache_line_size(void);
static int initialize_global_data(const config_t *config, global_data_t *global_data);
static int parse_options(int argc, char *argv[], config_t *config);
static void set_default_options(config_t *config);
static void setup_process(void);

static void free_global_data(global_data_t *global_data)
{
	if (global_data->ctx)
		free_thread_contexts(global_data);

	if (global_data->file_logger)
		global_data->file_logger->dispose(global_data->file_logger);

	if (global_data->fortunes_template) {
		mustache_api_t api = {.freedata = NULL};

		mustache_free(&api, global_data->fortunes_template);
	}

	h2o_config_dispose(&global_data->h2o_config);

	if (global_data->ssl_ctx)
		cleanup_openssl(global_data);
}

static int get_listener_socket(const config_t *config)
{
	int ret = -1;
	char port[16];

	if ((size_t) snprintf(port, sizeof(port), "%u", (unsigned) config->port) >= sizeof(port)) {
		print_error(__FILE__, __LINE__, "snprintf", "Truncated output.");
		return ret;
	}

	struct addrinfo *res = NULL;
	struct addrinfo hints = {.ai_socktype = SOCK_STREAM, .ai_flags = AI_PASSIVE};
	const int error_code = getaddrinfo(config->bind_address, port, &hints, &res);

	if (error_code) {
		print_error(__FILE__, __LINE__, "getaddrinfo", gai_strerror(error_code));
		return ret;
	}

	struct addrinfo *iter = res;

	for (; iter; iter = iter->ai_next) {
		const int s = socket(iter->ai_family,
		                     iter->ai_socktype | SOCK_NONBLOCK | SOCK_CLOEXEC,
		                     iter->ai_protocol);

		if (s == -1) {
			LIBRARY_ERROR("socket");
			continue;
		}

#define LOCAL_CHECK_ERRNO(function, ...) \
	do { \
		const int error_code = (function)(__VA_ARGS__); \
		\
		if (error_code) { \
			print_library_error(__FILE__, __LINE__, #function, errno); \
			goto error; \
		} \
	} while(0)

		int option = 1;

		LOCAL_CHECK_ERRNO(setsockopt, s, SOL_SOCKET, SO_REUSEADDR, &option, sizeof(option));
		LOCAL_CHECK_ERRNO(setsockopt, s, IPPROTO_TCP, TCP_QUICKACK, &option, sizeof(option));
		option = H2O_DEFAULT_HANDSHAKE_TIMEOUT_IN_SECS;
		LOCAL_CHECK_ERRNO(setsockopt, s, IPPROTO_TCP, TCP_DEFER_ACCEPT, &option, sizeof(option));
		option = DEFAULT_TCP_FASTOPEN_QUEUE_LEN;
		LOCAL_CHECK_ERRNO(setsockopt, s, IPPROTO_TCP, TCP_FASTOPEN, &option, sizeof(option));
		LOCAL_CHECK_ERRNO(bind, s, iter->ai_addr, iter->ai_addrlen);
		LOCAL_CHECK_ERRNO(listen, s, INT_MAX);
		ret = s;
		break;

#undef LOCAL_CHECK_ERRNO

error:
		close(s);
	}

	freeaddrinfo(res);
	return ret;
}

static size_t get_maximum_cache_line_size(void)
{
	const int name[] = {_SC_LEVEL1_DCACHE_LINESIZE,
	                    _SC_LEVEL2_CACHE_LINESIZE,
	                    _SC_LEVEL3_CACHE_LINESIZE,
	                    _SC_LEVEL4_CACHE_LINESIZE};
	size_t ret = 0;

	for (size_t i = 0; i < ARRAY_SIZE(name); i++) {
		errno = 0;

		const long rc = sysconf(name[i]);

		if (rc < 0) {
			if (errno)
				LIBRARY_ERROR("sysconf");
		}
		else if ((size_t) rc > ret)
			ret = rc;
	}

	if (!ret)
		ret = DEFAULT_CACHE_LINE_SIZE;

	return ret;
}

static int initialize_global_data(const config_t *config, global_data_t *global_data)
{
	sigset_t signals;

	memset(global_data, 0, sizeof(*global_data));
	global_data->config = config;
	global_data->memory_alignment = get_maximum_cache_line_size();
	assert(global_data->memory_alignment <= DEFAULT_CACHE_LINE_SIZE);
	CHECK_ERRNO(sigemptyset, &signals);
#ifdef NDEBUG
	CHECK_ERRNO(sigaddset, &signals, SIGINT);
#endif // NDEBUG
	CHECK_ERRNO(sigaddset, &signals, SIGTERM);
	CHECK_ERRNO_RETURN(global_data->signal_fd, signalfd, -1, &signals, SFD_NONBLOCK | SFD_CLOEXEC);
	global_data->fortunes_template = get_fortunes_template(config->template_path);
	h2o_config_init(&global_data->h2o_config);
	global_data->listener_sd = get_listener_socket(config);

	if (global_data->listener_sd == -1)
		goto error;

	if (config->cert && config->key)
		initialize_openssl(global_data);

	const h2o_iovec_t host = h2o_iovec_init(H2O_STRLIT("default"));
	h2o_hostconf_t * const hostconf = h2o_config_register_host(&global_data->h2o_config,
	                                                           host,
	                                                           config->port);
	h2o_access_log_filehandle_t *log_handle = NULL;

	if (config->log) {
		log_handle = h2o_access_log_open_handle(config->log, NULL);

		if (!log_handle)
			goto error;
	}

	register_request_handlers(hostconf, log_handle);

	// Must be registered after the rest of the request handlers.
	if (config->root) {
		h2o_pathconf_t * const pathconf = h2o_config_register_path(hostconf, "/", 0);
		h2o_file_register(pathconf, config->root, NULL, NULL, 0);

		if (log_handle)
			global_data->file_logger = h2o_access_log_register(pathconf, log_handle);
	}

	global_data->ctx = initialize_thread_contexts(global_data);

	if (global_data->ctx)
		return EXIT_SUCCESS;

error:
	free_global_data(global_data);
	return EXIT_FAILURE;
}

static int parse_options(int argc, char *argv[], config_t *config)
{
	memset(config, 0, sizeof(*config));
	opterr = 0;

	while (1) {
		const int opt = getopt(argc, argv, "?a:b:c:d:f:k:l:m:p:q:r:t:");

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
			return EXIT_FAILURE; \
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
			case 't':
				PARSE_NUMBER(config->thread_num);
				break;
			default:
				fprintf(stderr, USAGE_MESSAGE, *argv);
				return EXIT_FAILURE;

#undef PARSE_NUMBER
		}
	}

	set_default_options(config);
	return EXIT_SUCCESS;
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

int main(int argc, char *argv[])
{
	config_t config;
	int rc = EXIT_FAILURE;

	if (parse_options(argc, argv, &config) == EXIT_SUCCESS) {
		global_data_t global_data;

		if (initialize_global_data(&config, &global_data) == EXIT_SUCCESS) {
			setup_process();
			start_threads(global_data.ctx);
			connect_to_database(global_data.ctx);
			event_loop(global_data.ctx);
			free_global_data(&global_data);
			rc = EXIT_SUCCESS;
		}
	}

	return rc;
}
