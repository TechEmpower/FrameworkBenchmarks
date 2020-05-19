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

#ifndef REQUEST_HANDLER_H_

#define REQUEST_HANDLER_H_

#include <h2o.h>
#include <stdbool.h>

#include "global_data.h"
#include "utility.h"

#define REQ_ERROR "request error\n"

typedef enum {
	HTML,
	JSON,
	PLAIN
} content_type_t;

typedef enum {
	OK = 200,
	INTERNAL_SERVER_ERROR = 500,
	BAD_GATEWAY = 502,
	SERVICE_UNAVAILABLE = 503,
	GATEWAY_TIMEOUT = 504
} http_status_code_t;

void cleanup_request_handlers(global_data_t *global_data);
void free_request_handler_thread_data(request_handler_thread_data_t *request_handler_thread_data);
const char *get_query_param(const char *query,
                            size_t query_len,
                            const char *param,
                            size_t param_len);
void initialize_request_handler_thread_data(
		const config_t *config, request_handler_thread_data_t *request_handler_thread_data);
void initialize_request_handlers(const config_t *config,
                                 global_data_t *global_data,
                                 h2o_hostconf_t *hostconf,
                                 h2o_access_log_filehandle_t *log_handle);
void register_request_handler(const char *path,
                              int (*handler)(struct st_h2o_handler_t *, h2o_req_t *),
                              h2o_hostconf_t *hostconf,
                              h2o_access_log_filehandle_t *log_handle);
void send_error(http_status_code_t status_code, const char *body, h2o_req_t *req);
int send_json_response(json_generator_t *gen, bool free_gen, h2o_req_t *req);
void send_service_unavailable_error(const char *body, h2o_req_t *req);
void set_default_response_param(content_type_t content_type,
                                size_t content_length,
                                h2o_req_t *req);

#endif // REQUEST_HANDLER_H_
