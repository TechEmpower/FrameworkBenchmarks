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

#include <h2o.h>
#include <stdbool.h>
#include <string.h>
#include <yajl/yajl_gen.h>

#include "error.h"
#include "json_serializer.h"
#include "plaintext.h"
#include "request_handler.h"
#include "thread.h"
#include "utility.h"

static int json_serializer(struct st_h2o_handler_t *self, h2o_req_t *req)
{
	IGNORE_FUNCTION_PARAMETER(self);

	thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
	                                                      event_loop.h2o_ctx,
	                                                      req->conn->ctx);
	json_generator_t * const gen = get_json_generator(&ctx->json_generator,
	                                                  &ctx->json_generator_num);
	// volatile is used to ensure that the object is instantiated every time
	// the function is called.
	const volatile struct {
		const char *message;
	} object = {HELLO_RESPONSE};

	if (gen) {
		CHECK_YAJL_STATUS(yajl_gen_map_open, gen->gen);
		CHECK_YAJL_STATUS(yajl_gen_string, gen->gen, YAJL_STRLIT("message"));
		CHECK_YAJL_STATUS(yajl_gen_string,
		                  gen->gen,
		                  (const unsigned char *) object.message,
		                  strlen(object.message));
		CHECK_YAJL_STATUS(yajl_gen_map_close, gen->gen);

		// The response is small enough, so that it is simpler to copy it
		// instead of doing a delayed deallocation of the JSON generator.
		if (!send_json_response(gen, true, req))
			return 0;

error_yajl:
		// If there is a problem with the generator, don't reuse it.
		free_json_generator(gen, NULL, NULL, 0);
	}

	send_error(INTERNAL_SERVER_ERROR, REQ_ERROR, req);
	return 0;
}

void initialize_json_serializer_handler(h2o_hostconf_t *hostconf,
                                        h2o_access_log_filehandle_t *log_handle)
{
	register_request_handler("/json", json_serializer, hostconf, log_handle);
}
