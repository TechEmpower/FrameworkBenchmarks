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
#include <string.h>

#include "plaintext.h"
#include "request_handler.h"
#include "utility.h"

static int plaintext(struct st_h2o_handler_t *self, h2o_req_t *req)
{
	IGNORE_FUNCTION_PARAMETER(self);

	h2o_generator_t generator;
	h2o_iovec_t body = {.base = HELLO_RESPONSE, .len = sizeof(HELLO_RESPONSE) - 1};

	memset(&generator, 0, sizeof(generator));
	set_default_response_param(PLAIN, sizeof(HELLO_RESPONSE) - 1, req);
	h2o_start_response(req, &generator);
	h2o_send(req, &body, 1, H2O_SEND_STATE_FINAL);
	return 0;
}

void initialize_plaintext_handler(h2o_hostconf_t *hostconf, h2o_access_log_filehandle_t *log_handle)
{
	register_request_handler("/plaintext", plaintext, hostconf, log_handle);
}
