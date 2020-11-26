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

#include <h2o.h>
#include <stdlib.h>
#include <openssl/err.h>
#include <openssl/ssl.h>

#include "error.h"
#include "global_data.h"
#include "tls.h"

#define CHECK_OPENSSL_ERROR(function, ...) \
	do { \
		const int error_code = (function)(__VA_ARGS__); \
		\
		if (error_code != 1) { \
			const unsigned long openssl_error = ERR_get_error(); \
			char buf[128] = ""; \
			\
			ERR_error_string_n(openssl_error, buf, sizeof(buf)); \
			print_error(__FILE__, __LINE__, #function, "%s (%lu)", buf, openssl_error); \
			abort(); \
		} \
	} while(0)

void cleanup_openssl(global_data_t *global_data)
{
	SSL_CTX_free(global_data->ssl_ctx);
}

void initialize_openssl(const config_t *config, global_data_t *global_data)
{
	global_data->ssl_ctx = SSL_CTX_new(TLS_server_method());
	SSL_CTX_set_min_proto_version(global_data->ssl_ctx, TLS1_2_VERSION);
	SSL_CTX_set_cipher_list(global_data->ssl_ctx, "ECDHE");
	h2o_ssl_register_alpn_protocols(global_data->ssl_ctx, h2o_http2_alpn_protocols);
	CHECK_OPENSSL_ERROR(SSL_CTX_use_certificate_file,
	                    global_data->ssl_ctx,
	                    config->cert,
	                    SSL_FILETYPE_PEM);
	CHECK_OPENSSL_ERROR(SSL_CTX_use_PrivateKey_file,
	                    global_data->ssl_ctx,
	                    config->key,
	                    SSL_FILETYPE_PEM);
}
