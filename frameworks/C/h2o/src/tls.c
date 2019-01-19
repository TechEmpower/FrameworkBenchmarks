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
#include <h2o.h>
#include <pthread.h>
#include <stddef.h>
#include <stdlib.h>
#include <openssl/conf.h>
#include <openssl/crypto.h>
#include <openssl/err.h>
#include <openssl/opensslv.h>
#include <openssl/ssl.h>

#include "error.h"
#include "tls.h"
#include "utility.h"

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

struct CRYPTO_dynlock_value {
	pthread_mutex_t mutex;
};

static struct CRYPTO_dynlock_value *dyn_create_function(const char *file, int line);
static void dyn_destroy_function(struct CRYPTO_dynlock_value *l, const char *file, int line);
static void dyn_lock_function(int mode, struct CRYPTO_dynlock_value *l, const char *file, int line);
static void locking_function(int mode, int n, const char *file, int line);

static struct {
	pthread_mutex_t *lock;
	size_t num_lock;
	pthread_mutexattr_t lock_attr;
} openssl_global_data;

static struct CRYPTO_dynlock_value *dyn_create_function(const char *file, int line)
{
	struct CRYPTO_dynlock_value *ret = malloc(sizeof(*ret));

	if (ret) {
		const int error_code = pthread_mutex_init(&ret->mutex, &openssl_global_data.lock_attr);

		if (error_code) {
			print_library_error(file, line, "pthread_mutex_init", error_code);
			free(ret);
			ret = NULL;
		}
	}

	return ret;
}

static void dyn_destroy_function(struct CRYPTO_dynlock_value *l, const char *file, int line)
{
	const int error_code = pthread_mutex_destroy(&l->mutex);

	if (error_code)
		print_library_error(file, line, "pthread_mutex_destroy", error_code);

	free(l);
}

static void dyn_lock_function(int mode, struct CRYPTO_dynlock_value *l, const char *file, int line)
{
	const char *function;
	int error_code;

	if (mode & CRYPTO_LOCK) {
		function = "pthread_mutex_lock";
		error_code = pthread_mutex_lock(&l->mutex);
	}
	else {
		function = "pthread_mutex_unlock";
		error_code = pthread_mutex_unlock(&l->mutex);
	}

	if (error_code) {
		print_library_error(file, line, function, error_code);
		abort();
	}
}

static void locking_function(int mode, int n, const char *file, int line)
{
	assert((size_t) n < openssl_global_data.num_lock);
	static_assert(!offsetof(struct CRYPTO_dynlock_value, mutex),
	              "The mutex must be the first field in struct CRYPTO_dynlock_value.");

	dyn_lock_function(mode,
	                  (struct CRYPTO_dynlock_value *) (openssl_global_data.lock + n),
	                  file,
	                  line);
}

void cleanup_openssl(global_data_t *global_data)
{
	SSL_CTX_free(global_data->ssl_ctx);
	CRYPTO_set_locking_callback(NULL);
	CRYPTO_set_id_callback(NULL);
	CRYPTO_set_dynlock_create_callback(NULL);
	CRYPTO_set_dynlock_destroy_callback(NULL);
	CRYPTO_set_dynlock_lock_callback(NULL);
	ERR_remove_state(0);
	ERR_free_strings();
	CONF_modules_unload(1);
	EVP_cleanup();
	CRYPTO_cleanup_all_ex_data();

	for (size_t i = 0; i < openssl_global_data.num_lock; i++)
		CHECK_ERROR(pthread_mutex_destroy, openssl_global_data.lock + i);

	free(openssl_global_data.lock);
	CHECK_ERROR(pthread_mutexattr_destroy, &openssl_global_data.lock_attr);
}

void initialize_openssl(const config_t *config, global_data_t *global_data)
{
	SSL_library_init();
	SSL_load_error_strings();
	openssl_global_data.num_lock = CRYPTO_num_locks();
	openssl_global_data.lock = calloc(openssl_global_data.num_lock,
	                                  sizeof(*openssl_global_data.lock));
	CHECK_ERROR(pthread_mutexattr_init, &openssl_global_data.lock_attr);
	CHECK_ERROR(pthread_mutexattr_settype,
	            &openssl_global_data.lock_attr,
	            PTHREAD_MUTEX_ADAPTIVE_NP);

	for (size_t i = 0; i < openssl_global_data.num_lock; i++)
		CHECK_ERROR(pthread_mutex_init,
		            openssl_global_data.lock + i,
		            &openssl_global_data.lock_attr);

	CRYPTO_set_locking_callback(locking_function);
	CRYPTO_set_dynlock_create_callback(dyn_create_function);
	CRYPTO_set_dynlock_destroy_callback(dyn_destroy_function);
	CRYPTO_set_dynlock_lock_callback(dyn_lock_function);
	global_data->ssl_ctx = SSL_CTX_new(TLSv1_2_server_method());
#if OPENSSL_VERSION_NUMBER >= 0x1000200fL
	SSL_CTX_set_ecdh_auto(global_data->ssl_ctx, 1);
	h2o_ssl_register_alpn_protocols(global_data->ssl_ctx, h2o_http2_alpn_protocols);
#endif
	SSL_CTX_set_cipher_list(global_data->ssl_ctx, "DEFAULT:!3DES:!RC4");
	CHECK_OPENSSL_ERROR(SSL_CTX_use_certificate_file,
	                    global_data->ssl_ctx,
	                    config->cert,
	                    SSL_FILETYPE_PEM);
	CHECK_OPENSSL_ERROR(SSL_CTX_use_PrivateKey_file,
	                    global_data->ssl_ctx,
	                    config->key,
	                    SSL_FILETYPE_PEM);
}
