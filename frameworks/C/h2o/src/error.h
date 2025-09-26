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

#ifndef ERROR_H_

#define ERROR_H_

#include <errno.h>
#include <stdlib.h>
#include <yajl/yajl_gen.h>

#define CHECK_ERRNO(function, ...) \
	do { \
		const int error_code = (function)(__VA_ARGS__); \
		\
		if (error_code) { \
			print_library_error(__FILE__, __LINE__, #function, errno); \
			abort(); \
		} \
	} while(0)

#define CHECK_ERRNO_RETURN(out, function, ...) \
	do { \
		const int return_value = (function)(__VA_ARGS__); \
		\
		if (return_value == -1) { \
			print_library_error(__FILE__, __LINE__, #function, errno); \
			abort(); \
		} \
		\
		(out) = return_value; \
	} while(0)

#define CHECK_ERROR(function, ...) \
	do { \
		const int error_code = (function)(__VA_ARGS__); \
		\
		if (error_code) { \
			print_library_error(__FILE__, __LINE__, #function, error_code); \
			abort(); \
		} \
	} while(0)

#define CHECK_YAJL_STATUS(function, ...) \
	do { \
		const yajl_gen_status status_code = (function)(__VA_ARGS__); \
		\
		if (status_code != yajl_gen_status_ok) { \
			print_error(__FILE__, __LINE__, #function, "Error (%d)", (int) status_code); \
			goto error_yajl; \
		} \
	} while(0)

#define ERROR(...) print_error(__FILE__, __LINE__, __func__, __VA_ARGS__)
#define LIBRARY_ERROR(function, ...) print_error(__FILE__, __LINE__, (function), __VA_ARGS__)
#define STANDARD_ERROR(function) print_library_error(__FILE__, __LINE__, (function), errno)

void print_error(const char *file,
                 unsigned line,
                 const char *function,
                 const char *error_string,
                 ...);
void print_library_error(const char *file,
                         unsigned line,
                         const char *function,
                         int error_code);

#endif // ERROR_H_
