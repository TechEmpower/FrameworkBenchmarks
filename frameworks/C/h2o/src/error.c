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

#include <libgen.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/syscall.h>

#include "error.h"

void print_error(const char *file,
                 unsigned line,
                 const char *function,
                 const char *error_string,
                 ...)
{
	char * const file_name = strdup(file);
	const long tid = syscall(SYS_gettid);
	struct timespec t = {.tv_sec = 0};

	if (file_name)
		file = basename(file_name);

	clock_gettime(CLOCK_REALTIME, &t);
	flockfile(stderr);
	fprintf(stderr,
	        "%010lld.%09ld [%ld] %s: %u: %s(): ",
	        (long long) t.tv_sec,
	        t.tv_nsec,
	        tid,
	        file,
	        line,
	        function);

	va_list arg;

	va_start(arg, error_string);
	vfprintf(stderr, error_string, arg);
	va_end(arg);
	putc_unlocked('\n', stderr);
	funlockfile(stderr);

	if (file_name)
		free(file_name);
}

void print_library_error(const char *file,
                         unsigned line,
                         const char *function,
                         int error_code)
{
	char error_string[128];

	if (strerror_r(error_code, error_string, sizeof(error_string)))
		*error_string = '\0';

	print_error(file, line, function, "%s (%d)", error_string, error_code);
}
